#install.packages("tidyverse", "readxl", "ggplot2", "sf", "tmap", "tmaptools")
library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)

SCENARIO_NAME <- "base"
IN_XLSX <- "input/equity_distribution_inputs.xlsx"  # Spreadsheet containing latest data


# Load input factors and weights ------------------------------------------

FACTORS <- read_xlsx(IN_XLSX, sheet="FACTORS")
WEIGHTS <- read_xlsx(IN_XLSX, sheet="WEIGHTS")


# Restrict to suburban Cook munis -----------------------------------------

FACTORS <- FACTORS %>%
  filter(IN_COOK == 1 & MUNI != "Chicago")


# Calculate factor-specific scoring thresholds ----------------------------

WEIGHTS$MED <- unlist(summarize_all(FACTORS[, WEIGHTS$FACTOR_NAME], median, na.rm=TRUE)[1,])
WEIGHTS$SD <- unlist(summarize_all(FACTORS[, WEIGHTS$FACTOR_NAME], sd, na.rm=TRUE)[1,])

# Exclude 0s from logged COVID-19 death rate distribution
WEIGHTS[WEIGHTS$FACTOR_NAME=="ln_COVID_DEATH_RATE", "MED"] <- filter(FACTORS, ln_COVID_DEATH_RATE > 0) %>%
  .$ln_COVID_DEATH_RATE %>%
  median()
WEIGHTS[WEIGHTS$FACTOR_NAME=="ln_COVID_DEATH_RATE", "SD"] <- filter(FACTORS, ln_COVID_DEATH_RATE > 0) %>%
  .$ln_COVID_DEATH_RATE %>%
  sd()

WEIGHTS <- WEIGHTS %>%
  mutate(
    CUT0 = -Inf,
    CUT1 = MED - SD * 1.2816,  # ~10th %ile (based on standard normal distribution)
    CUT2 = MED - SD * 0.8416,  # ~20th %ile
    CUT3 = MED - SD * 0.5244,  # ~30th %ile
    CUT4 = MED - SD * 0.2533,  # ~40th %ile
    CUT5 = MED,                # ~50th %ile
    CUT6 = MED + SD * 0.2533,  # ~60th %ile
    CUT7 = MED + SD * 0.5244,  # ~70th %ile
    CUT8 = MED + SD * 0.8416,  # ~80th %ile
    CUT9 = MED + SD * 1.2816,  # ~90th %ile
    CUT10 = Inf
  )

# Force equal intervals and midpoint of 0.5 for PCT_EDA_POP factor
WEIGHTS[WEIGHTS$FACTOR_NAME=="PCT_EDA_POP", paste0("CUT", 1:9)] <- as.list(
  seq(from=0.1, to=0.9, by=0.1)
)

# Force score=1 to represent only 0-death munis for logged COVID-19 death rate
ln_covid_cut1 <- WEIGHTS[WEIGHTS$FACTOR_NAME=="ln_COVID_DEATH_RATE", "CUT1"]
if (ln_covid_cut1 > 0) {
  WEIGHTS[WEIGHTS$FACTOR_NAME=="ln_COVID_DEATH_RATE", "CUT1"] <- 0
}

# For raw COVID-19 death rate, force score=1 for munis with 0 deaths, equal
# intervals for scores 2-10
WEIGHTS[WEIGHTS$FACTOR_NAME=="COVID_DEATH_RATE", paste0("CUT", 1:9)] <- as.list(
  seq(from=0, by=max(FACTORS$COVID_DEATH_RATE)/9, length.out=9)
)


# Calculate factor-specific scores ----------------------------------------

keep_cols <- append(c("GEOID", "MUNI", "IN_COOK"), WEIGHTS$FACTOR_NAME)
FACTORS <- FACTORS[, keep_cols]

score_cols <- c()
wt_score_cols <- c()

for (factor in unlist(WEIGHTS[WEIGHTS$WEIGHT!=0, "FACTOR_NAME"])) {
  weight <- WEIGHTS[WEIGHTS$FACTOR_NAME==factor, "WEIGHT"][[1]]

  score_col <- paste0("SCORE_", factor)
  score_cols <- append(score_cols, score_col)

  wt_score_col <- paste0("WT_SCORE_", factor)
  wt_score_cols <- append(wt_score_cols, wt_score_col)

  cuts <- WEIGHTS[WEIGHTS$FACTOR_NAME==factor,] %>%
    select(starts_with("CUT"))
  groups <- c(1:10)
  FACTORS[, score_col] <- cut(as.matrix(FACTORS[, factor]), cuts, groups, labels=FALSE)
  if (weight < 0) {
    # Reverse score order for factors with negative weights
    FACTORS[, score_col] <-  max(groups) + 1 - FACTORS[, score_col]
  }
  FACTORS[, wt_score_col] <- FACTORS[, score_col] * abs(weight)

  # Inspect score distribution
  print(
    ggplot(FACTORS) +
      geom_histogram(aes(x=get(factor)), color="white", fill="skyblue", bins=50) +
      geom_vline(xintercept=cuts[[2]], color="maroon", linetype="dotted") +
      geom_vline(xintercept=cuts[[3]], color="maroon", linetype="dotdash") +
      geom_vline(xintercept=cuts[[4]], color="maroon", linetype="dashed") +
      geom_vline(xintercept=cuts[[5]], color="maroon", linetype="longdash") +
      geom_vline(xintercept=cuts[[6]], color="maroon", linetype="solid", size=1) +  # Median
      geom_vline(xintercept=cuts[[7]], color="maroon", linetype="longdash") +
      geom_vline(xintercept=cuts[[8]], color="maroon", linetype="dashed") +
      geom_vline(xintercept=cuts[[9]], color="maroon", linetype="dotdash") +
      geom_vline(xintercept=cuts[[10]], color="maroon", linetype="dotted") +
      labs(title="Distribution of factor values (with group breaks)",
           subtitle=factor,
           x=factor, y="Number of municipalities") +
      theme_classic()
  )

  print(
    ggplot(FACTORS) +
      geom_histogram(aes(x=get(score_col)), color="white", fill="skyblue", binwidth=1) +
      geom_hline(yintercept=13.4, color="maroon", linetype="dashed") +
      scale_x_continuous(limits=c(min(groups)-1, max(groups)+1), breaks=groups) +
      labs(title="Distribution of factor scores", subtitle=score_col,
           caption="Dashed line represents a perfect decile distribution of 13.4 municipalities per group",
           x=score_col, y="Number of municipalities") +
      theme_classic()
  )
}


# Calculate overall score -------------------------------------------------

FACTORS$SCORE_OVERALL <- rowSums(FACTORS[, wt_score_cols])

# Rescale from 0-100
min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS <- FACTORS %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)

bin_width = 2.222222 #100 / (max_wt_score - min_wt_score)
bin_center = 1.111111 #bin_width / 2

ggplot(FACTORS) +
  geom_histogram(aes(x=SCORE_OVERALL_SCALED), fill="skyblue", color="white",
                 lwd=0.25, binwidth=bin_width, center=bin_center) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title=paste("Distribution of overall scores:", SCENARIO_NAME, "scenario"),
       x="Overall score", y="Number of municipalities") +
  theme_classic()


# Map the results ---------------------------------------------------------

IL_E_NAD83 <- 26971  # EPSG projection ID: IL State Plane East (NAD83)
tmap_mode("plot")  # "plot" (static) or "view" (interactive)

cnty_geo <- st_read("input/cook_county_boundaries.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83)

muni_geo <- st_read("input/cmap_munis.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  left_join(FACTORS, by=c("GEOID_n"="GEOID")) %>%
  filter(IN_COOK == 1 | MUNI.x == "Chicago")

muni_labels <- muni_geo %>%
  filter(MUNI.x %in% c("Elgin", "Cicero", "Arlington Heights", "Evanston", "Orland Park"))  # Label select munis

tm_shape(muni_geo, bbox=bb(muni_geo, ext=1.1)) +
  tm_polygons("SCORE_OVERALL_SCALED", title="", palette="-Reds", n=10, border.col="#ffffff",
              textNA="Ineligible", colorNA="#dddddd") +
tm_shape(cnty_geo) +
  tm_lines(col="#666666", lwd=1) +
# tm_shape(muni_labels) +
#   tm_text("MUNI.x", size=0.7, col="#000000", fontface="bold") +
tm_legend(legend.position=c("left", "bottom")) +
  tm_layout(title=paste("Overall scores:", SCENARIO_NAME, "scenario"), frame=FALSE)


# Write output files ------------------------------------------------------
OUT_DATA <- FACTORS %>%
  select(GEOID, MUNI, starts_with("SCORE_"), -SCORE_OVERALL)
write_csv(OUT_DATA, paste0("output/assigned_scores_", SCENARIO_NAME, ".csv"))


# Compare scores against base scenario ------------------------------------

BASE_SCORES <- read_csv("output/assigned_scores_base.csv") %>%
  rename(
    SCORE_BASE = SCORE_OVERALL_SCALED
  )

COMPARE <- FACTORS %>%
  left_join(BASE_SCORES, by=c("GEOID", "MUNI")) %>%
  mutate(
    SCORE_CHG = as.numeric(SCORE_OVERALL_SCALED) - as.numeric(SCORE_BASE)
  )

CHANGED_MUNIS <- COMPARE[COMPARE$SCORE_OVERALL_SCALED != COMPARE$SCORE_BASE, c("MUNI", "SCORE_OVERALL_SCALED", "SCORE_BASE", "SCORE_CHG")]
CHANGED_MUNIS

# Plots
ggplot(COMPARE) +
  geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
  geom_count(aes(x=SCORE_BASE, y=SCORE_OVERALL_SCALED), color="maroon", alpha=0.5) +
  scale_size_area(max_size=5) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title=paste("Municipalities by overall score vs. baseline:", SCENARIO_NAME, "scenario"),
       x="Base scenario score",
       y=paste(SCENARIO_NAME, "scenario score")) +
  theme_classic()

# Maps
muni_geo <- st_read("input/cmap_munis.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  left_join(COMPARE, by=c("GEOID_n"="GEOID")) %>%
  filter(IN_COOK == 1 | MUNI.x == "Chicago")

tm_shape(muni_geo, bbox=bb(muni_geo, ext=1.1)) +
  tm_polygons("SCORE_CHG", title="", palette="-PuOr", contrast=c(0,1), n=4, border.col="#ffffff",
              midpoint=NA, style="fixed", breaks=c(-100,-20,-10,-5,5,10,20,100),
              labels=c("-20 or more (increased eligibility)", "-20 to -10", "-10 to -5", "-5 to +5",
                       "+5 to +10", "+10 to +20", "+20 or more (decreased eligibility)"),
              textNA="Ineligible", colorNA="#dddddd") +
  tm_shape(cnty_geo) +
    tm_lines(col="#666666", lwd=1) +
  # tm_shape(muni_labels) +
  #   tm_text("MUNI.x", size=0.7, col="#000000", fontface="bold") +
  tm_legend(legend.position=c("left", "bottom")) +
  tm_layout(title=paste0("Score change (", SCENARIO_NAME, " vs. Base scenario)"), frame=FALSE)
