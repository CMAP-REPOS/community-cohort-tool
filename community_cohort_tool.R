#install.packages("tidyverse", "readxl", "ggplot2", "sf", "tmap", "tmaptools")
library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)

COHORT_YEAR <- 2020  # Update this each year!
IN_XLSX <- "input/community_cohort_inputs.xlsx"  # Spreadsheet containing latest data
COOK_MUNIS_CSV <- "input/cook_munis.csv"  # CSV of Cook County munis, with GEOID


# Load input factors, weights and cohort thresholds -----------------------

FACTORS_MUNI <- read_xlsx(IN_XLSX, sheet="FACTORS_MUNI")
FACTORS_CCA <- read_xlsx(IN_XLSX, sheet="FACTORS_CCA")
WEIGHTS <- read_xlsx(IN_XLSX, sheet="WEIGHTS")
COHORTS <- read_xlsx(IN_XLSX, sheet="COHORTS")
COHORTS$COHORT <- as.character(COHORTS$COHORT)


# Create boolean indicator for Cook munis ---------------------------------

COOK_MUNIS <- read_csv(COOK_MUNIS_CSV) %>%
  mutate(IN_COOK = TRUE) %>%
  select(GEOID, IN_COOK)
FACTORS_MUNI <- FACTORS_MUNI %>%
  left_join(COOK_MUNIS, by="GEOID") %>%
  mutate(IN_COOK = if_else(is.na(IN_COOK), FALSE, TRUE)) %>%
  filter(IN_COOK == TRUE & MUNI != "Chicago")  # Drop all but suburban Cook munis



# Calculate factor-specific scoring thresholds ----------------------------

WEIGHTS$MED <- unlist(summarize_all(FACTORS_MUNI[, WEIGHTS$FACTOR_NAME], median, na.rm=TRUE)[1,])
WEIGHTS$SD <- unlist(summarize_all(FACTORS_MUNI[, WEIGHTS$FACTOR_NAME], sd, na.rm=TRUE)[1,])

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
WEIGHTS[WEIGHTS$FACTOR_NAME=="PCT_EDA_POP", paste0("CUT", 1:9)] <- as.list(seq(0.1, 0.9, 0.1))


# Calculate factor-specific scores ----------------------------------------

keep_cols_muni <- append(c("GEOID", "MUNI", "IN_COOK"), WEIGHTS$FACTOR_NAME)
FACTORS_MUNI <- FACTORS_MUNI[, keep_cols_muni]

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
  FACTORS_MUNI[, score_col] <- cut(as.matrix(FACTORS_MUNI[, factor]), cuts, groups, labels=FALSE)
  if (weight < 0) {
    # Reverse score order for factors with negative weights
    FACTORS_MUNI[, score_col] <-  max(groups) + 1 - FACTORS_MUNI[, score_col]
  }
  FACTORS_MUNI[, wt_score_col] <- FACTORS_MUNI[, score_col] * abs(weight)

  # Inspect score distribution
  print(
    ggplot(FACTORS_MUNI) +
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
    ggplot(FACTORS_MUNI) +
      geom_histogram(aes(x=get(score_col)), color="white", fill="skyblue", binwidth=1) +
      geom_hline(yintercept=13, color="maroon", linetype="dashed") +
      scale_x_continuous(limits=c(min(groups)-1, max(groups)+1), breaks=groups) +
      labs(title="Distribution of factor scores", subtitle=score_col,
           caption="Dashed line represents a perfect decile distribution of 13 municipalities per group",
           x=score_col, y="Number of municipalities") +
      theme_classic()
  )
}


# Calculate overall score & cohorts ---------------------------------------

FACTORS_MUNI$SCORE_OVERALL <- rowSums(FACTORS_MUNI[, wt_score_cols])

# Rescale from 0-100
min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(SCORE_OVERALL_SCALED = (SCORE_OVERALL - min_wt_score) / (max_wt_score - min_wt_score) * 100)
FACTORS_MUNI$COHORT <- cut(as.matrix(FACTORS_MUNI$SCORE_OVERALL_SCALED), c(-Inf, COHORTS$MAX_SCORE), COHORTS$COHORT)
FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(COHORT = fct_relevel(COHORT, sort))

bin_width = 100 / (max_wt_score - min_wt_score)
bin_center = bin_width / 2

ggplot(FACTORS_MUNI) +
  geom_histogram(aes(x=SCORE_OVERALL_SCALED, fill=COHORT), color="white", binwidth=bin_width, center=bin_center) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  scale_fill_discrete(name="Assigned cohort") +
  labs(title="Distribution of overall scores (municipalities)", x="Overall score", y="Number of municipalities") +
  theme_classic()


# Map the results ---------------------------------------------------------

IL_E_NAD83 <- 26971  # EPSG projection ID: IL State Plane East (NAD83)
tmap_mode("plot")  # "plot" (static) or "view" (interactive)

cnty_geo <- st_read("input/cmap_county_boundaries.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83)

muni_geo <- st_read("input/cmap_munis.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  left_join(FACTORS_MUNI, by=c("GEOID_n"="GEOID"))

muni_labels <- muni_geo %>%
  filter(MUNI.x %in% c("Chicago", "Joliet", "Aurora", "Elgin", "Waukegan"))  # Label select munis

mutate(muni_geo, COHORT = as.integer(COHORT)) %>%
tm_shape(bbox=bb(filter(muni_geo, IN_COOK==TRUE), ext=1.2)) +
  tm_polygons("COHORT", title="", palette="Reds", n=4, border.col="#ffffff",
              labels=c("1 (low need)", "2 (moderate need)", "3 (high need)", "4 (very high need)"),
              textNA="No data", colorNA="#dddddd") +
tm_shape(cnty_geo) +
  tm_lines(col="#888888", lwd=2) +
tm_shape(muni_labels) +
  tm_text("MUNI.x", size=0.7, col="#000000", fontface="bold") +
tm_legend(legend.position=c("left", "bottom")) +
tm_layout(title="Assigned cohorts (municipalities)", frame=FALSE)


# # Write output files ------------------------------------------------------
# OUT_DATA_MUNI <- FACTORS_MUNI %>%
#   rename(WEIGHTED_SCORE = SCORE_OVERALL_SCALED) %>%
#   select(GEOID, MUNI, COHORT, WEIGHTED_SCORE, starts_with("SCORE_")) %>%
#   select(-SCORE_OVERALL)
# write_csv(OUT_DATA_MUNI, "output/cohort_assignments_muni.csv")
