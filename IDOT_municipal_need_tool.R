###############################################################################
# IDOT MUNICIPAL NEED SCORING TOOL
# --------------------------------
# Last updated: 2021-02-12

# Created for IDOT by your friends at CMAP

####### UUUUUUUU       __
####### :UUUUUUU      /  |_ . _ _  _  _   |\/| _|_ _ _  _  _ |.|_ _  _
#######. :UUUUUU      \__| )|(_(_|(_)(_)  |  |(-|_| (_)|_)(_)|||_(_|| )
########  :UUUUU                  _/                   |
#########:  :UUU                         _        __
##########:    '       /\  _  _ _  _    (_ _  _  |__)| _  _  _ . _  _
#############:.       /--\(_)(-| )(_\/  | (_)|   |   |(_|| )| )|| )(_)
################          _/        /                              _/

###############################################################################

#install.packages("tidyverse", "readxl", "ggplot2", "sf", "tmap", "tmaptools", "devtools", "GGally")
#devtools::install_github("CMAP-REPOS/cmapplot", build_vignettes = TRUE)
library(tidyverse)
library(readxl)
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)
library(cmapplot)
apply_cmap_default_aes()

ANALYSIS_YEAR <- 2021  # Update this each year!
IN_XLSX <- "input/municipality_need_inputs.xlsx"  # Spreadsheet containing latest data


# Load input factors, weights and cohort thresholds -----------------------

FACTORS_MUNI <- read_xlsx(IN_XLSX, sheet="FACTORS_MUNI")
WEIGHTS <- read_xlsx(IN_XLSX, sheet="WEIGHTS")


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


# Calculate factor-specific scores ----------------------------------------

keep_cols_muni <- append(c("GEOID", "MUNI", "POP"), WEIGHTS$FACTOR_NAME)
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

  # Inspect factor distribution
  print(
    ggplot(FACTORS_MUNI) +
      geom_histogram(aes(x=get(factor)), color="#222222", fill="#73c9e3", size=0.3, bins=50) +
      geom_vline(xintercept=cuts[[2]], color="#222222", linetype="dotted") +
      geom_vline(xintercept=cuts[[3]], color="#222222", linetype="dotdash") +
      geom_vline(xintercept=cuts[[4]], color="#222222", linetype="dashed") +
      geom_vline(xintercept=cuts[[5]], color="#222222", linetype="longdash") +
      geom_vline(xintercept=cuts[[6]], color="#222222", linetype="solid", size=1) +  # Median
      geom_vline(xintercept=cuts[[7]], color="#222222", linetype="longdash") +
      geom_vline(xintercept=cuts[[8]], color="#222222", linetype="dashed") +
      geom_vline(xintercept=cuts[[9]], color="#222222", linetype="dotdash") +
      geom_vline(xintercept=cuts[[10]], color="#222222", linetype="dotted") +
      labs(title=paste("Distribution of factor values (with group breaks)", factor, sep="\n")) +
      theme_cmap(hline=0, ylab="Number of municipalities")
  )

  # Inspect score distribution
  print(
    ggplot(FACTORS_MUNI) +
      geom_histogram(aes(x=get(score_col)), color="#222222", fill="#73c9e3", size=0.3, binwidth=1) +
      geom_hline(yintercept=129.8, color="#222222", size=0.5, linetype="dashed") +
      scale_x_continuous(limits=c(min(groups)-0.5, max(groups)+0.5), breaks=groups) +
      labs(title = paste("Distribution of municipality need scores", score_col, sep="\n"),
           caption="Note: Dashed line represents a perfect decile distribution of 129.8 municipalities per group.") +
      theme_cmap(hline=0, ylab="Number of municipalities")
  )
}

# Plot distributions of and correlations between factors
FACTORS_MUNI[, WEIGHTS$FACTOR_NAME] %>%
  GGally::ggpairs()


# Calculate overall score (0-100) -----------------------------------------

FACTORS_MUNI$WEIGHTED_SCORE <- rowSums(FACTORS_MUNI[, wt_score_cols])

# Rescale from 0-100
min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(MUNI_NEED_SCORE = ((WEIGHTED_SCORE - min_wt_score) / (max_wt_score - min_wt_score) * 100))

bin_width = 100 / (max_wt_score - min_wt_score)
bin_center = bin_width / 2

# Inspect score distribution
ggplot(FACTORS_MUNI) +
  geom_histogram(aes(x=MUNI_NEED_SCORE), color="#222222", fill="#73c9e3", size=0.3,
                 binwidth=bin_width, center=bin_center) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title="Distribution of municipality need scores") +
  theme_cmap(hline=0, ylab="Number of municipalities")


# Assign scores to cohorts (1-5) ------------------------------------------

# Calculate approximate quintile thresholds (note: exact quintiles will
# probably not be obtained, given that only a fixed number of overall scores
# are possible, with groups of municipalities sharing identical scores)
SCORE_MED <- median(FACTORS_MUNI$MUNI_NEED_SCORE, na.rm=TRUE)
SCORE_SD <- sd(FACTORS_MUNI$MUNI_NEED_SCORE, na.rm=TRUE)
COHORTS <- append(
  c(-Inf, Inf),
  quantile(FACTORS_MUNI$MUNI_NEED_SCORE, probs=seq(0.2, 0.8, 0.2), na.rm=TRUE),
  after=1
)

FACTORS_MUNI$COHORT <- cut(as.matrix(FACTORS_MUNI$MUNI_NEED_SCORE), COHORTS, 1:5, labels=FALSE)

ggplot(FACTORS_MUNI) +
  geom_histogram(aes(x=MUNI_NEED_SCORE), color="#222222", fill="#73c9e3", size=0.3,
                 binwidth=bin_width, center=bin_center) +
  geom_vline(xintercept=COHORTS[[2]], color="#222222", linetype="longdash", size=1) +
  geom_vline(xintercept=COHORTS[[3]], color="#222222", linetype="longdash", size=1) +
  geom_vline(xintercept=COHORTS[[4]], color="#222222", linetype="longdash", size=1) +
  geom_vline(xintercept=COHORTS[[5]], color="#222222", linetype="longdash", size=1) +
  geom_label(x=0+(COHORTS[[2]]-0)/2, y=25, label="1") +
  geom_label(x=COHORTS[[2]]+(COHORTS[[3]]-COHORTS[[2]])/2, y=25, label="2") +
  geom_label(x=COHORTS[[3]]+(COHORTS[[4]]-COHORTS[[3]])/2, y=25, label="3") +
  geom_label(x=COHORTS[[4]]+(COHORTS[[5]]-COHORTS[[4]])/2, y=25, label="4") +
  geom_label(x=COHORTS[[5]]+(100-COHORTS[[5]])/2, y=25, label="5") +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title="Distribution of municipality need scores (with cohort breaks)") +
  theme_cmap(hline=0, ylab="Number of municipalities")

ggplot(FACTORS_MUNI) +
  geom_histogram(aes(x=COHORT), color="#222222", fill="#73c9e3", size=0.3, binwidth=1) +
  geom_hline(yintercept=259.6, color="#222222", size=0.5, linetype="dashed") +
  labs(title="Distribution of municipality need cohorts",
       caption="Note: Dashed line represents a perfect quintile distribution of 259.6 municipalities per group.") +
  theme_cmap(hline=0, ylab="Number of municipalities")


# Write output CSV --------------------------------------------------------

get_factor_themes <- function(score_cols) {
  factor_cols <- str_replace(score_cols, "SCORE_", "")
  factor_themes <- c()
  for (i in 1:length(factor_cols)) {
    factor = factor_cols[i]
    factor_themes[i] <- WEIGHTS[WEIGHTS$FACTOR_NAME==factor, "FACTOR_THEME"][[1]]
  }
  return(factor_themes)
}

OUT_DATA_MUNI <- FACTORS_MUNI %>%
  select(GEOID, MUNI, POP, COHORT, starts_with("SCORE_")) %>%
  rename(
    `Municipality` = MUNI,
    `Population` = POP,
    `Need Cohort` = COHORT
  ) %>%
  rename_with(.fn = get_factor_themes,
              .cols = starts_with("SCORE_"))

write_csv(OUT_DATA_MUNI, paste0("output/municipality_need_cohorts_", ANALYSIS_YEAR, ".csv"))




# Map the results ---------------------------------------------------------

IL_E_NAD83 <- 26971  # EPSG projection ID: IL State Plane East (NAD83)
tmap_mode("view")  # "plot" (static) or "view" (interactive)

il_geo <- st_read("input/illinois.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83)

muni_geo <- st_read("input/municipalities.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  mutate(GEOID_n = as.numeric(GEOID)) %>%
  left_join(OUT_DATA_MUNI, by=c("GEOID_n"="GEOID")) %>%
  mutate(COHORT_n = as.double(`Need Cohort`))

# Map each individual factor's scores
for (i in 1:length(score_cols)) {
  factor <- get_factor_themes(score_cols)[i]
  score_labels <- append(c("1 - lowest need", "10 - highest need"), 2:9, after=1)
  print(
    tm_shape(muni_geo) +
      tm_polygons(col=factor, id="NAME", textNA="No data",
                  palette="-magma", border.col="#cccccc", lwd=0.5, style="equal",
                  n=10, labels=score_labels, legend.reverse=TRUE) +
      tm_shape(il_geo) +
        tm_borders(col="#888888", lwd=2)
  )
}

# Map the assigned cohorts
cohort_labels <- score_labels <- append(c("1 - lowest need", "5 - highest need"), 2:4, after=1)
tm_shape(muni_geo) +
  tm_polygons(col="COHORT_n", title="Municipality Need Cohort", id="NAME", textNA="Insufficient data",
              palette="-magma", border.col="#cccccc", lwd=0.5, style="equal",
              n=5, labels=cohort_labels, legend.reverse=TRUE,
              popup.vars=append(c("Need Cohort", "Population"), get_factor_themes(score_cols))) +
  tm_shape(il_geo) +
    tm_borders(col="#888888", lwd=2)
