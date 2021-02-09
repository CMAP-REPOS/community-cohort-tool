#install.packages("tidyverse", "readxl", "ggplot2", "sf", "tmap", "tmaptools", "devtools")
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

WEIGHTS$MED <- unlist(summarize_all(FACTORS_MUNI[, WEIGHTS$FACTOR_NAME], median, na.rm = TRUE)[1,])
WEIGHTS$SD <- unlist(summarize_all(FACTORS_MUNI[, WEIGHTS$FACTOR_NAME], sd, na.rm = TRUE)[1,])

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
WEIGHTS[WEIGHTS$FACTOR_NAME=="PCT_EJ_POP", paste0("CUT", 1:9)] <- as.list(seq(0.1, 0.9, 0.1))


# Calculate factor-specific scores ----------------------------------------

keep_cols_muni <- append(c("GEOID", "MUNI"), WEIGHTS$FACTOR_NAME)
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


# Calculate overall score & cohorts ---------------------------------------

FACTORS_MUNI$WEIGHTED_SCORE <- rowSums(FACTORS_MUNI[, wt_score_cols])

# Rescale from 0-100, and invert relationship (0=low need, 100=high need)
min_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 1
max_wt_score <- sum(abs(WEIGHTS$WEIGHT)) * 10

FACTORS_MUNI <- FACTORS_MUNI %>%
  mutate(MUNI_NEED_SCORE = 100 - ((WEIGHTED_SCORE - min_wt_score) / (max_wt_score - min_wt_score) * 100))

bin_width = 100 / (max_wt_score - min_wt_score)
bin_center = bin_width / 2

ggplot(FACTORS_MUNI) +
  geom_histogram(aes(x=MUNI_NEED_SCORE), color="#222222", fill="#73c9e3", size=0.3,
                 binwidth=bin_width, center=bin_center) +
  scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 10)) +
  labs(title="Distribution of municipality need scores") +
  theme_cmap(hline=0, ylab="Number of municipalities")


# Map the results ---------------------------------------------------------

IL_E_NAD83 <- 26971  # EPSG projection ID: IL State Plane East (NAD83)
tmap_mode("view")  # "plot" (static) or "view" (interactive)

il_geo <- st_read("input/illinois.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83)

muni_geo <- st_read("input/municipalities.geojson", quiet=TRUE) %>%
  st_transform(IL_E_NAD83) %>%
  mutate(GEOID_n = as.numeric(GEOID)) %>%
  left_join(FACTORS_MUNI, by=c("GEOID_n"="GEOID"))

tm_shape(muni_geo, bbox=bb(il_geo, ext=1.2)) +
  tm_polygons(col="MUNI_NEED_SCORE", title="Municipality Need Score", id="NAME",
              palette="-magma", n=10, border.col="#cccccc", lwd=0.5,
              popup.vars=append("MUNI_NEED_SCORE", score_cols), legend.reverse=TRUE) +
tm_shape(il_geo) +
  tm_borders(col="#888888", lwd=2)


# Write output files ------------------------------------------------------

OUT_DATA_MUNI <- FACTORS_MUNI %>%
  select(GEOID, MUNI, MUNI_NEED_SCORE, starts_with("SCORE_"))

write_csv(OUT_DATA_MUNI, paste0(getwd(), "/output/municipality_need_scores_", ANALYSIS_YEAR, ".csv"))

# Export shapefile
OUT_SHP <- paste0(getwd(), "/output/IDOT_muni_need_scores_DRAFT.shp")
muni_geo %>%
  select(append("MUNI_NEED_SCORE", score_cols)) %>%
  rename(MUNI_NEED = MUNI_NEED_SCORE,
         D_MED_INC = SCORE_ln_MED_HH_INC,
         D_EAV_PC = SCORE_ln_NF_TIF_EAV_PER_CAP,
         D_SVI_SES = SCORE_SVI_SES,
         D_SVI_MSL = SCORE_ln_SVI_MSL) %>%
  st_write(OUT_SHP)


# # Compare scores/cohorts against previous year ----------------------------
#
# prev_year <- ANALYSIS_YEAR - 1
# prev_muni_csv <- paste0("output/archive/municipality_need_scores_", prev_year, ".csv")
#
# PREV_SCORES_MUNI <- read_csv(prev_muni_csv, col_types=cols(COHORT=col_character())) %>%
#   rename(
#     SCORE_PREV = WEIGHTED_SCORE,
#     COHORT_PREV = COHORT
#   ) %>%
#   select(MUNI, SCORE_PREV, COHORT_PREV)
#
# COMPARE_MUNI <- FACTORS_MUNI %>%
#   left_join(PREV_SCORES_MUNI, by="MUNI") %>%
#   mutate(
#     COHORT_CHG = as.numeric(COHORT) - as.numeric(COHORT_PREV),
#     POP = exp(ln_POP)
#   )
#
# CHANGED_MUNIS <- COMPARE_MUNI[COMPARE_MUNI$COHORT != COMPARE_MUNI$COHORT_PREV, c("MUNI", "MUNI_NEED_SCORE", "COHORT", "COHORT_PREV", "POP")]
# CHANGED_MUNIS
#
# # Descriptive statistics
# library(modelr)
#
# cat("MUNICIPALITIES")
# lm_muni <- lm(MUNI_NEED_SCORE ~ SCORE_PREV, data=COMPARE_MUNI)
# cat("Correlation (R-squared):", cor(COMPARE_MUNI$SCORE_PREV, COMPARE_MUNI$MUNI_NEED_SCORE))
# cat("Linear trend: SCORE_new = ", lm_muni$coefficients[1], " + ", lm_muni$coefficients[2], "*SCORE_old", sep="")
# cat("Root-mean-square error (RMSE):", rmse(lm_muni, COMPARE_MUNI))
#
# # Plots
# ggplot(COMPARE_MUNI) +
#   geom_point(aes(x=SCORE_PREV, y=MUNI_NEED_SCORE, color=COHORT), alpha=0.6, size=3) +
#   geom_abline(intercept=0, slope=1, color="gray", linetype="dashed") +
#   geom_abline(intercept=lm_muni$coefficients[1], slope=lm_muni$coefficients[2]) +
#   scale_x_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
#   scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, 20)) +
#   labs(title="Updated vs. previous scores (municipalities)") +
#   theme_cmap(gridlines="hv", xlab="Previous score", ylab="Updated score",
#              legend.position="right", legend.direction="vertical",
#              legend.title=element_text()) +
#   guides(color=guide_legend(title="Updated cohort"))
#
# ggplot(COMPARE_MUNI) +
#   geom_count(aes(x=COHORT_PREV, y=COHORT, color=COHORT)) +
#   scale_size_area(max_size = 20) +
#   labs(title="Updated vs. previous cohorts (municipalities)") +
#   theme_cmap(gridlines="hv", xlab="Previous cohort", ylab="Updated cohort",
#              legend.position="right", legend.direction="vertical") +
#   guides(color=guide_none())
#
# ggplot(COMPARE_MUNI) +
#   geom_histogram(aes(x=COHORT_PREV, fill="Previous"), stat="count", width=0.4, position=position_nudge(x=-0.2)) +
#   geom_histogram(aes(x=COHORT, fill="Updated"), stat="count", width=0.4, position=position_nudge(x=0.2)) +
#   labs(title="Updated vs. previous cohorts (municipalities)") +
#   theme_cmap(xlab="Cohort", ylab="Number of municipalities")
#
# COHORT_POP <- COMPARE_MUNI %>%
#   select(COHORT, POP) %>%
#   group_by(COHORT) %>%
#   summarize(POP = sum(POP), .groups="drop")
#
# COHORT_POP_PREV <- COMPARE_MUNI %>%
#   select(COHORT_PREV, POP) %>%
#   group_by(COHORT_PREV) %>%
#   summarize(POP = sum(POP), .groups="drop")
#
# COHORT_POP_CHG <- COHORT_POP %>%
#   left_join(COHORT_POP_PREV, by=c("COHORT" = "COHORT_PREV"), suffix=c("", "_PREV"))
#
# ggplot(COHORT_POP_CHG) +
#   geom_col(aes(x=COHORT, y=POP_PREV, fill="Previous"), width=0.4, position=position_nudge(x=-0.2)) +
#   geom_col(aes(x=COHORT, y=POP, fill="Updated"), width=0.4, position=position_nudge(x=0.2)) +
#   labs(title="Population in updated vs. previous cohorts (municipalities)") +
#   scale_y_continuous(labels=scales::label_comma()) +
#   theme_cmap(xlab="Cohort", ylab="Total population")
#
# # Maps
# muni_geo <- st_read("input/cmap_munis.geojson", quiet=TRUE) %>%
#   st_transform(IL_E_NAD83) %>%
#   left_join(COMPARE_MUNI, by=c("GEOID_n"="GEOID"))
#
# tm_shape(muni_geo, bbox=bb(cnty_geo, ext=1.2)) +
#   tm_polygons("COHORT_CHG", title="", palette="-PuOr", contrast=c(0,1), n=7, border.col="#ffffff", lwd=0.5,
#               midpoint=NA, style="fixed", breaks=c(-3,-2,-1,0,1,2,3,4),
#               labels=c("-3 (lower need)", "-2", "-1", "+0 (no change)", "+1", "+2", "+3 (higher need)")) +
# tm_shape(cnty_geo) +
#   tm_lines(col="#888888", lwd=2) +
# # tm_shape(muni_labels) +
# #   tm_text("MUNI.x", size=0.7, col="#000000") +
# tm_legend(legend.position=c("left", "bottom")) +
# tm_layout(title="Change in municipality cohort (previous to updated)", frame=FALSE,
#           fontface=cmapplot_globals$font$strong$face,
#           fontfamily=cmapplot_globals$font$strong$family,
#           legend.text.fontface=cmapplot_globals$font$regular$face,
#           legend.text.fontfamily=cmapplot_globals$font$regular$family)
#
#
# # Tables for memo ---------------------------------------------------------
#
# MEMO_MUNI <- COMPARE_MUNI %>%
#   mutate(
#     POP = exp(ln_POP),
#     TAX_BASE_PER_CAP = exp(ln_TAX_BASE_PER_CAP),
#     MED_HH_INC = exp(ln_MED_HH_INC),
#     COHORT_NAME = paste("Cohort", COHORT),
#     PREV_COHORT_NAME = paste("Cohort", COHORT_PREV)
#   ) %>%
#   select(MUNI, COHORT_NAME, PREV_COHORT_NAME, COHORT_CHG, MUNI_NEED_SCORE,
#          MED_HH_INC, POP, TAX_BASE_PER_CAP, PCT_EJ_POP) %>%
#   rename(
#     `Community Name` = MUNI,
#     `Cohort` = COHORT_NAME,
#     `Previous Cohort` = PREV_COHORT_NAME,
#     `Change in Cohort` = COHORT_CHG,
#     `Overall Score` = MUNI_NEED_SCORE,
#     `Median Income` = MED_HH_INC,
#     `Population` = POP,
#     `Tax Base Per Capita` = TAX_BASE_PER_CAP,
#     `Population in EJ Areas` = PCT_EJ_POP
#   )
#
# for (cohort_name in c("Cohort 1", "Cohort 2", "Cohort 3", "Cohort 4")) {
#   MEMO_MUNI %>%
#     filter(`Cohort` == cohort_name) %>%
#     select(-`Cohort`, -`Previous Cohort`, -`Change in Cohort`, -`Overall Score`) %>%
#     write_csv(paste0("output/Memo - Municipalities - ", cohort_name, ".csv"))
# }
#
# MEMO_MUNI %>%
#   select(`Community Name`, `Cohort`, `Overall Score`) %>%
#   write_csv("output/Memo - Municipalities - All Cohorts - Scores.csv")
#
# MEMO_MUNI %>%
#   filter(`Change in Cohort` < 0) %>%
#   rename(`Updated Cohort` = `Cohort`) %>%
#   select(`Community Name`, `Previous Cohort`, `Updated Cohort`) %>%
#   write_csv("output/Memo - Municipalities - Trending Up.csv")
#
# MEMO_MUNI %>%
#   filter(`Change in Cohort` > 0) %>%
#   rename(`Updated Cohort` = `Cohort`) %>%
#   select(`Community Name`, `Previous Cohort`, `Updated Cohort`) %>%
#   write_csv("output/Memo - Municipalities - Trending Down.csv")
