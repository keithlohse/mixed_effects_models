# ------------------------------------------------------------------
# Simulate a fully within-subjects 2 (Load) x 3 (Terrain) x 2 (Fatigue)
# repeated-measures design, for Section 3 (Mixed-Effects Models for
# Factorial Designs), demonstrating the mixed-effects analogue of a
# three-way, fully within-subjects, repeated-measures ANOVA.
#
# Outcome: walking speed (m/s), one observation per subject per cell
# (no between-subjects factor -- e.g., no age_group/treatment group).
# ------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
})

set.seed(8675309)

n_subj   <- 32
load_lv  <- c("Light","Heavy")
terr_lv  <- c("Flat","Uneven","Stairs")
fatg_lv  <- c("Pre","Post")

design <- expand.grid(
  subID   = factor(sprintf("S%02d", 1:n_subj)),
  load    = factor(load_lv,  levels = load_lv),
  terrain = factor(terr_lv,  levels = terr_lv),
  fatigue = factor(fatg_lv,  levels = fatg_lv)
) %>% arrange(subID, load, terrain, fatigue)

subj_ids <- levels(design$subID)

# ---- subject-level random deviates for every stratum in a 3-way,
# fully-crossed, within-subjects design (subject; subject:load;
# subject:terrain; subject:fatigue; subject:load:terrain;
# subject:load:fatigue; subject:terrain:fatigue). The finest stratum,
# subject:load:terrain:fatigue, is confounded with the residual,
# because there is only one observation per subject/cell. ----
u_subj      <- rnorm(n_subj, 0, 0.09); names(u_subj) <- subj_ids
u_subj_load <- rnorm(n_subj, 0, 0.05); names(u_subj_load) <- subj_ids
u_subj_fatg <- rnorm(n_subj, 0, 0.05); names(u_subj_fatg) <- subj_ids

u_subj_terr <- matrix(rnorm(n_subj*3, 0, 0.05), nrow=n_subj, dimnames=list(subj_ids, terr_lv))
u_subj_terr <- u_subj_terr - rowMeans(u_subj_terr)

u_subj_loadterr <- matrix(rnorm(n_subj*3, 0, 0.05), nrow=n_subj, dimnames=list(subj_ids, terr_lv))
u_subj_loadterr <- u_subj_loadterr - rowMeans(u_subj_loadterr)

u_subj_loadfatg <- rnorm(n_subj, 0, 0.07); names(u_subj_loadfatg) <- subj_ids

u_subj_terrfatg <- matrix(rnorm(n_subj*3, 0, 0.05), nrow=n_subj, dimnames=list(subj_ids, terr_lv))
u_subj_terrfatg <- u_subj_terrfatg - rowMeans(u_subj_terrfatg)

# ---- Population (fixed) effects, in m/s ----
b0             <- 1.30
b_load_heavy   <- -0.12
b_terr         <- c(Flat=0, Uneven=-0.10, Stairs=-0.22)
b_fatg_post    <- -0.08
b_loadterr     <- c(Flat=0, Uneven=-0.03, Stairs=-0.10)   # Load x Terrain
b_loadfatg     <- -0.06                                   # Load x Fatigue
b_terrfatg     <- c(Flat=0, Uneven=-0.02, Stairs=-0.03)   # Terrain x Fatigue
# Load:Terrain:Fatigue fixed interaction = 0 (no true 3-way effect simulated)

design <- design %>%
  rowwise() %>%
  mutate(
    speed = b0 +
      u_subj[as.character(subID)] +
      (load=="Heavy")*b_load_heavy + (load=="Heavy")*u_subj_load[as.character(subID)] +
      b_terr[as.character(terrain)] + u_subj_terr[as.character(subID), as.character(terrain)] +
      (fatigue=="Post")*b_fatg_post + (fatigue=="Post")*u_subj_fatg[as.character(subID)] +
      (load=="Heavy")*b_loadterr[as.character(terrain)] +
      (load=="Heavy")*u_subj_loadterr[as.character(subID), as.character(terrain)] +
      (load=="Heavy")*(fatigue=="Post")*b_loadfatg +
      (load=="Heavy")*(fatigue=="Post")*u_subj_loadfatg[as.character(subID)] +
      b_terrfatg[as.character(terrain)]*(fatigue=="Post") +
      u_subj_terrfatg[as.character(subID), as.character(terrain)]*(fatigue=="Post") +
      rnorm(1, 0, 0.05)
  ) %>% ungroup() %>%
  mutate(speed = round(speed, 3)) %>%
  select(subID, load, terrain, fatigue, speed)

write.csv(design, "data_GAIT_3way_example.csv", row.names = FALSE)
cat("Wrote data_GAIT_3way_example.csv:", nrow(design), "rows,", n_subj, "subjects\n")
