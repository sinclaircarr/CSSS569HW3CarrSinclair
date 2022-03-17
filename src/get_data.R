# Script to compile exposure, relative risk, paf, and burden for each RO pair
# Must be ran on IHME cluster
rm(list = ls())
options(scipen = 999)

# System info
os <- Sys.info()[1]
user <- Sys.info()[7]

# define arguments
rei.id <- 102
id_cols <- c("location_id", "sex_id", "age_group_id", "year_id")

# Drives
j <- if (os == "Linux") "/home/j/" else if (os == "Windows") "J:/"
h <- if (os == "Linux") paste0("/homes/", user, "/") else if (os == "Windows") "H:/"

code_dir <- if (os == "Linux") paste0("/ihme/code/dbd/", user, "/") else if (os == "Windows") ""
work_dir <- paste0(h, "csss_569_final_proj/")
setwd(work_dir)
invisible(sapply(list.files("/share/cc_resources/libraries/current/r/", full.names = T), source))
library(dplyr)

# Load in and save datasets and vars
locs <- get_location_metadata(22, gbd_round_id = 7, decomp_step = 'iterative')
lvl3_locs <- unique(locs[level == 3, location_id])
saveRDS(locs, file = "data/location_metadata.RDS")
causes <- get_ids("cause")
saveRDS(causes, file = "data/cause_metadata.RDS")
reis <- get_ids("rei")
saveRDS(reis, file = "data/rei_metadata.RDS")
ages <- get_age_metadata(age_group_set_id = 19)
saveRDS(ages, file = "data/age_metadata.RDS")

age_vec <- c(9:20, 30:32, 235)
pops <- get_population(age_group_id = age_vec,
                       year_id = 2020,
                       location_id = lvl3_locs,
                       sex_id = c(1,2),
                       gbd_round_id = 7,
                       decomp_step = "iterative") %>%
  .[, `:=` (run_id = NULL, year_id = NULL)]
saveRDS(pops, file = "/ihme/homes/lojustin/csss_569_final_proj/data/population.RDS")

# Get draws and format
# Exposure ----
print("getting alcohol exposure")
get_exposure <- function(gbd_2020_year_id = 2019) {
  
  # Scatter 2020 exposure vs 2019 exposure
  exp_old <- get_draws(
    gbd_id_type = "rei_id",
    gbd_id = rei.id,
    source = "exposure",
    location_id = lvl3_locs,
    #age_group_id = age,
    year_id = 2019,
    gbd_round_id = 6,
    decomp_step = "step4"
  ) %>%
    format_draws(., version = "gbd_2019", measure = "Alcohol in grams/day")
  
  exp_new <- get_draws(
    gbd_id_type = "rei_id",
    gbd_id = rei.id,
    source = "exposure",
    location_id = lvl3_locs,
    #age_group_id = age,
    year_id = gbd_2020_year_id,
    gbd_round_id = 7,
    decomp_step = "iterative"
  ) %>%
    format_draws(., version = "gbd_2020", measure = "Alcohol in grams/day")
  
  expo <- merge(exp_old, exp_new, by = c(id_cols, "label"))
  
  return(expo)
}

exposure <- get_exposure()

# RR ----
print("getting alcohol rrs")
# Read in uploaded draws of RRs
#cause.ids <- readRDS("/ihme/dbd/anthropometrics/bmi/paf/gbd_2020/rr_draws/rei_id_370/cause_ids_vector.RDS")

rr_files <- list.files("/mnt/team/team/pub/sub_risks/alcohol/alcohol/drugs_alcohol/rr/rr_2020/final", full.names = T)
rr_new <-rbindlist(lapply(rr_files, fread), use.names = T)

rr_files_old <- list.files("/mnt/team/team/pub/sub_risks/alcohol/alcohol/drugs_alcohol/rr/rr_2016/4", full.names = T)
rr_old <-rbindlist(lapply(rr_files_old, fread), use.names = T)

rr_old <- rr_old[cause_id %in% unique(rr_new$cause_id)] 

# format draws
format_rr_draws <- function(df, version) {
  keep_cols <- c("exposure", "cause_id")
  df[, mean_val := mean(rr), by = c(keep_cols)]
  df[, lo_val := quantile(rr, probs = 0.025), by = c(keep_cols)]
  df[, hi_val := quantile(rr, probs = 0.975), by = c(keep_cols)]
  
  df <- unique(df[, c(keep_cols, "mean_val", "lo_val", "hi_val"), with = F])
  df[, version := version]
  
  return(df)
}

rr_old <- format_rr_draws(df = rr_old, version = "GBD 2016")
rr_new <- format_rr_draws(df = rr_new, version = "GBD 2020")

rr <- rbind(rr_old, rr_new)

# Save data ----
saveRDS(exposure, "data/alcohol_exposure.RDS")
saveRDS(rr, "data/alcohol_rr.RDS")
saveRDS(ages, "data/age_metadata.RDS")
saveRDS(causes, "data/cause_metadata.RDS")
saveRDS(locs, "data/location_metadata.RDS")
saveRDS(reis, "data/rei_metadata.RDS")
