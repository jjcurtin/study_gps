# setup jobs to make GPS features on CHTC

# Tracking notes
# labels_version v1 used an old version of the get_lapse_labels() function
# old version excluded no lapses 6 hours within lapse event
# new version uses prior system of 24 hour exclusion rule
# labels_version v2 uses this updated version of the function
# version v2 of this mak_jobs script corresponds to the same

# Constants
window <- "day" # hour day week
roll_dur <- "hour" # "day"
sample <- "gps"
labels_version <- "v2" # corresponds to version of lapse_labels file
version <- "v2" # corresponds to version of mak_jobs script

# load packages
library(tidyverse)
library(lubridate)

devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true",
                     sha1 = "a58e57da996d1b70bb9a5b58241325d6fd78890f")

# Paths and filenames
path_jobs <- format_path("studydata/risk/chtc/gps")
name_job <- str_c("features_", window, "_", roll_dur, "hour_", version)

path_processed <- format_path("studydata/risk/data_processed/gps")
name_gps <- "gps_enriched.csv.xz"
#name_labels <- str_c("labels_", sample, "_",
                     #window, "_",
                     #roll_dur, "hour", "_",
                     #labels_version, ".csv")
  
name_labels <- str_c("labels_", roll_dur, ".csv")
name_study_dates <- "study_dates.csv"

path_shared <- format_path("studydata/risk/data_processed/shared")
if (roll_dur == "hour") {
  name_lapses <- "lapses.csv"
}
if (roll_dur == "day") {
  name_lapses <- "lapses_day.csv"
}


name_fun <- "fun_features.R"
name_script <- "mak_features.R"


# create new job directory (if it does not already exist)
if (!dir.exists(file.path(path_jobs, name_job))) {
  dir.create(file.path(path_jobs, name_job))
  dir.create(file.path(path_jobs, name_job, "input"))
  dir.create(file.path(path_jobs, name_job, "output"))
} else {
  stop("Job folder already exists. No new folders created.")
}

# save out jobs csv file for queue
n_labels <- nrow(read_csv(file.path(path_processed, name_labels),
                          show_col_types = FALSE))
labels_per_job <- 300
job_start <- seq(1, n_labels, by = labels_per_job)
job_stop <- c(seq(job_start[2] - 1, n_labels, by = labels_per_job), n_labels)
tibble(job_start, job_stop) |> 
  write_csv(file.path(path_jobs, name_job, "input", "jobs.csv"),
              col_names = FALSE, append = FALSE)

# select and format relevant variables and then copy gps
read_csv(file.path(path_processed, name_gps), show_col_types = FALSE) |>
  select(-street_address, -city, -state, -full_address,
         -sgmnt_type, -trckpnt_type, -app_source, -data_type) |> 
  arrange(subid, time) |>
  write_csv(file.path(path_jobs, name_job, "input", "gps.csv.xz"))

# select and format relevant variables and then copy lapses
read_csv(file.path(path_shared, name_lapses), show_col_types = FALSE) |> 
  filter(!exclude) |> 
  select(subid, dttm_obs = lapse_start) |> 
  arrange(subid, dttm_obs) |> 
  mutate(count = "lapse") |> 
  write_csv(file.path(path_jobs, name_job, "input", "lapses.csv"))

# copy over other data files verbatim
file.copy(from = file.path(path_processed, name_labels),
          to = file.path(path_jobs, name_job, "input", "labels.csv"))
file.copy(from = file.path(path_processed, name_study_dates),
          to = file.path(path_jobs, name_job, "input", "study_dates.csv"))

# copy over function script
file.copy(from = file.path("../proj_risk/shared", name_fun),
          to = file.path(path_jobs, name_job, "input", name_fun))

# copy over unix files(run script, submit)
file.copy(from = file.path("_chtc", "features", "unix",
                           c(list.files(file.path("_chtc",
                                                  "features", "unix")))),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE)

# copy R script
file.copy(from = file.path("_chtc", "features", name_script),
          to = file.path(path_jobs, name_job, "input", name_script))
