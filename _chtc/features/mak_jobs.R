# setup jobs to make GPS features on CHTC

# Constants
window <- "day" # hour day week
roll_dur: 1 # 1 24
lead <- 0
sample <- "gps"
labels_version <- "v1" # corresponds to version of lapse_labels file
version <- "v1" # corresponds to version of mak_jobs script

# load packages
library(tidyverse)
library(lubridate)

devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true",
                     sha1 = "a58e57da996d1b70bb9a5b58241325d6fd78890f")

path_gps <- format_path("studydata/risk/data_processed/gps")

# Paths and filenames
path_jobs <- format_path("studydata/risk/chtc/gps")
name_job <- str_c("features_", window, "_", roll_dur, "hour_", lead, "_", version)

path_processed <- format_path("studydata/risk/data_processed/gps")
name_gps <- "gps_enriched.csv.xz"
name_labels <- str_c("labels_", sample, "_",
                     window, "_",
                     roll_dur, "hour", "_",
                     labels_version, ".csv")
name_study_dates <- "study_dates.csv"

path_shared <- format_path("studydata/risk/data_processed/shared")
if (roll_dur == 1) {
  name_lapses <- "lapses.csv"
}
if (roll_dur == 24) {
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
n_jobs <- nrow(read_csv(file.path(path_processed, name_labels), show_col_types = FALSE))
labels_per_job <- 300
job_start <- seq(1, n_jobs, by = labels_per_job)
job_stop <- c(seq(job_start[2] - 1, n_jobs, by = labels_per_job), n_jobs)
tibble(job_start, job_stop) %>%
  write_csv(file.path(path_jobs, name_job, "input", "jobs.csv"),
              col_names = FALSE, append = FALSE)

# select and format relevant variables and then copy gps
read_csv(file.path(path_processed, name_gps), show_col_types = FALSE) |> 
  arrange(subid, dttm_obs) |> 
  # this probably still needs to be the compressed file?
  write_csv(file.path(path_jobs, name_job, "input", "gps.csv"))

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
file.copy(from = file.path("shared", name_fun),
          to = file.path(path_jobs, name_job, "input", name_fun))

# copy over unix files(run script, submit)
file.copy(from = file.path("gps", "chtc", "features", "unix", c(list.files(file.path("gps", "chtc", "features", "unix")))),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE)

# copy R script
file.copy(from = file.path("gps", "chtc", "features", name_script),
          to = file.path(path_jobs, name_job, "input", name_script))
