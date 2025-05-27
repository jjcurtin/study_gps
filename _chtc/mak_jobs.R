# setup chtc jobs & associated files/folders

library(tidyverse) 
devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/chtc/fun_make_jobs.R?raw=true")

path_training_controls <- here::here("shared/scripts_chtc/models_gps/training_controls_gps.R")
make_jobs(path_training_controls, overwrite_batch = TRUE)