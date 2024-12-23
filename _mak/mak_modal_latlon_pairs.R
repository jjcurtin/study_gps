#Round lat/lon points to 4th decimal place per: https://blis.com/precision-matters-critical-importance-decimal-places-five-lowest-go/#:~:text=The%20number%20of%20decimal%20places%20correlates%20directly%20to%20the%20level,a%20large%20city%20or%20district.

context_filtered <- context_filtered |>
  mutate(lat_rounded = round(lat, 4),
         lon_rounded = round(lon, 4))


#Calculate modal lat/lon at time of lapse event. If no mode, then select the first point.

get_mode <- function(x) {
  unique_x <- unique(x)
  freq <- table(x)
  mode_value <- unique_x[which.max(freq)]
  return(mode_value)
}


get_modal_latlon <- function(lapse_id_arg, context_filtered) {

  context_tmp <- context_filtered |> filter(lapse_id_arg == lapse_id)

  context_tmp <- context_tmp |>
    mutate(latlon = str_c(lat_rounded, ", ",lon_rounded))

  mode_value <- get_mode(context_tmp$latlon)

  freq <- table(context_tmp$latlon)

  if (max(freq) > 1) {
    # keep rows where the column value equals the mode
    #context_tmp <- context_tmp[context_tmp[[latlon]] == mode_value, ]
    context_tmp <- context_tmp[context_tmp$latlon == mode_value, ]
  } else {
    # take first row if unique
    context_tmp <- context_tmp[1, , drop = FALSE]
  }

  return(context_tmp)
}


context_modal <- context_filtered$lapse_id |>
  unique() |>
  furrr::future_map(\(lapse_id) get_modal_latlon(lapse_id_arg = lapse_id,
                                                 context_filtered)) |>
  list_rbind()
