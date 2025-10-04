index_to_date <- function(index) {
  dates <- seq(as.Date("2018-01-01"), as.Date("2022-12-31"), by = "day")
  if (any(index < 1 | index > length(dates))) {
    stop("out")
  }
  return(dates[index])
}



date_to_index <- function(date_vec) {
  dates <- seq(as.Date("2018-01-01"), as.Date("2022-12-31"), by = "day")
  date_vec <- as.Date(date_vec)
  
  if (any(!date_vec %in% dates)) {
    stop("not in")
  }
  
  return(match(date_vec, dates))
}