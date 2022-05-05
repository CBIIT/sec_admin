transform_prior_therapy  <- function(therapy_string, csv_codes) {
  new_string <- gsub(
    "check_if_any\\(",
    paste("check_if_any(\"", csv_codes, "\",safe_query
          ,  "),
    therapy_string
  )
  return(new_string)
}