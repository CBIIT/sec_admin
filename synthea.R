
# Logic to create the Value column of the Synthea data tables based on values from the DB.
create_value <- function(valtype_cd, tval_char, nval_num, name) {
  if (valtype_cd == 'N') {
    
    # Numeric value
    return(nval_num)
    
  } else {
    
    # String value
    if (tval_char == 'true') {
      return(name)
    } else {
      return(tval_char)
    }
  }
}
