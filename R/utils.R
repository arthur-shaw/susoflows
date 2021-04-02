#' Confirm that character input is a valid date
#' 
#' Checks whether character date is a valid date in YYYY-MM-DD format. Returns `TRUE` if so, `FALSE` otherwise
#' 
#' @param date Character vector
#' 
#' @return Logical. Returns `TRUE` if input is a valid date, `FALSE` otherwise
#' 
#' @noRd 
is_valid_date <- function(
    date
) {

    tryCatch(
        error = function(cnd) FALSE,
        {
            as.Date(date)
            TRUE
        }   
    )

}
