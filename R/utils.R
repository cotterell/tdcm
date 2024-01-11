# utils.R -- Utility functions for `tdcm` package.

#' Emit `tdcm`-related message
#'
#' @description
#' A short description...
#'
#' @param text The message text to emit, an object that can be coerced to a
#' `"character"` via [[as.character()]].
#'
#' @keywords internal
tdcm_emit <- function(text, label = "INFO:", func = base::message, ...) {
  text <- paste("[tdcm]", label, as.character(text))
  func(text, ...)
} # tdcm_emit

#' @describeIn tdcm_emit Emit `tdcm`-related warning message
#' @keywords internal
tdcm_warn <- function(text, ...) {
  tdcm_emit(text, label = "WARN:", func = base::warning, ...)
} # tdcm_warn

#' @describeIn tdcm_emit Emit `tdcm`-related stop message
#' @keywords internal
tdcm_stop <- function(text, ...) {
  tdcm_emit(text, label = "STOP:", func = base::stop, ...)
} # tdcm_stop

#' Check the number of time points for TDCM data.
#'
#' @param ir_data An `ir_data` parameter supplied to `tdcm()`.
#' @param q_data An `q_data` parameter supplied to `tdcm()`.
#'
#' @keywords internal
tdcm_check_dims <- function(ir_data, q_data) {

  # ensure the number of time points match
  if (ir_data[1] != q_data[1]) {
    text = sprintf(
      paste(
        "The number of time points in ir_data (%d) is not equal to the number",
        "of time points in q_data (%s)"
      ), # paste
      ir_data[1],
      q_data[1]
    ) # text
    tdcm_stop(text)
  } # if

  # ensure the number of items match
  if (ir_data[3] != q_data[2]) {
    text = sprintf(
      paste(
        "The number of items in ir_data (%d) is not equal to the number of",
        "items in q_data (%s)"
      ), # paste
      ir_data[3],
      q_data[2]
    ) # text
    tdcm_stop(text)
  } # if

  return(NULL)

} # tdcm_check_dims
