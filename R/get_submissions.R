#' Translate a raw submission file into the standard format
#'
#' @param csv_name Name of the CSV file.
#' @param key The google key of the sheet holding submissions
#'
#' @details Provide either of `csv_name` or `key`, but not both.
#'
#' @export
get_submissions <- function(csv_name = NULL,                            # Change to NULL
                            # Change to NULL
                            key = NULL) {
  if (is.null(csv_name)) {
    if (is.null(key)) {
      stop("Provide either `key` or `csv_name")
    } else {
      # Read the google sheet -- not yet implemented
      Raw <- iris # BOGUS
      stop("Still need to implement retrieval from Google sheet")
    }
  } else {
      Raw <- readr::read_csv(csv_name,
                             col_names = FALSE)
  }

  to_standard_format(Raw)
}

# This is keyed to the format of events in `submitr`.
# If that changes, this needs to change.

# Should check format and call the right function

to_standard_format <- function(raw) {
  names(raw) <- c("timestamp", "who", "session_id",
                  "type", "item", "prompt", "answer",
                   "correct", "feedback")


  time = strptime(raw$timestamp, format = "%a %b %d %H:%M:%S %p %Y %z")
  time2 =  strptime(raw$timestamp, format = "%a %b %d %H:%M:%S %Y %z")
  keepers <- is.na(time)
  time[keepers] <- time2[keepers]
  raw$time <- time
  Tmp <- raw %>%
    tidyr::separate(who, into = c("login", "system_id"),
                    sep = " ") %>%
    tidyr::separate(item, into = c("item", "type", "document", "version"),
                    sep = " ") %>%
    filter((type != "trash"))

  Tmp
}

example_submissions1 <- function() {
  system.file("submissions-2020-07-10.csv", package = "gradesubmitr")
}
example_submissions2 <- function() {
  system.file("submissions-2020-07-13.csv", package = "gradesubmitr")
}
