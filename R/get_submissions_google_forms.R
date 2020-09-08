#' Reading Hash submissions from a GoogleForms sheet
#'
#'
# CSV_name <- "/Users/kaplan/Downloads/Math 141Z submissions.csv (2).zip"

#' @export
get_submissions_google_forms <- function(csv_name) {

  Raw <- readr::read_csv(csv_name)
  names(Raw) <- c("time_submitted", "id_submitted", "hash",  "auth_submitted")

  Results <- list()

  for (k in 1:nrow(Raw)) {
    row <- Raw[k,]
    df <- try(
      learnrhash::decode_obj(row$hash),
      silent = TRUE
    )
    # If no events were in the hash, create a non-empty data frame
    if (inherits(df, "try-error") || nrow(df)  == 0) {
      df <- tibble::tibble(time_submitted = row$time_submitted)
      #cat(k, "...")
    }

    # Populate the data frame with the submission-form data
    df$time_submitted <- row$time_submitted
    df$id_submitted <- row$id_submitted
    df$auth_submitted <- row$auth_submitted
    # handle debris from submitr
    if ("user_id" %in% names(df))
      df["user_id"] <- NULL
    if ("authentication" %in% names(df))
      df["authentication"] <- NULL

    Results[[k]] <- df
  }

  bind_rows(Results) -> Events

  convert_to_POSIX <- function(time_str) {
    time = strptime(time_str, format = "%a %b %d %H:%M:%S %p %Y %z")
    time2 =  strptime(time_str, format = "%a %b %d %H:%M:%S %Y %z")
    keepers <- is.na(time)
    time[keepers] <- time2[keepers]

    time
  }



  names(Events) <- c(
                     "event_time",  "who", "session_id", "type", "item",
                     "prompt", "answer", "correct", "feedback",
                     "time_submitted",
                     "id_submitted",  "auth_submitted")


  Events$event_time <- convert_to_POSIX(Events$event_time)
  Events$time_submitted <-  lubridate::ymd_hms(Events$time_submitted)
  Tmp <- Events %>%
    tidyr::separate(who, into = c("login", "system_id"),
                    sep = " ") %>%
    tidyr::separate(item, into = c("item", "type", "document", "version"),
                    sep = " ") %>%
    filter((type != "trash")) %>%
    mutate(login = paste(id_submitted, auth_submitted, sep=":"))

  Tmp
}




