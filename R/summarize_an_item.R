#' Tabulate answers for one question item
#'
#' @param events Events in the standard format
#' @param item_name Name of the item
#' @param fiducial_level A quantile level for the fiducial date. Default: 0.25
#'
#' @details
#' - `first_response` gives the time (in hours) from the student's first
#' answer to the question.
#' `correct_response`` gives the time from the students first correct response
#' `last_response`, similar but for the last response, true or not
#'
#'
#' @export
summarize_MC_item <- function(events, item_name, fiducial_date = 0.25) {
  if(fiducial_date < 0 || fiducial_date > 1)
            stop("`fiducial_date` must be between zero and one. You have",
                  "`fiducial_date` = ", fiducial_date)
  fiducial_date <- quantile(events$time, fiducial_date)
  prompt <- events$prompt[1]
  # For each user, figure out the times of their answers
  Item_times <- events %>%
    mutate(relative_time = as.numeric(
      difftime(time, fiducial_date, units = "hours"))) %>%
    group_by(login, item) %>%
    summarize(first_time = min(relative_time),
           last_time = max(relative_time),
           correct_time = min(relative_time[correct]),
           correct_time = ifelse(correct_time == Inf, NA, correct_time))

  # Tally up all the possible answers that were selected by somebody
  # for this event
  Item_choices <-
    events %>%
    group_by(answer, correct) %>%
    summarize(overall_count = n()) %>%
    ungroup() %>%
    mutate(choice_id = LETTERS[1:nrow(.)],
           overall_frac = overall_count / sum(overall_count))

  Student_choices <-
    events %>%
    group_by(item, login, answer) %>%
    summarize(n = n()) %>%
    left_join(Item_choices) %>%
    group_by(login) %>%
    mutate(score = - mean(log(1 - pmin(.9, overall_frac), base = 2))) %>%
    select(- overall_frac, - overall_count, - answer) %>%
    pivot_wider(names_from=c(choice_id, correct), values_from = n, values_fill = 0)

  list(
    by_student = Student_choices %>% left_join(Item_times),
    overall = Item_choices,
    prompt = prompt
  )

}

#' @export
summarize_essay_item <- function(Item_events, item_name, fiducial_date = 0.25) {
  if(fiducial_date < 0 || fiducial_date > 1)
    stop("`fiducial_date` must be between zero and one. You have",
         "`fiducial_date` = ", fiducial_date)

  fiducial_date <- quantile(Item_events$time, fiducial_date)
  prompt <- Item_events$prompt[1]
  Res <- list()
  # For each user, pick out the latest submission
  Student_events <- Item_events %>%
    group_by(login) %>%
    filter(time == max(time)) %>% # Just the last essay
    mutate(relative_time = as.numeric(
      difftime(time, fiducial_date, units = "hours")),
      essay_length = nchar(answer)
    ) %>%
    select(time, login, item, document, answer, essay_length, relative_time)

  Essay_lengths <- mosaic::ntiles(Student_events$essay_length,
                                  n=ceiling(pmin(10, sqrt(nrow(Student_events)))),
                                  format = "right") %>%
    table() %>%
    tibble::as_tibble() %>%
    rename(., Essay_length = ., n_essays = n) %>%
    mutate(Essay_length = paste("<=", Essay_length, "chars"))

  list(
    by_student = Student_events,
    overall = Essay_lengths,
    prompt = prompt
  )

}
