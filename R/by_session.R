#' Calculates session duration and other measures
#'
#' @param frame Data frame in standard format for this system
#'
#' Each session is about a particular document
#'
#'
#' @export
by_session <- function(frame) {
  frame %>%
    filter( ! grepl("section", type)) %>%
    group_by(session_id) %>%
    summarize(nevents = n(),  user = login[1],
              document = document[1],
              system = system_id[1],
              start = min(time), finish = max(time),
              duration = as.numeric(difftime(finish, start, units = "secs")),
              n_distinct_items = length(unique(item)),
              n_distinct_code = length(unique(item[grepl("exercise", type)])),
              n_code_runs = sum(type == "exercise_result"),
              n_code_submissions = sum(type == "exercise_result" ),
              n_code_success = sum(type == "exercise_result" & is_correct, rm.na=TRUE),
              n_distinct_questions = length(unique(item[grepl("question", type)])),
              n_question_attempts = sum(type == "question_submission"),
              n_question_success = sum(type == "question_submission" & is_correct, rm.na=TRUE)

              )

}
