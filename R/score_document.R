#' Score a document for all students
#'
#' @export
score_document <- function(Events) {
  Summed <- Events %>%
    group_by(login, item) %>%
    summarize(score = sum(correct))
  Total <- Summed %>%
    group_by(login) %>%
    summarize(TOTAL = sum(score, na.rm = TRUE))
  Result <- Summed %>%
    pivot_wider(id_cols = login, values_from = score, names_from = item)
  Result <- Result %>% left_join(Total)
}
