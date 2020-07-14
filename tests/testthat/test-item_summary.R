test_that("Summarization by item works", {
  Events <- get_submissions(csv_name = gradesubmitr:::example_submissions2())
  Events <- get_submissions("/Users/kaplan/KaplanFiles/Packages/submitr/inst/tutorials/minimal/minimal_submissions.csv")
  test_item <- "deer-rise-mug-Qinline1"
  question_item(Events, item_name = test_item)
  })
