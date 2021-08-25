test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


#outcomes
n <- 10000
cases_right <- c(600, 200, 100, 100)
cases_left <- c(600, 400, 100, 100)
outcome_texts <- c("Really bad headache", "Headache. Not so bad. But still...", "test 3", "test4")
headline_main_text_left <- "Placebo"
headline_main_text_right <- "Einhornstaub"
headline_1_text_left <- "erhielten"
headline_2_text_left <- "Personen"
headline_2_text_right <- headline_2_text_left



infobar(headline_main_text_left, headline_main_text_right,
        headline_1_text_left, headline_2_text_left,
        headline_2_text_right, n, outcome_texts, cases_right, cases_left,
        outcome_fontface = "plain", big.mark = ".", decimal.mark = ",")
