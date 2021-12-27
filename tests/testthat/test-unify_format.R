#' @import testthat
#' @import magrittr


testthat::test_that("test-unify_format", {
  #errors
  testthat::expect_error(unify_format())
  testthat::expect_error(unify_format(2))

})

testthat::test_that("test-unify_cabin", {

  test_data <- data.frame(
    cbn = LETTERS[1:8],
    val = c(c(1:7), "cabin")
  )
  #errors
  testthat::expect_error(test_data %>% unify_cabin())

})
