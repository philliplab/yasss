context("fitness_evaluator_general")

test_that("check_fitness_evaluator works", {
  fe <- list(fun = 'fitness_evaluator_uniform',
             args = NULL)
  x <- check_fitness_evaluator(fe$fun, fe$args)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})
