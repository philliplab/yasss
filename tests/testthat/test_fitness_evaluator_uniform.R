context("fitness_evaluator_uniform")

test_that("fitness_evaluator_uniform works", {
  fe <- list(fun = 'fitness_evaluator_uniform',
             args = NULL)

  # debugging
  #TODO: delete
  if (FALSE){
    fun <- fe$fun
    args <- fe$args
  }

  x <- check_fitness_evaluator(fe$fun, fe$args)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }

  x <- sim_pop('AAAA', gen_size = 2, n_pop = 15)
  fit_fun <- get(fe$fun)
  args <- fe$args
  args$the_seq = x$the_seq
  y <- do.call(fit_fun, args)
  expect_equal(class(y), "list")
  expect_true("fitness_score" %in% names(y))
  expect_true("the_seq" %in% names(y))
  expect_equal(class(y$fitness), "numeric")
  expect_equal(class(y$the_seq), "character")
  expect_true(all(y$the_seq == x$the_seq))
  
  x <- sim_pop('AAAAAAAAA', gen_size = 2, n_pop = 15,
               mutator = list(fun = "mutator_uniform_fun",
                              args = list(mu = 0.5)))
  fit_fun <- get(fe$fun)
  args <- fe$args
  args$the_seq = x$the_seq
  y <- do.call(fit_fun, args)
  expect_equal(class(y), "list")
  expect_true("fitness_score" %in% names(y))
  expect_true("the_seq" %in% names(y))
  expect_equal(class(y$fitness), "numeric")
  expect_equal(class(y$the_seq), "character")
  expect_true(all(y$the_seq == x$the_seq))
})
