context("fitness_evaluator_homology")

test_that("fitness_evaluator_homology works", {
  fe <- list(fun = 'fitness_evaluator_homology_fun',
             args = list(comparators = "AAAAAAAAA",
                         h2fs = "h2fs_1_epitope"))

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

  x <- sim_pop('AAAAAAAAA', r0 = 2, n_pop = 15)
  fit_fun <- get(fe$fun)
  args <- fe$args
  args$the_seq = x$the_seq
  y <- do.call(fit_fun, args)
  check_fitness_evaluator_result(y, args)
  
  x <- sim_pop('AAAAAAAAA', r0 = 2, n_pop = 15,
               mutator = list(fun = "mutator_uniform_fun",
                              args = list(mu = 0.5)))
  fit_fun <- get(fe$fun)
  args <- fe$args
  args$the_seq = x$the_seq
  y <- do.call(fit_fun, args)
  check_fitness_evaluator_result(y, args)
})
