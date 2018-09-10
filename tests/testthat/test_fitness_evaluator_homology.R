context("fitness_evaluator_homology")

test_that("fitness_evaluator_homology works", {
  fe <- list(fun = 'fitness_evaluator_homology_fun',
             the_seq = c("AAAAAAAAA", "AAAAAAAAC", "AAAAAACCC", "CCCCCCCCC"),
             args = list(comparators = "AAAAAAAAA",
                         h2fs = "h2fs_1_epitope"))

  # debugging
  #TODO: delete
  if (FALSE){
    fun <- fe$fun
    args <- fe$args
    the_seq <- fe$the_seq
    comparators <- fe$args$comparators
    h2fs <- fe$args$h2fs
  }

  x <- check_fitness_evaluator(fe$the_seq, fe$fun, fe$args)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }

  args <- fe$args
  args$the_seq <- fe$the_seq
  y <- do.call(get(fe$fun), args)
  expect_equal(y$dists[1,1], 0/9)
  expect_equal(y$dists[2,1], 1/9)
  expect_equal(y$dists[3,1], 3/9)
  expect_equal(y$dists[4,1], 9/9)

  x <- sim_pop('AAAAAAAAA', r0 = 2, n_pop = 15)
  fit_fun <- get(fe$fun)
  args <- fe$args
  args$the_seq = x$the_seq
  y <- do.call(fit_fun, args)
  check_fitness_evaluator_result(y, args)
  expect_equal(ncol(y$dists), length(fe$args$comparators))
  expect_true(all(y$dists >= 0))
  expect_true(all(y$dists <= 1))
  
  x <- sim_pop('AAAAAAAAA', r0 = 2, n_pop = 15,
               mutator = list(fun = "mutator_uniform_fun",
                              args = list(mu = 0.5)))
  fit_fun <- get(fe$fun)
  args <- fe$args
  args$the_seq = x$the_seq
  y <- do.call(fit_fun, args)
  check_fitness_evaluator_result(y, args)
  expect_equal(ncol(y$dists), length(fe$args$comparators))
  expect_true(all(y$dists >= 0))
  expect_true(all(y$dists <= 1))

})

test_that("max_homology correctly computes the max possible homology", {
  comparator_df <- data.frame(
    compa =     c("AAA", "XXX", "XAA", "AAX", "AXC", "AAAAAAAAX"),
    max_score = c(     3,    0,     2,      2,    2,           8),
    stringsAsFactors = FALSE)

  comped_max_homology <- max_homology(comparator_df$compa)
  for (i in 1:nrow(comparator_df)){
    expect_equal(comped_max_homology[[comparator_df[i,'compa']]], 
                 comparator_df[i,'max_score'], 
                 info = comparator_df[i,'compa'])
  }
})

