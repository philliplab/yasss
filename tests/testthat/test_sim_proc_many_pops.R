context('sim_proc_many_pops')

if (FALSE){
  library(testthat)
  devtools::load_all()
  c_arg_set <- arg_set1
  check_dsum(dcollection[[2]])
  check_dcollection(dcollection)
}

n_gen <- 5

arg_set1 <- list(
  label = 'A-based epitope',
  ancestors = paste(rep("A", 500), collapse = ''),
  r0 = 2,
  n_gen = n_gen,
  n_pop = Inf,
  mutator = list(fun = "mutator_uniform_fun",
                 args = list(mu = 1/250)),
  fitness_evaluator = list(fun = "fitness_evaluator_homology_fun",
                           args = list(comparators = paste(rep('XXXXA', 100), collapse = ''),
                                       h2fs = "h2fs_univariate_linear_fun"))
)

arg_set2 <- list(
  label = 'C-based epitope',
  ancestors = paste(rep("A", 500), collapse = ''),
  r0 = 2,
  n_gen = n_gen,
  n_pop = Inf,
  mutator = list(fun = "mutator_uniform_fun",
                 args = list(mu = 1/250)),
  fitness_evaluator = list(fun = "fitness_evaluator_homology_fun",
                           args = list(comparators = paste(rep('XXXXC', 100), collapse = ''),
                                       h2fs = "h2fs_univariate_linear_fun"))
)

arg_collection1 <- list(arg_set1, arg_set2)
arg_collection2 <- list(arg_set1)

many_pops1 <- sim_proc_many_pops(arg_collection1, n_sims = 1)
many_pops3 <- sim_proc_many_pops(arg_collection1, n_sims = 2)
many_pops4 <- sim_proc_many_pops(arg_collection2, n_sims = 1, output_dmat = TRUE)
many_pops5 <- sim_proc_many_pops(arg_collection2, n_sims = 1, output_dmat = TRUE, max_dmat_size = 10)

test_that('sim_proc_many_pops work', {
  expect_equal(class(many_pops1), 'list')
  x <- check_many_pops(many_pops1)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})

test_that('check_many_pops can catch broken dcollections', {
  many_pops2_1 <- many_pops1
  many_pops2_1$dcollection <- 'a'
  x <- check_many_pops(many_pops2_1)
  expect_false(x$valid_dcollection)

  many_pops2_2 <- many_pops1
  many_pops2_2$dcollection[[1]] <- 'a'
  x <- check_many_pops(many_pops2_2)
  expect_false(x$valid_dcollection)
})

test_that('setting n_sims causes more simulations to be run', {
  expect_equal(length(many_pops3$dcollection), 4)
  expect_equal(many_pops3$n_sims, 2)
})

test_that('output_dmats work', {
  expect_true('all_dmats' %in% names(many_pops4))
})

test_that('max_dmat_size argument works', {
  expect_equal(attr(many_pops5$all_dmats[[1]]$dmat, 'Size'), 10)
})

