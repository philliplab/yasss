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

test_that('sim_proc_many_pops work', {
  many_pops <- sim_proc_many_pops(arg_collection1)
  expect_equal(class(many_pops), 'list')
  x <- check_many_pops(many_pops)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})
