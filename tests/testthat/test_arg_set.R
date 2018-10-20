context('arg_set')

n_gen <- 6

arg_set1 <- list(
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

test_that('check_arg_set lets correct arg_sets pass', {
  x <- check_arg_set(arg_set1)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
  
  x <- check_arg_set(arg_set2)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})

test_that('check_arg_set catches bad arg_sets', {
  arg_set3_1 <- arg_set1
  arg_set3_2 <- arg_set1
  arg_set3_3 <- arg_set1
  arg_set3_1$ancestors <- NULL
  arg_set3_2$ancestors <- 1
  arg_set3_3$ancestors <- c('AA', 'CCC')

  x <- check_arg_set(arg_set3_1)
  expect_false(x$has_ancestors)
  x <- check_arg_set(arg_set3_2)
  expect_false(x$ancestors_are_character)
  x <- check_arg_set(arg_set3_3)
  expect_false(x$ancestors_same_length)
})

test_that('check_arg_collection lets correct arg_collections pass', {
  x <- check_arg_collection(arg_collection1)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})  
