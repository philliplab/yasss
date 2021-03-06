context('arg_set')

if (FALSE){
  yasss:::restart_r()
  devtools::load_all()
  library(testthat)
}

n_gen <- 6

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
                                       h2fs = "h2fs_univariate_linear_fun")),
  required_fitness = 0.02
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
                                       h2fs = "h2fs_univariate_linear_fun")),
  required_fitness = 0.97
)

arg_collection1 <- list(arg_set1, arg_set2)

seq_to_check_fitness_evaluator <- paste(rep('AAATCGAAAG', 50), collapse = '')

test_that('check_arg_set lets correct arg_sets pass', {
  x <- check_arg_set(arg_set1, the_seq = seq_to_check_fitness_evaluator)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
  
  x <- check_arg_set(arg_set2, the_seq = seq_to_check_fitness_evaluator)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})

test_that('check_arg_set catches bad ancestors', {
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

test_that('check_arg_set catches bad r0s', {
  arg_set4_1 <- arg_set1
  arg_set4_2 <- arg_set1
  arg_set4_3 <- arg_set1
  arg_set4_4 <- arg_set1
  arg_set4_5 <- arg_set1
  arg_set4_1$r0 <- NULL
  arg_set4_2$r0 <- 'a'
  arg_set4_3$r0 <- 2:3
  arg_set4_4$r0 <- 2.5
  arg_set4_5$r0 <- 0.5

  x <- check_arg_set(arg_set4_1)
  expect_false(x$has_r0)
  x <- check_arg_set(arg_set4_2)
  expect_false(x$r0_is_numeric)
  x <- check_arg_set(arg_set4_3)
  expect_false(x$r0_is_length_one)
  x <- check_arg_set(arg_set4_4)
  expect_false(x$r0_is_positive_integer)
  x <- check_arg_set(arg_set4_5)
  expect_false(x$r0_is_positive_integer)
})

test_that('check_arg_set catches bad n_gens', {
  arg_set4_1 <- arg_set1
  arg_set4_2 <- arg_set1
  arg_set4_3 <- arg_set1
  arg_set4_4 <- arg_set1
  arg_set4_5 <- arg_set1
  arg_set4_1$n_gen <- NULL
  arg_set4_2$n_gen <- 'a'
  arg_set4_3$n_gen <- 2:3
  arg_set4_4$n_gen <- 2.5
  arg_set4_5$n_gen <- 0.5

  x <- check_arg_set(arg_set4_1)
  expect_false(x$has_n_gen)
  x <- check_arg_set(arg_set4_2)
  expect_false(x$n_gen_is_numeric)
  x <- check_arg_set(arg_set4_3)
  expect_false(x$n_gen_is_length_one)
  x <- check_arg_set(arg_set4_4)
  expect_false(x$n_gen_is_positive_integer)
  x <- check_arg_set(arg_set4_5)
  expect_false(x$n_gen_is_positive_integer)
})

test_that('check_arg_set catches bad n_pops', {
  arg_set4_1 <- arg_set1
  arg_set4_2 <- arg_set1
  arg_set4_3 <- arg_set1
  arg_set4_4 <- arg_set1
  arg_set4_5 <- arg_set1
  arg_set4_1$n_pop <- NULL
  arg_set4_2$n_pop <- 'a'
  arg_set4_3$n_pop <- 2:3
  arg_set4_4$n_pop <- 2.5
  arg_set4_5$n_pop <- 0.5

  x <- check_arg_set(arg_set4_1)
  expect_false(x$has_n_pop)
  x <- check_arg_set(arg_set4_2)
  expect_false(x$n_pop_is_numeric)
  x <- check_arg_set(arg_set4_3)
  expect_false(x$n_pop_is_length_one)
  x <- check_arg_set(arg_set4_4)
  expect_false(x$n_pop_is_positive_integer)
  x <- check_arg_set(arg_set4_5)
  expect_false(x$n_pop_is_positive_integer)
})

test_that('check_arg_set can catch a bad mutator', {
  mutator = list(fun = "NOTAREALFUNCTIONNAMEBLEH",
                 args = list(mu = -5))
  arg_set5 <- arg_set1
  arg_set5$mutator <- mutator
  x <- check_arg_set(arg_set5)
  expect_false(x$mutator_fun_is_getable)
  
  mutator = list(fun = "mutator_uniform_fun",
                 args = list(mu = -5))
  arg_set5_1 <- arg_set1
  arg_set5_1$mutator <- mutator
  x <- check_arg_set(arg_set5_1)
  expect_false(x$mutator_all_mu_greater_equal_0)
})

test_that('check_arg_set can catch a bad fitness_evaluator', {
  fitness_evaluator1 <- list(fun = "NOTAREALFUNCTIONNAMEBLEHUGH",
                           args = list(comparators = paste(rep('XXXXA', 100), collapse = ''),
                                       h2fs = "h2fs_univariate_linear_fun"))
  arg_set6 <- arg_set1
  arg_set6$fitness_evaluator <- fitness_evaluator1
  x <- check_arg_set(arg_set6, the_seq = seq_to_check_fitness_evaluator)
  expect_false(x$fe_fun_is_getable)
})

test_that('check_arg_set can catch a bad ps_rate', {
  arg_set7_1 <- arg_set1
  arg_set7_2 <- arg_set1
  arg_set7_3 <- arg_set1
  arg_set7_4 <- arg_set1
  
  arg_set7_1$ps_rate <- -1
  arg_set7_2$ps_rate <- 1
  arg_set7_3$ps_rate <- 'A'
  arg_set7_4$ps_rate <- c(0.1, 0.2)

  x <- check_arg_set(arg_set7_1)
  expect_false(x$ps_rate_between_0_1)
  x <- check_arg_set(arg_set7_2)
  expect_false(x$ps_rate_between_0_1)
  x <- check_arg_set(arg_set7_3)
  expect_false(x$ps_rate_numeric)
  x <- check_arg_set(arg_set7_4)
  expect_false(x$ps_rate_length_one)
})

test_that('check_arg_collection lets correct arg_collections pass', {
  x <- check_arg_collection(arg_collection1, verbose = FALSE)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})  

test_that('check_arg_collection catches duplicated labels', {
  with_dups <- list(arg_set1, arg_set1)
  x <- check_arg_collection(with_dups)
  expect_false(x$labels_unique)
})  

