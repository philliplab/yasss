context('sim_proc_many_pops')

# fitness processing metrics at bottom

if (FALSE){
  yasss:::restart_r()
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
  required_fitness = .97
)

arg_set3 <- list(
  label = 'A-based epitope',
  ancestors = paste(rep("A", 500), collapse = ''),
  r0 = 2,
  n_gen = n_gen+2,
  n_pop = Inf,
  mutator = list(fun = "mutator_uniform_fun",
                 args = list(mu = 1/250)),
  fitness_evaluator = list(fun = "fitness_evaluator_homology_fun",
                           args = list(comparators = paste(rep('XXXXA', 100), collapse = ''),
                                       h2fs = "h2fs_univariate_linear_fun")),
  required_fitness = 0.02
)



arg_collection1 <- list(arg_set1, arg_set2)
arg_collection2 <- list(arg_set1)
arg_collection3 <- list(arg_set3)

many_pops1 <- sim_proc_many_pops(arg_collection1, n_sims = 1)
many_pops3 <- sim_proc_many_pops(arg_collection1, n_sims = 2)
many_pops4 <- sim_proc_many_pops(arg_collection2, n_sims = 1, output_dmat = TRUE)
many_pops5 <- sim_proc_many_pops(arg_collection2, n_sims = 1, output_dmat = TRUE, max_dmat_size = 10)
many_pops6 <- sim_proc_many_pops(arg_collection3, n_sims = 1, output_dmat = TRUE, fitness_processing = 'fit_unfit_pair', output_genealogy = 'full')





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

context('fitness_processing_metrics')

fpm1 <- list(sim_id = 1,
             label = 'A',
             sampling = 'none',
             input_seqs = 100,
             output_seqs = 100)

fpm2 <- fpm1
fpm2$sim_id <- 'A'
fpm2$label <- 1:2
fpm2$sampling <- 'oops'

fpm3 <- fpm1
fpm3$input_seqs <- 'A'
fpm4 <- fpm1
fpm4$input_seqs <- 100:101
fpm5 <- fpm1
fpm5$input_seqs <- 100.5

fpm6 <- fpm1
fpm6$output_seqs <- 'A'
fpm7 <- fpm1
fpm7$output_seqs <- 100:101
fpm8 <- fpm1
fpm8$output_seqs <- 100.5

test_that('check_fitness_processing_metrics work', {
  x <- check_fitness_processing_metrics(fpm1)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }

  x <- check_fitness_processing_metrics(fpm2)
  expect_false(x$sim_id_integer)
  expect_false(x$label_length_one)
  expect_false(x$sampling_valid)

  x <- check_fitness_processing_metrics(fpm3)
  expect_false(x$input_seqs_integer)
  x <- check_fitness_processing_metrics(fpm4)
  expect_false(x$input_seqs_length_one)
  x <- check_fitness_processing_metrics(fpm5)
  expect_false(x$input_seqs_integer)

  x <- check_fitness_processing_metrics(fpm6)
  expect_false(x$output_seqs_integer)
  x <- check_fitness_processing_metrics(fpm7)
  expect_false(x$output_seqs_length_one)
  x <- check_fitness_processing_metrics(fpm8)
  expect_false(x$output_seqs_integer)
})

test_that('required_fitness required if non-none fitness processing',{
  arg_set4 <- list(
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
  arg_collection4 <- list(arg_set4)
  expect_error(many_pops7 <- sim_proc_many_pops(arg_collection4, n_sims = 1, fitness_processing = 'fit_unfit_pair', output_genealogy = 'full'), 'error in arg_collection')
})

test_that('recombination occurs',{
  arg_set4 <- list(
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
    required_fitness = 0.02,
    ps_rate = 0.2
  )
  arg_collection4 <- list(arg_set4)
  many_pops7 <- sim_proc_many_pops(arg_collection4, n_sims = 1, fitness_processing = 'fit_unfit_pair', output_genealogy = 'full')

  x <- many_pops7$all_genealogies[[1]]

  expect_false(all(is.na(x$recomb_pos)))
  expect_false(all(is.na(x$recomb_partner)))
  expect_false(all(is.na(x$recomb_muts)))
  expect_false(all(is.na(x$recomb_replaced)))
})






