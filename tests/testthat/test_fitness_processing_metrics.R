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

