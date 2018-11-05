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

test_that('fitness_processing_metrics_to_df works', {
  fpm_df1 <- fpm1
  fpm_df1$sampling <- 'fit_threshold'
  fpm_df2 <- fpm1
  fpm_df2$sampling <- 'size_matched_sampling'

  fpm_for_df <- c(list(fpm_df1), list(fpm_df2))
  fpm_df <- fitness_processing_metrics_to_df(fpm_for_df)
  expect_equal(class(fpm_df), 'data.frame')
  expect_equal(nrow(fpm_df), length(fpm_for_df))

  col_names <- c("sim_id", "label", "sampling", "input_seqs", "output_seqs")
  for (col_name in col_names){
    expect_true(col_name %in% names(fpm_df))
  }
})


