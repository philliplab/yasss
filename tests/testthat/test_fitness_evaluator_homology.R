context("fitness_evaluator_homology")

if (FALSE){
  yasss:::restart_r()
  library(testthat)
  devtools::load_all()
}

test_that("fitness_evaluator_homology checks the comparators", {
  fe <- list(fun = 'fitness_evaluator_homology_fun',
             the_seq = c("AAA", "AAC", "ACC", "CCC"),
             args = list(comparators = "AAA",
                         h2fs = "h2fs_univariate_linear_fun"))
  
  x <- check_fitness_evaluator(fe$the_seq, fe$fun, fe$args)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }

  args <- fe$args
  args$the_seq <- fe$the_seq
  args$comparators <- "AAY"
  expect_error({y <- do.call(get(fe$fun), args)},
    "Only A, C, G, T and X are allowed in comparators")

  args <- fe$args
  args$the_seq <- fe$the_seq
  args$comparators <- "XXX"
  expect_error({y <- do.call(get(fe$fun), args)},
    'A comparator must have at least one non-X character')
})

test_that("fitness_evaluator_homology works", {
  fe <- list(fun = 'fitness_evaluator_homology_fun',
             the_seq = c("AAAAAAAAA", "AAAAAAAAC", "AAAAAACCC", "CCCCCCCCC"),
             args = list(comparators = "AAAAAAAAA",
                         h2fs = "h2fs_univariate_linear_fun"))

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
  expect_equal(ncol(y$dists), 1)
  expect_equal(y$dists[1,1], 0/9)
  expect_equal(y$dists[2,1], 1/9)
  expect_equal(y$dists[3,1], 3/9)
  expect_equal(y$dists[4,1], 9/9)
  
  
  fe <- list(fun = 'fitness_evaluator_homology_fun',
             the_seq =                 c("AAAAAAAAATAT", "AAAAAAAACTAT", 
                                         "AAAAAACCCTAT", "CCCCCCCCCTAT"),
             args = list(comparators = c("AAAAAAAAXTAT", "XAAAAAAAATAT", 
                                         "XAXAXAXAXTAX"),
                         h2fs = "h2fs_univariate_linear_fun"))
  args <- fe$args
  args$the_seq <- fe$the_seq
  y <- do.call(get(fe$fun), args)
  expect_equal(ncol(y$dists), 3)

  expect_equal(y$dists[1,1], 0/11)
  expect_equal(y$dists[2,1], 0/11)
  expect_equal(y$dists[3,1], 2/11)
  expect_equal(y$dists[4,1], 8/11)

  expect_equal(y$dists[1,2], 0/11)
  expect_equal(y$dists[2,2], 1/11)
  expect_equal(y$dists[3,2], 3/11)
  expect_equal(y$dists[4,2], 8/11)

  expect_equal(y$dists[1,3], 0/6)
  expect_equal(y$dists[2,3], 0/6)
  expect_equal(y$dists[3,3], 1/6)
  expect_equal(y$dists[4,3], 4/6)

  if (FALSE){
    options(error = recover)
    x <- sim_pop('AAAAAAAAAAAA', r0 = 2, n_pop = 15)

    genealogy <- data.frame(gen_num = 0, 
                            id = 1L, 
                            parent_id = NA_real_,
                            the_seq = "AAAAAAAAA", 
                            n_mut = NA_real_,
                            recomb_pos = NA_real_, 
                            recomb_replaced = NA_character_, 
                            recomb_partner = NA_real_,
                            recomb_muts = NA_real_, 
                            fitness_score = 0.897154232952744)
    mutator <- list(fun = "mutator_uniform_fun", 
                    args = list(mu = 0.01))
    gen_num <- 1
    r0 <- 2
  }
  
  if (FALSE){
    fun <- fe$fun
    args <- fe$args
    the_seq <- x$the_seq
    comparators <- fe$args$comparators
    h2fs <- fe$args$h2fs
  }

  x <- sim_pop('AAAAAAAAAAAA', r0 = 2, n_pop = 15)
  fit_fun <- get(fe$fun)
  args <- fe$args
  args$the_seq = x$the_seq
  y <- do.call(fit_fun, args)
  check_fitness_evaluator_result(y, args)
  expect_equal(ncol(y$dists), length(fe$args$comparators))
  expect_true(all(y$dists >= 0))
  expect_true(all(y$dists <= 1))
  
  x <- sim_pop('AAAAAAAAAAAA', r0 = 2, n_pop = 15,
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
