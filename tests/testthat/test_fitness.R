context("fitness general")

test_that("check_fitness_evaluator works", {
  fe <- list(fun = 'fitness_evaluator_uniform',
             args = NULL)
  x <- check_fitness_evaluator(fe$fun, fe$args)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})

test_that("assign_fitness works", {
  fe <- list(fun = 'fitness_evaluator_uniform',
             args = NULL)

  x <- check_fitness_evaluator(fe$fun, fe$args)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }

  x <- sim_pop('AAAA', gen_size = 2, n_pop = 15)
  x$fitness_score <- NA_real_
  y <- assign_fitness(x, fe)

  z <- check_genealogy(y)
  for (i in names(z)){
    expect_true(z[[i]], info = i)
  }
  
  y_non_last <- y %>% filter(gen_num != max(gen_num))
  y_last <- y %>% filter(gen_num == max(gen_num))

  expect_true(all(is.na(y_non_last$fitness_score)))
  expect_true(all(!is.na(y_last$fitness_score)))
  expect_true(all(y_last$fitness_score > 0 & y_last$fitness_score < 1))
})

