context("Fitness: General")


SAMPLE_GENEALOGIES <- list(
  bif_2gen = data.frame(
#==========================================================
gen_num          =  c(  0,              1,              1),
id               =  c(  1,              1,              2),
parent_id        =  c(  NA,             1,              1),
the_seq          =  c(  'AAA',          'AAC',          'AAA'),
n_mut            =  c(  NA_real_,       1,              0),
recomb_pos       =  c(  NA_real_,       NA_real_,       NA_real_),
recomb_replaced  =  c(  NA_character_,  NA_character_,  NA_character_),
recomb_partner   =  c(  NA_real_,       NA_real_,       NA_real_),
recomb_muts      =  c(  NA_real_,       NA_real_,       NA_real_),
fitness_score    =  c(  0.98,           0.99,           0.96),
#==========================================================
stringsAsFactors = FALSE
  )
                           )



test_that("check_fitness_evaluator works", {
  fe <- list(fun = 'fitness_evaluator_uniform_fun',
             args = NULL)
  #debugging
  #TODO:delete
  if (FALSE){
    fun <- fe$fun
    args <- fe$args
  }

  x <- check_fitness_evaluator(fe$fun, fe$args)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})

test_that("assign_fitness works", {
  fe <- list(fun = 'fitness_evaluator_uniform_fun',
             args = NULL)

  x <- check_fitness_evaluator(fe$fun, fe$args)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }

  x <- sim_pop('AAAA', gen_size = 2, n_pop = 15)
  x$fitness_score <- NA_real_
  y <- assign_fitness(x, fe)

  z <- check_genealogy(y)
  for (i in setdiff(names(z), c("n_mut_calc", "all_n_mut"))){
    expect_true(z[[i]], info = i)
  }
  
  y_non_last <- y %>% filter(gen_num != max(gen_num))
  y_last <- y %>% filter(gen_num == max(gen_num))

  expect_true(all(is.na(y_non_last$fitness_score)))
  expect_true(all(!is.na(y_last$fitness_score)))
  expect_true(all(y_last$fitness_score > 0 & y_last$fitness_score < 1))
})

test_that("get_fit_offspring works", {

  c_genea <- SAMPLE_GENEALOGIES[['bif_2gen']]
  x <- get_fit_offspring(c_genea, 0)
  expect_equal(x, c_genea)

  x <- get_fit_offspring(c_genea, 0.999)
  expect_equal(names(x), names(c_genea))
  expect_equal(nrow(x), 0)

  x <- get_fit_offspring(c_genea, 0.989)
  expect_equal(names(x), names(c_genea))
  expect_equal(nrow(x), 0)

  x <- get_fit_offspring(c_genea, 0.979)
  expect_equal(nrow(x), 2)
  expect_true(all(x$fitness_score > 0.979))
  y <- check_genealogy(x)
  for (i in names(y)){
    expect_true(y[[i]], info = paste("get_fit_offspring(c_genea, 0.979)", i, sep = ' '))
  }

  c_genea <- sim_pop(ancestors = paste(rep('A', 90), collapse = ''), gen_size = 2, n_gen = 4)
  c_genea$fitness_score[1:5] <- rep(0.99, 5)
  x <- get_fit_offspring(c_genea, 0.1)
  expect_true(all(x$fitness_score > 0.1))
  y <- check_genealogy(x)
  for (i in names(y)){
    expect_true(y[[i]], info = paste("get_fit_offspring(c_genea, 0.979)", i, sep = ' '))
  }
  
  x2 <- get_fit_offspring(c_genea, 0.2)
  expect_true(all(x2$fitness_score > 0.2))
  expect_true(nrow(x2) <= nrow(x))
  y <- check_genealogy(x2)
  for (i in names(y)){
    expect_true(y[[i]], info = paste("get_fit_offspring(c_genea, 0.979)", i, sep = ' '))
  }
})
