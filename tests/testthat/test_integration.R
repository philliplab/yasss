context("Integration Tests")

test_that("A small simulation with a moderate uniform mutator yields reasonable results", {
  x <- sim_pop(ancestors = paste(rep("A", 60), collapse = ''),
               r0 = 2,
               n_gen = 5,
               n_pop = Inf,
               mutator = list(fun = "mutator_uniform_fun",
                              args = list(mu = 0.1)))

  y <- check_genealogy(x)
  for (i in names(y)){
    expect_true(y[[i]], info = i)
  }

  for (i in nrow(x)){
    y <- table(strsplit(x[i,'the_seq'], '')[[1]])
    expect_true("A" %in% names(y)[y == max(y)])
  }
  #TODO: figure out seeds and then uncomment
#  expect_true(min(x[-1,'n_mut']) > 0)
  expect_true(max(x[-1,'n_mut']) < 25)
})

test_that("sim_pop can be memoised with memoiseCache", {

  x <- sim_pop

  fun <- "sim_pop"
  args <- list(ancestors = paste(rep("A", 60), collapse = ''),
               r0 = 2,
               n_gen = 5,
               n_pop = Inf,
               mutator = list(fun = "mutator_uniform_fun",
                              args = list(mu = 0.1)))

  x <- memoiseCache(fun, args)

  y <- check_genealogy(x)
  for (i in names(y)){
    expect_true(y[[i]], info = i)
  }

  for (i in nrow(x)){
    y <- table(strsplit(x[i,'the_seq'], '')[[1]])
    expect_true("A" %in% names(y)[y == max(y)])
  }
  #TODO: figure out seeds and then uncomment
#  expect_true(min(x[-1,'n_mut']) > 0)
  expect_true(max(x[-1,'n_mut']) < 25)

  z <- memoiseCache(fun, args)
  expect_equal(x, z)
})
