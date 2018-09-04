context("Integration Tests")

test_that("A small simulation with a moderate uniform mutator yields reasonable results", {
  x <- sim_pop(ancestors = paste(rep("A", 60), collapse = ''),
               gen_size = 2,
               n_gen = 5,
               n_pop = Inf,
               mutator = list(fun = "mutator_uniform_fun",
                              args = list(mu = 0.1)))

  expect_true(all(unlist(check_genealogy(x))))
  for (i in nrow(x)){
    y <- table(strsplit(x[i,'the_seq'], '')[[1]])
    expect_true("A" %in% names(y)[y == max(y)])
  }
  expect_true(min(x[-1,'n_mut']) > 0)
  expect_true(max(x[-1,'n_mut']) < 20)
})

