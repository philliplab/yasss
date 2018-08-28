context("General Mutators")

YASSS_MUTATORS <- list(mutator_uniform = 
                         list(fun = mutator_uniform_fun,
                              args = list(mu = 0.01),
                              checker = c)
                      )

for (c_mutator in names(YASSS_MUTATORS)){
  test_that(paste(c_mutator, "'s general things work", sep = ''), {
    x <- mutator_checks_general(YASSS_MUTATORS[[c_mutator]]$fun,
                                YASSS_MUTATORS[[c_mutator]]$args)
    for (c_result in names(x)){
      expect_true(x[[c_result]], info = c_result)
    }
  })
}

test_that("General Mutator stuff works", {
  expect_equal(1+1, 2)
})

