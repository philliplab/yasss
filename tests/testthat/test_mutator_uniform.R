context("uniform_mutator")

#YASSS_MUTATORS <- list(mutator_uniform = 
#                         list(fun = mutator_uniform_fun,
#                              args = list(mu = 0.01),
#                              checker = c)
#                      )

test_that("uniform_mutator introduces zero mutations on mu=0", {
  fun = mutator_uniform_fun
  args = list(mu = 0)
  parents <- c('AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA',
               'ACGTACGTA',
               paste(rep('AAACCCGGGTTT', 10), sep = '', collapse = ''),
               paste(rep('AAACCCGGGTTT', 1000), sep = '', collapse = ''))

  for (parent in parents){
    args$parent <- parent
    x <- do.call(fun, args)
    expect_equal(x$parent, x$child, info = parent)
    expect_equal(x$mutation_stats$n_mut, 0, info = parent)
  }
})
