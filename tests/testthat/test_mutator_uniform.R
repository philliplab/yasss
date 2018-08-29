context("uniform_mutator")

fun <- mutator_uniform_fun
parents <- c('AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA',
             'ACGTACGTA',
             paste(rep('AAACCCGGGTTT', 10), sep = '', collapse = ''),
             paste(rep('AAACCCGGGTTT', 1000), sep = '', collapse = ''))

# debugging setting
parent <- parents[3]

test_that("uniform_mutator introduces zero mutations on mu=0", {
  args <- list(mu = 0)

  for (parent in parents){
    args$parent <- parent
    x <- do.call(fun, args)
    expect_equal(x$parent, x$child, info = parent)
    expect_equal(x$mutation_stats$n_mut, 0, info = parent)
    observed_mutations <- stringdist(x$parent, x$child, method = 'hamming')
    expect_equal(observed_mutations, x$mutation_stats$n_mut)
  }
})

test_that("uniform_mutator introduces reasonable amount of mutations on mu=0.05", {
  args <- list(mu = 0.05)

  for (parent in parents){
    args$parent <- parent
    x <- do.call(fun, args)
    observed_mutations <- stringdist(x$parent, x$child, method = 'hamming')
    expect_equal(observed_mutations, x$mutation_stats$n_mut)
  }

  args$parent <- paste(rep('AAACCCGGGTTT', 1000), sep = '', collapse = '')
  max_mut <- 750 #computed with max(rbinom(1e10, 12000, 0.05)) + small offset
  min_mut <- 450
  
  for (i in 1:10){
    x <- do.call(fun, args)
    expect_lt(x$mutation_stats$n_mut, max_mut)
    expect_gt(x$mutation_stats$n_mut, min_mut)
  }
})

