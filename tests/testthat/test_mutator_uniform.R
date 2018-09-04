context("uniform_mutator")

fun <- get("mutator_uniform_fun")
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

test_that('mutator_uniform produces mutations when called with 100% mutation rates', {
  same_nuc_parents <- c(paste(rep('A', 60), sep = '', collapse = ''),
                        paste(rep('C', 60), sep = '', collapse = ''),
                        paste(rep('G', 60), sep = '', collapse = ''),
                        paste(rep('T', 60), sep = '', collapse = ''))

  args <- list(mu = 1)

  for (parent in same_nuc_parents){
    parent_letter <- unique(strsplit(parent , '')[[1]])
    stopifnot(length(parent_letter) == 1)
    stopifnot(nchar(parent_letter) == 1)
    args$parent <- parent
    x <- do.call(fun, args)
    expect_false(x$child == parent)
    
    expect_false(parent_letter %in% strsplit(x$child, '')[[1]])

    expect_true(all(strsplit(x$child, '')[[1]] %in% setdiff(c('A', 'C', 'G', 'T'), parent_letter)))
  }
})
