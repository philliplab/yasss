context('sim_next_gen')

YASSS_MUTATORS <- list(mutator_uniform = 
                         list(fun = mutator_uniform_fun,
                              args = list(mu = 0.01))
                      )

test_that('sim_next_gen produces output of the correct format', {
  x <- sim_next_gen(c('AAA', 'CCC'), 2, 
                    YASSS_MUTATORS$mutator_uniform)
  expect_equal(class(x), 'data.frame')
  expect_true('the_seq' %in% names(x))
  expect_equal(class(x$the_seq), 'character')
})

test_that('sim_next_gen produces the correct number of offspring', {
  
  for (gen_size in c(1,2,5,100)){
    x <- sim_next_gen(c('AAA'), gen_size, 
                      YASSS_MUTATORS$mutator_uniform)
    expect_equal(length(x$the_seq), gen_size)
  }
            
  for (gen_size in c(1,2,5,100)){
    x <- sim_next_gen(c('AAA', 'CCC'), gen_size, 
                      YASSS_MUTATORS$mutator_uniform)
    expect_equal(length(x$the_seq), 2*gen_size)
  }

  for (gen_size in c(1,2,5,100)){
    x <- sim_next_gen(c('AAA', 'CCC', 'GGG', 'TTT', 'ACG'), gen_size, 
                      YASSS_MUTATORS$mutator_uniform)
    expect_equal(length(x$the_seq), 5*gen_size)
  }
})

test_that('sim_next_gen produces mutations when called with 100% mutation rates', {
  mutator_uniform_mu100 <- YASSS_MUTATORS$mutator_uniform
  mutator_uniform_mu100$args$mu <- 1

  x <- sim_next_gen(c('AAA'), gen_size = 2,
                    mutator_uniform_mu100)
  expect_false(any(x$the_seq == 'AAA'))
  expect_false('A' %in% strsplit(x$the_seq[1], '')[[1]])
  expect_false('A' %in% strsplit(x$the_seq[2], '')[[1]])

  expect_true(all(strsplit(x$the_seq[1], '')[[1]] %in% c('C', 'G', 'T')))
  expect_true(all(strsplit(x$the_seq[2], '')[[1]] %in% c('C', 'G', 'T')))
})
