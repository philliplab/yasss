context('sim_next_gen')

YASSS_MUTATORS <- list(mutator_uniform = 
                         list(fun = "mutator_uniform_fun",
                              args = list(mu = 0.01))
                      )

make_genealogy <- function(ancestors){
  genealogy <- data.frame(gen_num = 0,
                          id = 1:length(ancestors),
                          parent_id = -1,
                          the_seq = ancestors,
                          n_mut = NA_real_,
                          recomb_pos = NA_real_,
                          recomb_replaced = NA_character_,
                          recomb_partner = NA_real_,
                          recomb_muts = NA_real_,
                          fitness_score = NA_real_,
                          stringsAsFactors = FALSE
                          )
}

test_that('sim_next_gen produces output of the correct format', {
  x <- sim_next_gen(make_genealogy(c('AAA', 'CCC')), 2, 
                    YASSS_MUTATORS$mutator_uniform)
  expect_equal(class(x), 'data.frame')
  expect_true('the_seq' %in% names(x))
  expect_equal(class(x$the_seq[x$gen_num == max(x$gen_num)]), 'character')
})

test_that('sim_next_gen produces the correct number of offspring', {
  
  for (r0 in c(1,2,5,100)){
    x <- sim_next_gen(make_genealogy(c('AAA')), r0, 
                      YASSS_MUTATORS$mutator_uniform)
    expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), r0)
  }
            
  for (r0 in c(1,2,5,100)){
    x <- sim_next_gen(make_genealogy(c('AAA', 'CCC')), r0, 
                      YASSS_MUTATORS$mutator_uniform)
    expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 2*r0)
  }

  for (r0 in c(1,2,5,100)){
    x <- sim_next_gen(make_genealogy(c('AAA', 'CCC', 'GGG', 'TTT', 'ACG')), r0, 
                      YASSS_MUTATORS$mutator_uniform)
    expect_equal(length(x$the_seq[x$gen_num == max(x$gen_num)]), 5*r0)
  }
})

test_that('sim_next_gen produces mutations when called with 100% mutation rates', {
  mutator_uniform_mu100 <- YASSS_MUTATORS$mutator_uniform
  mutator_uniform_mu100$args$mu <- 1

  x <- sim_next_gen(make_genealogy(c('AAA')), r0 = 2,
                    mutator_uniform_mu100)
  expect_false(any(x$the_seq[x$gen_num == max(x$gen_num)] == 'AAA'))
  expect_false('A' %in% strsplit(x$the_seq[x$gen_num == max(x$gen_num)][1], '')[[1]])
  expect_false('A' %in% strsplit(x$the_seq[x$gen_num == max(x$gen_num)][2], '')[[1]])

  expect_true(all(strsplit(x$the_seq[x$gen_num == max(x$gen_num)][1], '')[[1]] %in% c('C', 'G', 'T')))
  expect_true(all(strsplit(x$the_seq[x$gen_num == max(x$gen_num)][2], '')[[1]] %in% c('C', 'G', 'T')))
})
