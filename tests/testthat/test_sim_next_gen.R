context('sim_next_gen')

test_that('sim_next_gen works', {
  x <- sim_next_gen(c('AAA', 'CCC'), 2)
  expect_equal(length(x), 4)
})
