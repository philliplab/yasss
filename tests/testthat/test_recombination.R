context('recombine_gen')

test_that('recombine_gen works', {
  expect_equal(1+1, 2)
})


context('recombine_seqs')

target_seq <- 'AAAAAAAAAA'
recombination_partner_seq <- 'CCCCCCCCCC'
min_dist_to_edge <- 2

test_that('recombine_seqs works', {
  expect_equal(1+1, 2)
  x <- recombine_seqs(target_seq, recombination_partner_seq, 2)
  first_char <- substr(x$recombinant, 1, 1)
  last_char <- substr(x$recombinant, 10, 10)
  expect_equal(length(x), 3)
  if (x$target_on_LHS){
    expect_equal(first_char, 'A')
    expect_equal(last_char, 'C')
  } else {
    expect_equal(first_char, 'C')
    expect_equal(last_char, 'A')
  }

})
