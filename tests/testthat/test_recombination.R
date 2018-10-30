context('recombine_gen')

test_that('recombine_gen works', {
  expect_equal(1+1, 2)
})


context('recombine_seqs')

target_seq <- 'AAAAAAAAAA'
recombination_partner_seq <- 'CCCCCCCCCC'
min_dist_to_edge <- 2

test_that('recombine_seqs works', {
  x <- recombine_seqs(target_seq, recombination_partner_seq, 2)
  expect_equal(class(x), 'list')
  expect_equal(length(x), 4)
  expect_true('recombinant' %in% names(x))
  expect_true('recomb_muts' %in% names(x))
  expect_true('recomb_replaced' %in% names(x))
  expect_true('recomb_pos' %in% names(x))

  first_char <- substr(x$recombinant, 1, 1)
  last_char <- substr(x$recombinant, 10, 10)
  if (x$recomb_replaced == 'right'){
    expect_equal(first_char, 'A')
    expect_equal(last_char, 'C')
  } else {
    expect_equal(first_char, 'C')
    expect_equal(last_char, 'A')
  }

  last_lhs <- substr(x$recombinant, x$recomb_pos, x$recomb_pos)
  first_rhs <- substr(x$recombinant, x$recomb_pos+1, x$recomb_pos+1)
  expect_false(last_lhs == first_rhs)

})
