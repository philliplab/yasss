context('linkage_diseq')

link_dat <- matrix('A', 10, 10)
link_dat[1:5,2] <- 'C'

V1 <- link_dat
V1[1:5,9] <- 'C'
V1 <- apply(V1, 1, paste, collapse = '')

V2 <- link_dat
V2[6:10,9] <- 'C'
V2 <- apply(V2, 1, paste, collapse = '')

V3 <- link_dat
V3[3:8,9] <- 'C'
V3 <- apply(V3, 1, paste, collapse = '')

test_that('linkage_diseq computes the consensusMatrix correctly', {
  for (i in list(V1, V2, V3)){
    cm <- linkage_diseq(i, verbose = FALSE)[['cm']]
    expect_equal(cm, consensusMatrix_character(i))
  }
})


