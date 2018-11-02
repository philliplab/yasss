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

i <- V1

test_that('linkage_diseq computes the consensusMatrix correctly', {
  for (i in list(V1, V2, V3)){
    ld <- linkage_diseq(seqs = i, verbose = FALSE)
    cmat <- ld$cmat
    expect_equal(cmat, consensusMatrix_character(i))

    max_freq <- ld$max_freq
    expect_equal(max_freq[1:8], c(10, 5, 10, 10, 10, 10, 10, 10))

    max_nuc <- ld$max_nuc
    expect_equal(max_nuc[1:8], rep('A', 8))
    if (cmat['C',9] == 6){
      expect_equal(max_nuc[9], 'C')
    } else {
      expect_equal(max_nuc[9], 'A')
    }
  }
})


