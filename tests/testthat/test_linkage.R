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

link_dat <- list(V1 = V1,
                 V2 = V2,
                 V3 = V3)

i <- 'V1'
i <- 'V2'
i <- 'V3'

test_that('linkage_diseq computes the consensusMatrix correctly', {
  for (i in names(link_dat)){
    c_dat <- link_dat[[i]]
    ld <- linkage_diseq(seqs = c_dat, verbose = FALSE)
    print(ld)
    cmat <- ld$cmat
    expect_equal(cmat, consensusMatrix_character(c_dat))

    max_freq <- ld$max_freq
    expect_equal(max_freq[1:8], c(10, 5, 10, 10, 10, 10, 10, 10))

    max_nuc <- ld$max_nuc
    expect_equal(max_nuc[1:8], rep('A', 8))

    jot <- ld$jot
    expect_equal(class(jot), 'matrix')

    if (i == 'V1'){
      expect_equal(max_nuc[9], 'A')
      expect_equal(jot[2,9], 5)
    } else if (i == 'V2'){
      expect_equal(max_nuc[9], 'A')
      expect_equal(jot[2,9], 0)
    } else if (i == 'V3'){
      expect_equal(max_nuc[9], 'C')
      expect_equal(jot[2,9], 3)
    } else {
      stop('not implemented')
    }
    
    linkages <- ld$linkages
    if (is.null(linkages)){
      next
    }
    expect_equal(class(linkages), 'data.frame')
    expect_true('link_dist' %in% names(linkages))
    expect_true('D_prime' %in% names(linkages))
    if (i == 'V1'){
      expect_equal(linkages[1,2], 1)
    } else if (i == 'V2'){
      expect_equal(linkages[1,2], 0.5)
    } else if (i == 'V3'){
      expect_equal(linkages[1,2], 0)
    } else {
      stop('not implemented')
    }
  }
})


