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

V4 <- link_dat
V4[3:9,9] <- 'C'
V4 <- apply(V4, 1, paste, collapse = '')

link_dat <- list(V1 = V1,
                 V2 = V2,
                 V3 = V3,
                 V4 = V4)

i <- 'V1'
i <- 'V2'
i <- 'V3'
i <- 'V4'

test_that('linkage_diseq checks arguments correctly', {
  expect_error(ld <- linkage_diseq(seqs = 2:5),
               'seqs must be of class character')

  expect_error(ld <- linkage_diseq(seqs = 'AAAACCCCCAAAGT'),
               'At least two sequences must be provided')
})

test_that('linkage_diseq computes the consensusMatrix correctly', {
  for (i in names(link_dat)){
    c_dat <- link_dat[[i]]
    ld <- linkage_diseq(seqs = c_dat, verbose = FALSE)
    rld <- linkage_diseq(seqs = rev(c_dat), verbose = FALSE)
    sld <- linkage_diseq(seqs = sample(c_dat), verbose = FALSE)
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
    } else if (i == 'V4'){
      expect_equal(max_nuc[9], 'C')
      expect_equal(jot[2,9], 4)
    } else {
      stop('not implemented')
    }
    
    linkages <- ld$linkages
    if (is.null(linkages)){
      next
    }
    rlinkages <- rld$linkages
    slinkages <- sld$linkages
    expect_equal(class(linkages), 'data.frame')
    expect_true('link_dist' %in% names(linkages))
    expect_true('D_prime' %in% names(linkages))
    if (i == 'V1'){
      expect_equal(linkages[1,2], 1)
      expect_equal(rlinkages[1,2], 1)
      expect_equal(slinkages[1,2], 1)
    } else if (i == 'V2'){
      expect_equal(linkages[1,2], 1)
      expect_equal(rlinkages[1,2], 1)
      expect_equal(slinkages[1,2], 1)
    } else if (i == 'V3'){
      expect_equal(linkages[1,2], 0)
      expect_equal(rlinkages[1,2], 0)
      expect_equal(slinkages[1,2], 0)
    } else if (i == 'V4'){
      expect_equal(linkages[1,2], 1/3)
      expect_equal(rlinkages[1,2], 1/3)
      expect_equal(slinkages[1,2], 1/3)
    } else {
      stop('not implemented')
    }
  }
})


