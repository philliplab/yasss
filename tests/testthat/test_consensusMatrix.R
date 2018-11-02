context("consensusMatrix")

x <- sample(c('A','C','G','T'), 50, replace = TRUE)
x <- matrix(x, ncol = 5, nrow = 10)
seqs <- apply(x, 1, paste, sep = '', collapse = '')

test_that("consensusMatrix works", {
  cm <- consensusMatrix_character(seqs)
  the_rownames <- c("A", "C", "G", "T", "M", "R", "W", "S", "Y", "K", "V", "H",
                    "D", "B", "N", "-", "+", ".")
  expect_equal(attr(cm, "dimnames")[[1]], the_rownames)

  expect_equal(min(apply(cm, 2, sum)), max(apply(cm, 2, sum)))
  expect_equal(min(apply(cm, 2, sum)), 10)

  expect_equal(dim(cm), c(18, 5))

})



