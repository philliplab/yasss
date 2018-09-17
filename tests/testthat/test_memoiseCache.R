context("memoiseCache")

.old.seed <- .Random.seed

test_that("memoiseCache returns the correct result", {
  fun <- "sum"
  args <- list(1:10)

  set.seed(1)
  y <- do.call(fun, args)
  set.seed(1)
  z <- memoiseCache(fun, args)
  expect_equal(y, z)
})

set.seed(.old.seed)
