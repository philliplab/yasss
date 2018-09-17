context("memoiseCache")

.old.seed <- .Random.seed

test_that("memoiseCache checks arguments correctly", {
  expect_error(memoiseCache(1, list(x=1)), "fun must be character or a function of length 1")
  expect_error(memoiseCache(c("sum", "mean"), list(x=1)), "fun must be character or a function")
  expect_error(memoiseCache(sum, 1), "args must be a list")
  expect_error(memoiseCache("sum", "name"), "args must be a list")
  expect_error(memoiseCache("rnorm", list(n = 100, mean = 10, sd = 5), cacheNamePrefix = 51),
               "cacheNamePrefix must be a character of length 1 or NULL")
  expect_error(memoiseCache("rnorm", list(n = 100, mean = 10, sd = 5), 
                            cacheNamePrefix = c("abc", "def")),
               "cacheNamePrefix must be a character of length 1 or NULL")
  expect_error(memoiseCache("rnorm", list(n = 100, mean = 10, sd = 5), seed = "name"),
               "seed must be a positive integer or NULL")
  expect_error(memoiseCache("rnorm", list(n = 100, mean = 10, sd = 5), seed = -1),
               "seed must be a positive integer or NULL")
  expect_error(memoiseCache("rnorm", list(n = 100, mean = 10, sd = 5), seed = 0),
               "seed must be a positive integer or NULL")
  expect_error(memoiseCache("rnorm", list(n = 100, mean = 10, sd = 5), seed = 5.5),
               "seed must be a positive integer or NULL")
})

test_that("memoiseCache returns the correct result", {
  fun <- "sum"
  args <- list(1:10)

  set.seed(1)
  y <- do.call(fun, args)
  set.seed(1)
  z <- memoiseCache(fun, args)
  expect_equal(y, z)
})

test_that("memoiseCache reads the result from the cache", {
  fun <- "rnorm"
  args <- list(n = 10, mean = 2)
  x <- memoiseCache(fun, args)
  y <- memoiseCache(fun, args)
  z <- memoiseCache(fun, args)
  expect_equal(x, y)
  expect_equal(x, z)
})

test_that("seed argument works", {
  fun <- "rnorm"
  args <- list(n = 10, mean = 2)
  x <- memoiseCache(fun, args, seed = 1)
  y <- memoiseCache(fun, args, seed = 2)
  z <- memoiseCache(fun, args, seed = 2)
  expect_true(x != y)
  expect_equal(y, z)
})

set.seed(.old.seed)
