context('dsum')

x <- sim_pop(ancestors = paste(rep("A", 500), sep = '', collapse = ''), 
             r0 = 2,
             n_gen = 6)

y <- x %>% 
  filter(gen_num == max(gen_num)) %>%
  select(the_seq)

y <- y$the_seq
dmat <- stringdistmatrix(y, method = 'hamming')
z <- quantile(dmat, (0:100)/100)
names(z) <- NULL

dsum1 <- list(avg_hd = mean(dmat),
              perc = z,
              dens = density(dmat))

if (FALSE){
  library(yasss)
  library(testthat)
}

test_that('check_dsum works', {
  result <- check_dsum(dsum1)
  expect_equal(class(result), 'list')
  for (check_name in names(result)){
    expect_true(result[[check_name]], info = check_name)
  }
})
