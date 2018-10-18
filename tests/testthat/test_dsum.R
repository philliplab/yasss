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

dsum2 <- dsum1
dsum2$sim_id <- 1
dsum2$label <- 'A'
dsum2$sampling <- 'fit_threshold'

dsum3 <- dsum2
dsum3$sampling <- 'size_matched_sampling'

dsum4 <- dsum2
dsum4$sampling <- 'none'

dsum5 <- dsum2
dsum5$sampling <- 'this is wrong'

dsum6 <- dsum2
dsum6$sim_id <- 'A'

dsum7 <- dsum2
dsum7$avg_hd <- 'A'

fake_perc <- runif(101)*50
sorted_fake_perc <- sort(fake_perc)
while (all(fake_perc == sorted_fake_perc)){
  fake_perc <- sample(fake_perc)
}
dsum8 <- dsum2
dsum8$perc <- sorted_fake_perc[1:100]

dsum9 <- dsum2
dsum9$perc <- fake_perc

dsum10 <- dsum2
dsum10$dens <- matrix(rnorm(100), ncol = 10)

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
