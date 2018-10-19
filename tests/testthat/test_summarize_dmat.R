context('summarize_dmat')

x <- sim_pop(ancestors = paste(rep("A", 500), sep = '', collapse = ''), 
             r0 = 2,
             n_gen = 6)

y <- x %>% 
  filter(gen_num == max(gen_num)) %>%
  select(the_seq)

y <- y$the_seq
dmat <- stringdistmatrix(y, method = 'hamming')

test_that('summarize_dmat works', {
  result <- summarize_dmat(dmat)
  x <- check_dsum(result)
  for (i in names(x)){
    expect_true(x[[i]], info = i)
  }
})
