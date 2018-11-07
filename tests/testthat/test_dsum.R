context('dsum')

if (FALSE){
  yasss:::restart_r()
  devtools::load_all()
  library(testthat)
}

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
              sd_hd = sd(dmat),
              perc = z,
              dens = density(dmat)
              )

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

dsum5_1 <- dsum2
dsum5_1$sampling <- c('none', 'fit_threshold')

dsum6 <- dsum2
dsum6$sim_id <- 'A'

dsum7 <- dsum2
dsum7$avg_hd <- 'A'

dsum7_1 <- dsum2
dsum7_1$avg_hd <- 50:51

fake_perc <- runif(101)*50
sorted_fake_perc <- sort(fake_perc)
while (all(fake_perc == sorted_fake_perc)){
  fake_perc <- sample(fake_perc)
}
dsum8 <- dsum2
dsum8$perc <- sorted_fake_perc[1:100]

dsum9 <- dsum2
dsum9$perc <- fake_perc

dsum9_1 <- dsum2
dsum9_1$perc <- as.character(dsum2$perc)

dsum10 <- dsum2
dsum10$dens <- matrix(rnorm(100), ncol = 10)

dsum11 <- dsum2
dsum11$perc <- NULL

dsum11_1 <- dsum2
dsum11_1$sim_id <- NULL

dsum12 <- dsum2
dsum12$sim_id <- NULL

dsum13 <- dsum2
dsum13$bad_element <- 'is bad'

dsum14 <- dsum2
dsum14$sim_id <- 1:2

dsum15 <- dsum2
dsum15$label <- c('A', 'B')

dsum16 <- dsum2
dsum16$sd_hd <- 'A'

dsum16_1 <- dsum2
dsum16_1$sd_hd <- 50:51


if (FALSE){
  library(yasss)
  library(testthat)
}

test_that('check_dsum passes on correct dsums', {
  result <- check_dsum(dsum1)
  expect_equal(class(result), 'list')
  for (check_name in names(result)){
    expect_true(result[[check_name]], info = check_name)
  }
  expect_false('sim_id_exists' %in% names(result))
  expect_false('label_exists' %in% names(result))
  expect_false('sampling_exists' %in% names(result))
  
  result <- check_dsum(dsum2)
  expect_equal(class(result), 'list')
  for (check_name in names(result)){
    expect_true(result[[check_name]], info = check_name)
  }
  expect_false('sim_id_exists' %in% names(result))
  expect_false('label_exists' %in% names(result))
  expect_false('sampling_exists' %in% names(result))
  
  result <- check_dsum(dsum2, identifiers = TRUE)
  expect_equal(class(result), 'list')
  for (check_name in names(result)){
    expect_true(result[[check_name]], info = check_name)
  }
  expect_true('sim_id_exists' %in% names(result))
  expect_true('label_exists' %in% names(result))
  expect_true('sampling_exists' %in% names(result))
  
  result <- check_dsum(dsum3, identifiers = TRUE)
  expect_equal(class(result), 'list')
  for (check_name in names(result)){
    expect_true(result[[check_name]], info = check_name)
  }

  result <- check_dsum(dsum4, identifiers = TRUE)
  expect_equal(class(result), 'list')
  for (check_name in names(result)){
    expect_true(result[[check_name]], info = check_name)
  }
})

test_that('check_dsum find violations', {
  result <- check_dsum(dsum11, identifiers = TRUE)
  expect_false(result$has_req_elements)
  
  result <- check_dsum(dsum11_1, identifiers = TRUE)
  expect_false(result$has_req_elements)

  result <- check_dsum(dsum13, identifiers = TRUE)
  expect_false(result$only_valid_columns)
  
  result <- check_dsum(dsum14, identifiers = TRUE)
  expect_false(result$sim_id_length_one)
  
  result <- check_dsum(dsum6, identifiers = TRUE)
  expect_false(result$sim_id_integer)

  result <- check_dsum(dsum5, identifiers = TRUE)
  expect_false(result$sampling_valid)

  result <- check_dsum(dsum5_1, identifiers = TRUE)
  expect_false(result$sampling_length_one)

  result <- check_dsum(dsum8, identifiers = TRUE)
  expect_false(result$perc_length_101)

  result <- check_dsum(dsum9, identifiers = TRUE)
  expect_false(result$perc_is_sorted)
  
  result <- check_dsum(dsum9_1, identifiers = TRUE)
  expect_false(result$perc_is_numeric)
  
  result <- check_dsum(dsum7, identifiers = TRUE)
  expect_false(result$avg_hd_is_numeric)
  
  result <- check_dsum(dsum7_1, identifiers = TRUE)
  expect_false(result$avg_hd_length_one)
  
  result <- check_dsum(dsum10, identifiers = TRUE)
  expect_false(result$dens_is_density)
  
  result <- check_dsum(dsum16, identifiers = TRUE)
  expect_false(result$sd_hd_is_numeric)
  
  result <- check_dsum(dsum16_1, identifiers = TRUE)
  expect_false(result$sd_hd_length_one)
  
})

dcol1 <- list(dsum2, dsum3, dsum4)
dcol2 <- list()
dcol3 <- 'a'
dcol4 <- c(dcol2, 'a')
dcol5 <- dcol1
names(dcol5) <- paste('got_a_name_', 1:length(dcol5), sep = '')

test_that('check_dcollection let correct dcollections pass', {
  result <- check_dcollection(dcol1)
  for (check_name in names(result)){
    expect_true(result[[check_name]], info = check_name)
  }
})

test_that('check_dcollection catches issues', {
  result <- check_dcollection(dcol2)
  expect_false(result[['length_gt_zero']])
  
  result <- check_dcollection(dcol3)
  expect_false(result[['is_list']])

  result <- check_dcollection(dcol4)
  expect_false(result[['all_valid_dsums']])

  result <- check_dcollection(dcol5)
  expect_false(result[['unnamed']])
})

dsum17 <- dsum2
dsum17$clara2 <- list(
  avg_within_cluster = 50,
  avg_between_cluster = 100,
  cluster_sizes = c(10, 10))
dcol2 <- c(dcol1, list(dsum17))

test_that('dcollection_to_df works', {
  x <- dcollection_to_df(dcol1)
  expect_true(class(x) == 'list')
  expect_true('dmat_metrics' %in% names(x))
  expect_true('dmat_distribution_df' %in% names(x))
  
  expect_equal(class(x$dmat_metrics), 'data.frame')
  dmat_metrics_names <- c('sim_id', 'label', 'sampling', 
    'group_label', 'uniq_id', 'metric', 'value')
  for (col_name in dmat_metrics_names){
    expect_true(col_name %in% names(x$dmat_metrics))
  }


  x <- dcollection_to_df(dcol2)
  expect_true(class(x) == 'list')
  expect_true('dmat_metrics' %in% names(x))
  expect_true('dmat_distribution_df' %in% names(x))
  expect_true('dmat_clara2_df' %in% names(x))
  clara2_metrics <- c(
        'avg_within_cluster',
        'avg_between_cluster',
        'cluster_size_ratio',
        'within_between_ratio',
        'smallest_cluster'
      )
  for (i in clara2_metrics){
    expect_true(i %in% x$dmat_clara2_df$metric, info = i)
  }
})

test_that('clara2 clustering in summarize_dmat works', {
  x <- sim_pop(ancestors = c(paste(rep("A", 500), sep = '', collapse = ''), 
                             paste(rep("C", 500), sep = '', collapse = '')),
               r0 = 2,
               n_gen = 5)
  
  y <- x %>% 
    filter(gen_num == max(gen_num)) %>%
    select(the_seq)
  
  dsum <- summarize_dmat(stringdistmatrix(y$the_seq, method = 'hamming'))
  z <- check_dsum(dsum)
  for (i in names(z)){
    expect_true(z[[i]], info = i)
  }
  expect_true(dsum$clara2$avg_within_cluster < 80)
  expect_true(dsum$clara2$avg_between_cluster > 400)
  expect_equal(dsum$clara2$cluster_sizes, c(32, 32))
})

