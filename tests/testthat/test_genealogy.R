context('genealogy')

SAMPLE_GENEALOGIES <- list(
  bif_2gen = data.frame(
#==========================================================
gen_num          =  c(     0,      1,      1),
id               =  c(     1,      1,      2),
parent_id        =  c(     NA,     1,      1),
the_seq          =  c(     'AAA',  'AAC',  'AAA'),
n_mut            =  c(     NA,     1,      0),
recomb_pos       =  c(     NA,     NA,     NA),
recomb_replaced  =  c(     NA,     NA,     NA),
recomb_partner   =  c(     NA,     NA,     NA),
recomb_muts      =  c(     NA,     NA,     NA),
fitness_score    =  c(     0.95,   0.99,   0.96),
#==========================================================
stringsAsFactors = FALSE
  )
                           )

genealogy_expector <- function(genealogy, true_list = 'all', false_list = 'none', extra_info = ''){
  if ('all' %in% true_list & 'all' %in% false_list){
    stop('You cannot expect all columns to be both true and false')
  }
  if ('all' %in% true_list){
    true_list <- setdiff(names(genealogy), false_list)
  }
  if ('none' %in% false_list){
    false_list <- NULL
  }
  y <- check_genealogy(genealogy)
  for (i in true_list){
    expect_true(y[[i]], info = paste(i, extra_info, sep = ' '))
  }
  for (i in false_list){
    expect_false(y[[i]], info = paste(i, extra_info, sep = ' '))
  }
}

test_that('check_genealogy lets correct genealogies pass', {
  ancestors <- 'AAA'
  x <- data.frame(gen_num = 0,
                  id = 1:length(ancestors),
                  parent_id = -1,
                  the_seq = ancestors,
                  n_mut = NA_real_,
                  recomb_pos = NA_real_,
                  recomb_replaced = NA_character_,
                  recomb_partner = NA_real_,
                  recomb_muts = NA_real_,
                  fitness_score = NA_real_,
                  stringsAsFactors = FALSE
                  )
  genealogy_expector(x)

  ancestors <- c('AAA', 'GGG', 'TTT')
  x <- data.frame(gen_num = 0,
                  id = 1:length(ancestors),
                  parent_id = -1,
                  the_seq = ancestors,
                  n_mut = NA_real_,
                  recomb_pos = NA_real_,
                  recomb_replaced = NA_character_,
                  recomb_partner = NA_real_,
                  recomb_muts = NA_real_,
                  fitness_score = NA_real_,
                  stringsAsFactors = FALSE
                  )
  genealogy_expector(x)

  for (c_name in names(SAMPLE_GENEALOGIES)){
    c_genea <- SAMPLE_GENEALOGIES[[c_name]]
    genealogy_expector(c_genea, extra_info = c_name)
  }
})

test_that('check_genealogy flags issues with missing columns', {
  ancestors <- 'AAA'
  x <- data.frame(gen_num = 0,
                  id = 1:length(ancestors),
                  parent_id = -1,
                  the_seq = ancestors,
                  n_mut = NA_real_,
                  recomb_pos = NA_real_,
                  recomb_replaced = NA_character_,
                  recomb_partner = NA_real_,
                  recomb_muts = NA_real_,
                  fitness_score = NA_real_,
                  stringsAsFactors = FALSE
                  )
  column_list <- c("gen_num", "id", "parent_id", "the_seq", "n_mut",
                   "recomb_pos", "recomb_replaced", "recomb_partner",
                   "recomb_muts", "fitness_score")
  for (i in names(x)){


    y <- x
    y[,i] <- NULL
    
    all_has_columns <- paste('has', column_list, sep = '_')
    false_list <- c(paste('has', i, sep = '_'), 'number_of_columns', 'column_order')
    true_list <- setdiff(all_has_columns, false_list)
    
    genealogy_expector(y, true_list = true_list, false_list = false_list)
  }
})

test_that('check_genealogy flags issues with unexpected columns', {
  ancestors <- 'AAA'
  x <- data.frame(gen_num = 0,
                  id = 1:length(ancestors),
                  parent_id = -1,
                  the_seq = ancestors,
                  n_mut = NA_real_,
                  recomb_pos = NA_real_,
                  recomb_replaced = NA_character_,
                  recomb_partner = NA_real_,
                  recomb_muts = NA_real_,
                  fitness_score = NA_real_,
                  stringsAsFactors = FALSE
                  )
  for (i in c('col1', '1', '__A', '  ')){
    y <- x
    y[,i] <- 1
    genealogy_expector(y, false_list = c('number_of_columns', 'column_order'))
  }
})

#  results$gen_num_not_missing <- !any(is.na(genealogy$gen_num) |
#  results$gen_num_naturals <- all(genealogy$gen_num %in% 0:max(genealogy$gen_num)) &

test_that('check_genealogy flags issues with gen_num', {
  c_genea <- SAMPLE_GENEALOGIES[['bif_2gen']]
  m_genea <- c_genea
  m_genea$gen_num[2] <- NA
  genealogy_expector(m_genea, false_list = c('gen_num_not_missing', 'gen_num_naturals'))

  m_genea <- c_genea
  m_genea$gen_num[2] <- -1
  genealogy_expector(m_genea, false_list = c('gen_num_naturals'))
  
  m_genea <- c_genea
  m_genea$gen_num[2] <- 5
  genealogy_expector(m_genea, false_list = c('gen_num_naturals'))
})

test_that('make_genealogy works', {
  x <- make_genealogy(ancestors = c('AAA'))
  genealogy_expector(x)
})
