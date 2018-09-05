context('genealogy')

SAMPLE_GENEALOGIES <- list(
  bif_2gen = data.frame(
#==========================================================
gen_num          =  c(  0,              1,              1),
id               =  c(  1,              1,              2),
parent_id        =  c(  NA,             1,              1),
the_seq          =  c(  'AAA',          'AAC',          'AAA'),
n_mut            =  c(  NA_real_,       1,              0),
recomb_pos       =  c(  NA_real_,       NA_real_,       NA_real_),
recomb_replaced  =  c(  NA_character_,  NA_character_,  NA_character_),
recomb_partner   =  c(  NA_real_,       NA_real_,       NA_real_),
recomb_muts      =  c(  NA_real_,       NA_real_,       NA_real_),
fitness_score    =  c(  0.98,           0.99,           0.96),
#==========================================================
stringsAsFactors = FALSE
  )
                           )

genealogy_expector <- function(genealogy, true_list = 'all', false_list = 'none', 
                               ignore_list = NULL, extra_info = NULL,
                               which_checker = 'check_genealogy',
                               prerequisite_result = list()){
  if ('all' %in% true_list & 'all' %in% false_list){
    stop('You cannot expect all columns to be both true and false')
  }
  if (which_checker == 'check_genealogy'){
    y <- get(which_checker)(genealogy = genealogy)
  } else {
    y <- get(which_checker)(genealogy = genealogy, result = prerequisite_result)
  }
  if ('all' %in% true_list){
    # An abomination that allows you to check column's existence by specifying
    # them in the true_list
    non_all_true_list <- true_list[true_list != 'all']
    all_not_false <- setdiff(names(y), false_list)
    true_list <- unique(c(non_all_true_list, all_not_false))
  }
  if ('none' %in% false_list){
    false_list <- NULL
  }
  true_list <- setdiff(true_list, ignore_list)
  false_list <- setdiff(false_list, ignore_list)
  for (i in true_list){
    expect_true(y[[i]], info = trimws(paste(i, extra_info, sep = ' ')))
  }
  for (i in false_list){
    expect_false(y[[i]], info = trimws(paste(i, extra_info, sep = ' ')))
  }
}

test_that('genealogy_expector works', {
  ancestors <- 'AAA'
  x <- data.frame(gen_num = 0,
                  id = 1:length(ancestors),
                  parent_id = NA_real_,
                  the_seq = ancestors,
                  n_mut = NA_real_,
                  recomb_pos = NA_real_,
                  recomb_replaced = NA_character_,
                  recomb_partner = NA_real_,
                  recomb_muts = NA_real_,
                  fitness_score = NA_real_,
                  stringsAsFactors = FALSE
                  )

  expect_error(genealogy_expector(x, true_list = 'bogus_column'), "y\\[\\[i\\]\\] isn't true.\nbogus_column")
  expect_error(genealogy_expector(x, true_list = c('all', 'bogus_column')), "y\\[\\[i\\]\\] isn't true.\nbogus_column")
  
  expect_error(genealogy_expector(x, false_list = 'bogus_column'), "y\\[\\[i\\]\\] isn't false.\nbogus_column")
})

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
    false_list <- c(paste('has', i, sep = '_'), 'number_of_columns', 'column_order', 'all_structure')
    true_list <- setdiff(all_has_columns, false_list)
    
    genealogy_expector(y, true_list = true_list, false_list = false_list,
                       which_checker = "check_genealogy_structure",
                       prerequisite_result = list())
  }
})

test_that('check_genealogy flags issues with incorrect column data types', {
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

    if (class(y[,i]) == "character"){
      y[,i] <- 5.3
      class(y[,i]) <- "numeric"
    } else {
      y[,i] <- "A"
      class(y[,i]) <- "character"
    }
    
    all_class_columns <- paste('class', column_list, sep = '_')
    false_list <- c(paste('class', i, sep = '_'), 'all_structure')
    true_list <- 'all'
    
    genealogy_expector(y, true_list = true_list, false_list = false_list,
                       which_checker = "check_genealogy_structure",
                       prerequisite_result = list())
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
    genealogy_expector(y, false_list = c('number_of_columns', 'column_order', 'all_structure'),
                       which_checker = 'check_genealogy_structure')
  }

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
  y <- cbind(x, x[,3:5])
  y[,3:5] <- NULL
  genealogy_expector(y, false_list = c('column_order', 'all_structure'),
                     which_checker = 'check_genealogy_structure')

})

#  results$gen_num_not_missing <- !any(is.na(genealogy$gen_num) |
#  results$gen_num_naturals <- all(genealogy$gen_num %in% 0:max(genealogy$gen_num)) &

test_that('check_genealogy flags issues with gen_num', {
  c_genea <- SAMPLE_GENEALOGIES[['bif_2gen']]
  m_genea <- c_genea
  m_genea$gen_num[2] <- NA
  genealogy_expector(m_genea, 
                     false_list = c('gen_num_not_missing', 'gen_num_naturals', 'all_gen_num'),
                     extra_info = "gen_num[2] is NA",
                     which_checker = "check_genealogy_gen_num",
                     prerequisite_result = list(has_gen_num = TRUE))

  m_genea <- c_genea
  m_genea$gen_num[2] <- -1
  genealogy_expector(m_genea, false_list = c('gen_num_naturals', 'all_gen_num'),
                     extra_info = "gen_num[2] equals -1",
                     which_checker = "check_genealogy_gen_num",
                     prerequisite_result = list(has_gen_num = TRUE))
  
  m_genea <- c_genea
  m_genea$gen_num[2] <- 5
  genealogy_expector(m_genea, false_list = c('gen_num_naturals', 'all_gen_num'),
                     extra_info = "gen_num[2] equals 5",
                     which_checker = "check_genealogy_gen_num",
                     prerequisite_result = list(has_gen_num = TRUE))
})

test_that('check_genealogy flags issues with ids', {
  c_genea <- SAMPLE_GENEALOGIES[['bif_2gen']]
  m_genea <- c_genea
  m_genea$id[2] <- NA
  genealogy_expector(m_genea, 
                     false_list = c('id_not_missing', 'id_gt_zero', 
                                    'id_no_duplicates_within_gen', 'id_is_integer',
                                    'all_id'),
                     which_checker = 'check_genealogy_id',
                     prerequisite_result = list(has_is = TRUE))

  m_genea <- c_genea
  m_genea$id[2] <- -1
  genealogy_expector(m_genea, 
                     false_list = c('id_gt_zero',
                                    'all_id'),
                     which_checker = 'check_genealogy_id',
                     prerequisite_result = list(has_is = TRUE))
  
  m_genea <- c_genea
  m_genea$id[3] <- 1
  genealogy_expector(m_genea, 
                     false_list = c('id_no_duplicates_within_gen',
                                    'all_id'),
                     which_checker = 'check_genealogy_id',
                     prerequisite_result = list(has_is = TRUE))
  
  m_genea <- c_genea
  m_genea$id[3] <- 1.1
  genealogy_expector(m_genea, 
                     false_list = c('id_is_integer',
                                    'all_id'),
                     which_checker = 'check_genealogy_id',
                     prerequisite_result = list(has_is = TRUE))
})

test_that('check_genealogy flags issues with parent_ids', {
  c_genea <- SAMPLE_GENEALOGIES[['bif_2gen']]
  m_genea <- c_genea
  m_genea$parent_id[2] <- NA
  genealogy_expector(m_genea, 
                     false_list = c('parent_id_after_gen_zero_not_missing', 'parent_id_gt_zero', 
                                    'all_parent_ids_present', 'all_parent_id'),
                     which_checker = 'check_genealogy_parent_id',
                     prerequisite_result = list(has_parent_id = TRUE, 
                                                 has_gen_num = TRUE, 
                                                 gen_num_not_missing = TRUE))

  m_genea <- c_genea
  m_genea$parent_id[2] <- -1
  genealogy_expector(m_genea, 
                     false_list = c('parent_id_gt_zero','all_parent_ids_present', 
                                    'all_parent_id'),
                     which_checker = 'check_genealogy_parent_id',
                     prerequisite_result = list(has_parent_id = TRUE, 
                                                 has_gen_num = TRUE, 
                                                 gen_num_not_missing = TRUE))
})

test_that('check_genealogy flags issues with the_seq', {
  c_genea <- SAMPLE_GENEALOGIES[['bif_2gen']]
  m_genea <- c_genea
  m_genea$the_seq[2] <- NA
  genealogy_expector(m_genea, 
                     false_list = c('the_seq_not_missing', 'the_seq_valid_letters', 
                                    'all_the_seq'),
                     which_checker = 'check_genealogy_the_seq',
                     prerequisite_result = list(has_the_seq = TRUE),
                     extra_info = 'm_genea$the_seq[2] <- NA')

  m_genea <- c_genea
  m_genea$the_seq[1] <- ''
  genealogy_expector(m_genea, 
                     false_list = c('the_seq_not_missing', 'the_seq_valid_letters',
                                    'all_the_seq'),
                     which_checker = 'check_genealogy_the_seq',
                     prerequisite_result = list(has_the_seq = TRUE),
                     extra_info = 'm_genea$the_seq[1] <- ""')

  m_genea <- c_genea
  m_genea$the_seq[3] <- 'W'
  genealogy_expector(m_genea, 
                     false_list = c('the_seq_valid_letters', 'all_the_seq'),
                     which_checker = 'check_genealogy_the_seq',
                     prerequisite_result = list(has_the_seq = TRUE),
                     extra_info = 'm_genea$the_seq[3] <- "W"')

  m_genea <- c_genea
  m_genea$the_seq[2] <- 'AACGTCCGT?'
  genealogy_expector(m_genea, 
                     false_list = c('the_seq_valid_letters', 'all_the_seq'),
                     which_checker = 'check_genealogy_the_seq',
                     prerequisite_result = list(has_the_seq = TRUE),
                     extra_info = 'm_genea$the_seq[2] <- "AACGTCCGT?"')
})

test_that('check_genealogy flags issues with n_mut', {
  c_genea <- SAMPLE_GENEALOGIES[['bif_2gen']]
  m_genea <- c_genea
  m_genea$n_mut[2] <- NA

  genealogy_expector(m_genea, 
                     false_list = c('n_mut_not_missing', 'all_n_mut',
                                    'n_mut_is_integer', 'n_mut_calc'),
                     which_checker = 'check_genealogy_n_mut',
                     prerequisite_result = list(has_the_seq = TRUE),
                     extra_info = 'm_genea$the_seq[2] <- NA')

  m_genea <- c_genea
  m_genea$n_mut[2] <- 1.5

  genealogy_expector(m_genea, 
                     false_list = c('all_n_mut',
                                    'n_mut_is_integer', 'n_mut_calc'),
                     which_checker = 'check_genealogy_n_mut',
                     prerequisite_result = list(has_the_seq = TRUE),
                     extra_info = 'm_genea$the_seq[2] <- 1.5')

  m_genea <- c_genea
  m_genea$n_mut[2] <- 5

  genealogy_expector(m_genea, 
                     false_list = c('all_n_mut', 'n_mut_calc'),
                     which_checker = 'check_genealogy_n_mut',
                     prerequisite_result = list(has_the_seq = TRUE),
                     extra_info = 'm_genea$the_seq[2] <- 5')
})

test_that('make_genealogy works', {
  x <- make_genealogy(ancestors = c('AAA'))
  genealogy_expector(x)
})
