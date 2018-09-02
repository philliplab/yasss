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

genealogy_expector <- function(genealogy, expect_false = NULL, extra_info = ''){
  y <- check_genealogy(genealogy)
  for (i in setdiff(names(y), expect_false)){
    expect_true(y[[i]], info = paste(i, extra_info, sep = ' '))
  }
  for (i in expect_false){
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
  for (i in names(x)){
    y <- x
    y[,i] <- NULL
    genealogy_expector(y, expect_false = c(paste('has', i, sep = '_'), 
                                           'number_of_columns', 'column_order'))
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
    genealogy_expector(y, expect_false = c('number_of_columns', 'column_order'))
  }
})

test_that('check_genealogy flags issues with gen_num', {
  expect_equal(1+1,2)

#  genealogy_expector(y, expect_false = c('gen_num may not be missing'))

#  genealogy_expector(y, expect_false = c('gen_num must contain each integer between zero and max(gen_num)'))
})

test_that('make_genealogy works', {
  x <- make_genealogy(ancestors = c('AAA'))
  genealogy_expector(x)
})
