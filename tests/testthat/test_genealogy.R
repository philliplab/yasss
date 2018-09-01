context('genealogy')

genealogy_expector <- function(genealogy){
  y <- check_genealogy(genealogy)
  for (i in names(y)){
    expect_true(y[[i]], info = i)
  }
}

test_that('check_genealogy works', {
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
})

test_that('make_genealogy works', {
  x <- make_genealogy(ancestors = c('AAA'))
  genealogy_expector(x)
})
