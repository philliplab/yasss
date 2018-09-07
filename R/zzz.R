.onLoad <- function(libname, pkgname){
  YASSS_ERR_MSG <- list(
    N_GEN_N_POP_INF_NULL = "Either n_gen or n_pop must be specified and at least one must be finite",
    N_GEN_N_POP_LESS_ONE = "Neither n_gen nor n_pop may be set to less than one",
    GEN_SIZE_VALID = "r0 must be between 1 and 1e6"
    )
  assign("YASSS_ERR_MSG", YASSS_ERR_MSG, envir = .GlobalEnv)

  YASSS_DATASETS <- list(
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
  ),
  ances_only_1 = data.frame(
#==========================================================
gen_num = 0,
id = 1,
parent_id = NA_real_,
the_seq = 'AAA',
n_mut = NA_real_,
recomb_pos = NA_real_,
recomb_replaced = NA_character_,
recomb_partner = NA_real_,
recomb_muts = NA_real_,
fitness_score = NA_real_,
#==========================================================
stringsAsFactors = FALSE
  ),
  ances_only_3 = data.frame(
#==========================================================
gen_num = 0,
id = 1:3,
parent_id = NA_real_,
the_seq = c('AAA', 'CCC', 'GGG'),
n_mut = NA_real_,
recomb_pos = NA_real_,
recomb_replaced = NA_character_,
recomb_partner = NA_real_,
recomb_muts = NA_real_,
fitness_score = NA_real_,
#==========================================================
stringsAsFactors = FALSE
  )
)
  assign("YASSS_DATASETS", YASSS_DATASETS, envir = .GlobalEnv)
}
