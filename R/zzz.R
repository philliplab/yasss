.onLoad <- function(libname, pkgname){
  YASSS_ERR_MSG <- list(
    N_GEN_N_POP_INF_NULL = "Either n_gen or n_pop must be specified and at least one must be finite",
    N_GEN_N_POP_LESS_ONE = "Neither n_gen nor n_pop may be set to less than one",
    GEN_SIZE_VALID = "r0 must be between 1 and 1e6"
    )
  assign("YASSS_ERR_MSG", YASSS_ERR_MSG, envir = .GlobalEnv)
}
