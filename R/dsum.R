#' Summarize distance matrix
#'
#' Summarizes a distance matrix into a basic set of metrics. The full list of
#' metrics is described in the section detailing the return value of this
#' function. Additionally, the distance matrix is divided into two clusters
#' using the \code{clara} function. Using this clustering, the within and
#' between cluster distances are computed and added into the final result.
#'
#' @return A list with the following elements: 
#' \itemize{ 
#'   \item avg_hd The average of the pairwise distances in the distance matrix.
#'   \item sd_hd The standard deviation of the pairwise distances in the distance matrix.
#'   \item perc The percentiles of the distribution of the pairwise distances.
#'   \item dens The result obtained when calling \code{density} on the distribution.
#'   \item clara2 The summary of the metrics related to the clustering of the distance matrix.
#' } 
#'
#' @param dmat A distance
#' matrix, or any set of observations from a distribution really.  @export

summarize_dmat <- function(dmat) {
  avg_hd <- mean(dmat)
  sd_hd <- sd(dmat)
  perc <- quantile(dmat, (0:100)/100)
  dens <- density(dmat)

  dmat <- as.matrix(dmat)

  ## Using clara with number of clusters equal to 2
  clusts <- clara(dmat, 2)
  cluster1 <- which(clusts$clustering==1)
  cluster2 <- which(clusts$clustering==2)

  within_cluster <- c(as.vector(dmat[cluster1, cluster1]), as.vector(dmat[cluster2, cluster2]))
  between_cluster <- c(as.vector(dmat[cluster1, cluster2]), as.vector(dmat[cluster2, cluster1]))

  avg_within_cluster <- mean(within_cluster)
  avg_between_cluster <- mean(between_cluster)
  cluster_sizes <- c(length(cluster1), length(cluster2))
  
  clara2 <- list(
    avg_within_cluster  = avg_within_cluster,
    avg_between_cluster = avg_between_cluster,
    cluster_sizes       = cluster_sizes
  )

  return(list(avg_hd = avg_hd,
              sd_hd = sd_hd,
              perc = perc,
              dens = dens,
              clara2 = clara2))
}

#' Checks a dsum list
#'
#' Checks that a dsum list adheres to the specifications
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param dsum The dsum to check
#' @param identifiers If TRUE, then the dsum will be checked to ensure that
#' the identifier elements are also present.
#' @export

check_dsum <- function(dsum, identifiers = FALSE){
  result <- list()
  result[['is_a_list']] <- class(dsum) == 'list'
  col_names <- c('avg_hd', 'sd_hd', 'perc', 'dens')
  identifier_col_names <- c('sim_id', 'label', 'sampling')
  optional_cols <- c('clara2')

  for (col_name in col_names){
    result_label <- paste(col_name, 'exists', sep = '_')
    result[[result_label]] <- col_name %in% names(dsum)
    if (!result[[result_label]]){
      result[['has_req_elements']] <- FALSE
      return(result)
    }
  }

  if (identifiers){
    for (col_name in identifier_col_names){
      result_label <- paste(col_name, 'exists', sep = '_')
      result[[result_label]] <- col_name %in% names(dsum)
      if (!result[[result_label]]){
        result[['has_req_elements']] <- FALSE
        return(result)
      }
    }

    # sim_id
    result[['sim_id_length_one']] <- length(dsum$sim_id) == 1

    result[['sim_id_integer']] <- class(dsum$sim_id) %in% c('numeric', 'integer')
    if (result[['sim_id_integer']] & result[['sim_id_length_one']]){
      result[['sim_id_integer']] <- floor(dsum$sim_id) == ceiling(dsum$sim_id)
    } else {
      result[['sim_id_integer']] <- FALSE
    }
  
    # label
    result[['label_length_one']] <- length(dsum$label) == 1

    # sampling
    result[['sampling_length_one']] <- length(dsum$sampling) == 1
    result[['sampling_valid']] <- dsum$sampling %in% c('fit_threshold', 'size_matched_sampling', 'none')
  }

  # avg_hd
  result[['avg_hd_is_numeric']] <- class(dsum$avg_hd) == 'numeric'
  result[['avg_hd_length_one']] <- length(dsum$avg_hd) == 1

  # sd_hd
  result[['sd_hd_is_numeric']] <- class(dsum$sd_hd) == 'numeric'
  result[['sd_hd_length_one']] <- length(dsum$sd_hd) == 1

  # perc
  result[['perc_length_101']] <- length(dsum$perc) == 101
  result[['perc_is_sorted']] <- all(dsum$perc == sort(dsum$perc))
  result[['perc_is_numeric']] <- class(dsum$perc) == 'numeric'

  # density
  result[['dens_is_density']] <- class(dsum$dens) == 'density'

  only_valid_columns <- TRUE
  for (element_name in names(dsum)){
    only_valid_columns <- only_valid_columns & element_name %in% c(col_names, identifier_col_names, optional_cols)
  }
  result[['only_valid_columns']] <- only_valid_columns

  return(result)
}

#' Checks a dcollection list
#'
#' Checks that a dcollection list adheres to the specifications
#'
#' @return A list with TRUE or FALSE indicating whether the related check
#' passed.
#' @param dsum The dsum to check
#' @param identifiers If TRUE, then the dsum will be checked to ensure that
#' the identifier elements are also present.
#' @export

check_dcollection <- function(dcollection){
  result <- list()

  # class list
  result[['is_list']] <- class(dcollection) == 'list'

  if (!result[['is_list']]){
    return(result)
  }

  # length > 0
  result[['length_gt_zero']] <- length(dcollection) > 0
  if (!result[['length_gt_zero']]){
    return(result)
  }

  # each element is dsum with identifiers
  all_valid_dsums <- TRUE
  identifiers <- NULL
  for (i in 1:length(dcollection)){
    x <- check_dsum(dcollection[[i]], identifiers = TRUE)
    c_dsum_valid <- all(unlist(x))
    if (c_dsum_valid){
      identifiers <- c(identifiers, paste(dcollection[[i]]$sim_id,
                                          dcollection[[i]]$label,
                                          dcollection[[i]]$sampling,
                                          sep = '_'))
    } else {
      c_dsum_valid <- FALSE
    }
    all_valid_dsums <- all_valid_dsums & c_dsum_valid
  }
  result[['all_valid_dsums']] <- all_valid_dsums

  # unnamed
  result[['unnamed']] <- is.null(names(dcollection))

  # identifiers unique
  result[['identifiers_unique']] <- length(identifiers) == length(unique(identifiers))

  return(result)
}

#' Convert dcollection into a data.frame
#'
#' @param dcollection The dcollection to convert
#' @export

dcollection_to_df <- function(dcollection){
  dmat_metrics <- NULL
  dmat_distribution_df <- NULL
  dmat_clara2 <- NULL

  for (i in 1:length(dcollection)){
    c_dsum <- dcollection[[i]]

    decile_labels <- paste((0:10)*10, '%', sep = '')
    metric_vector <- c('avg_hd', 'sd_hd', decile_labels)

    value_vector <- c(c_dsum$avg_hd, c_dsum$sd_hd, c_dsum$perc[decile_labels])

    dmat_metrics <- rbind(dmat_metrics,
      data.frame(sim_id = c_dsum$sim_id,
                 label = c_dsum$label,
                 sampling = c_dsum$sampling,
                 group_label = paste(c_dsum$label, c_dsum$sampling, sep = '_'),
                 uniq_id = paste(c_dsum$label, c_dsum$sampling, c_dsum$sim_id, sep = '_'),
                 metric = metric_vector,
                 value = value_vector))

    dmat_distribution_df <- rbind(dmat_distribution_df,
      data.frame(sim_id = c_dsum$sim_id,
                 label = c_dsum$label,
                 sampling = c_dsum$sampling,
                 group_label = paste(c_dsum$label, c_dsum$sampling, sep = '_'),
                 uniq_id = paste(c_dsum$label, c_dsum$sampling, c_dsum$sim_id, sep = '_'),
                 x = c_dsum$dens$x,
                 y = c_dsum$dens$y
                 )
      )

    if ('clara2' %in% names(c_dsum)){
      clara2_vals <- c(
        c_dsum$clara2$avg_within_cluster,
        c_dsum$clara2$avg_between_cluster,
        min(c_dsum$clara2$cluster_sizes) / sum(c_dsum$clara2$cluster_sizes),
        c_dsum$clara2$avg_within_cluster/c_dsum$clara2$avg_between_cluster,
        min(c_dsum$clara2$cluster_sizes)
      )
      clara2_metrics <- c(
        'avg_within_cluster',
        'avg_between_cluster',
        'cluster_size_ratio',
        'within_between_ratio',
        'smallest_cluster'
      )

      dmat_clara2 <- rbind(dmat_clara2,
        data.frame(sim_id = c_dsum$sim_id,
                  label = c_dsum$label,
                  sampling = c_dsum$sampling,
                  group_label = paste(c_dsum$label, c_dsum$sampling, sep = '_'),
                  uniq_id = paste(c_dsum$label, c_dsum$sampling, c_dsum$sim_id, sep = '_'),
                  metric = clara2_metrics,
                  value = clara2_vals))
    }

  }
  return(list(dmat_metrics = dmat_metrics,
              dmat_distribution_df = dmat_distribution_df,
              dmat_clara2_df = dmat_clara2))
}

#' Checks the result of dcollection_to_df
#'
#' Checks that the output produced by a call of dcollection_to_df on a
#' dcollection produced the expected result. Optionally, the dcollection on
#' which dcollection_to_df was called can be included to check that the values
#' are present in the result.
#'
#' @param dcollection_df The result returned by dcollection_to_df.
#' @param dcollection The dcollection on which dcollection_to_df was called.
#' @param has_clara2 Should the dcollection_df have clara2 metrics?
#' @export

check_dcollection_df <- function(dcollection_df, dcollection = NULL, has_clara2 = TRUE){

  result <- list()
  result$is_list <- class(dcollection_df) == 'list'
  
  # dmat_metrics
  result$has_dmat_metrics <- 'dmat_metrics' %in% names(dcollection_df)
  
  expect_equal(class(dcollection_df$dmat_metrics), 'data.frame')
  dmat_metrics_names <- c('sim_id', 'label', 'sampling', 
    'group_label', 'uniq_id', 'metric', 'value')
  for (col_name in dmat_metrics_names){
    expect_true(col_name %in% names(dcollection_df$dmat_metrics))
  }

  # dmat_distribution_df
  result$has_dmat_distribution_df <- 'dmat_distribution_df' %in% names(dcollection_df)
  
  expect_equal(class(dcollection_df$dmat_distribution_df), 'data.frame')
  dmat_distribution_df_names <- c('sim_id', 'label', 'sampling', 
    'group_label', 'uniq_id', 'x', 'y')
  for (col_name in dmat_distribution_df_names){
    expect_true(col_name %in% names(dcollection_df$dmat_distribution_df))
  }

  # dmat_clara2_df
  if (has_clara2){
    result$has_clara2 <- 'dmat_clara2_df' %in% names(dcollection_df)
  }

  return(result)
}




