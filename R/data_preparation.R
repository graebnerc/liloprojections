#' Prepare data for local projections.
#'
#' \code{create_projection_data} prepares the data used for computing projections.
#'
#' \code{create_projection_data} takes the original data and adds lags, dlags and future
#' changes of the relevant variables. This is the data set on which then projections can
#' be computed on.
#'
#' @param data_obj The data to be used for the projections
#' @param dependent_var The dependent variable for the estimation
#' @param shock_var The shock variable
#' @param lagged_vars The variables included as lags on the RHS
#' @param proj_horizon The time horizon for the projections, 8 by defauls
#' @param id_vars The id vars for the underlying panel data; should be given as variable name or index
#' @return A data frame to be used for local projection estimation
create_projection_data <- function(data_obj, dependent_var, shock_var, lagged_vars, proj_horizon=8, id_vars=c("Country", "Year")){
  data_obj_new <- dplyr::select(data_obj, dplyr::one_of(id_vars, unique(c(dependent_var, shock_var, lagged_vars))))
  data_obj_new <- add_lags(data_obj_new, unique(c(dependent_var, shock_var, lagged_vars)))
  data_obj_new <- add_k(data_obj_new, dependent_var)
  return(data_obj_new)
}

#' Adding lags and dlags to projection data.
#'
#' \code{add_lags} adds lags and differences of lags of selected variables.
#'
#' \code{add_lags} takes a data set and variable names, and adds the lags and
#' differenced lags (dlags) of these variables to the dataset.
#'
#' @param data_obj A data frame or related object with the raw data
#' @param var_names The names of the variables for which lags and dlags should
#'   be added
#' @return A tibble that contains all original variables, as well as lags and
#'   dlags of the selected variable
add_lags <- function(data_obj, var_names){
  data_obj_new <- data_obj
  for (var_name in var_names){
    data_obj_new <- data_obj_new %>%
      dplyr::mutate(UQ(as.name(paste0(var_name, "_LAG"))) := dplyr::lag(UQ(as.name(var_name)), n = 1),
                    UQ(as.name(paste0(var_name, "_DLAG"))) := dplyr::lag(UQ(as.name(var_name)), n = 1) - dplyr::lag(UQ(as.name(var_name)), n = 2))
  }
  return(data_obj_new)
}

#' Adding projection horizon to raw data.
#'
#' \code{add_k} adds future states of the dependent variable to the raw data.
#'
#' \code{add_k} adds the future states of the dependent variable, i.e. the states
#' on which projections will be performed, to the raw data. These are the k's
#' referred to in the original paper. The standard horizon is 8, but
#' alternative time horizons can be chosen as well.
#'
#' @param data_obj_new The original data set
#' @param dependent_var The dependent variable as a string
#' @param k_horizon The number of time steps for the projections
#' @return Modified tibble with the ks added as new columns
add_k <- function(data_obj, dependent_var, k_horizon=8){
  data_obj_new <- data_obj
for (k in 1:k_horizon) {
  data_obj_new <- data_obj_new %>%
    dplyr::mutate(UQ(as.name(paste0("k_", k))) := dplyr::lead(
      UQ(as.name(dependent_var)),
      n = k
    ) - UQ(as.name(dependent_var)))
}
  return(data_obj_new)
}
