# TODO: what would be the benefit to exclude cross sectional or time units only in the function, not before?
#' Estimate local projections
#'
#' \code{get_projections} estimates linear local projections.
#'
#' \code{get_projections} estimates linear local projections and returns the
#' estimation object, the FE estimates of the underlying estimations and
#' a plot with the resulting impulse response function.
#'
#' @param data_obj The data for the projections
#' @param regression_formula The general regression formula as character;
#'   the shock variable must be the first variable on the RHS
#' @param projection_horizon The horizon for which projections should be
#'   computed (8 by default)
#' @param id_var The identifier variables in data_obj as strings;
#'   cross sectional dimension comes before the time dimension;
#'   by defaul c("Country", "Year") is used
#' @param reg_model The model to be used in the regression; should match one
#'   of the plm options; by default, the within estimator is used
#' @param reg_effect The effects considered in the regression; should match
#'   one of the plm options; by default both time and cross sectional fixed
#'   effects are included
#' @param return_intermediate_data opt., FALSE by default: also return
#'  intermediate data
#' @param verbose opt., FALSE by default: if TRUE, print progress information
#' @param min_obs_i opt., 0 by default: if fewer than this nb of cross sect
#'  obs have variation in the shock variable, an error is thrown
#' @return A list with three elements: [["projections"]] contains estimations,
#'    [["impulse_plot"]] contains a ggplot2 object with the impulse response
#'    [["fe_estimates"]] contain the FE estimates.
get_projections <- function(data_obj,
                            regression_formula,
                            projection_horizon=8,
                            id_vars=c("Country", "Year"),
                            reg_model="within",
                            reg_effect="twoways",
                            return_intermediate_data=FALSE,
                            verbose=FALSE,
                            min_obs_i=0){
  return_list <- list()

  # Check input
  if (!is.character(regression_formula)){
    stop(paste0("Regression formula should by given as string, but is:",
                typeof(regression_formula)))
  }

  # Disect regression equation
  regression_formula <- gsub(" ", "", regression_formula)

  regression_formula_red <- gsub("_DLAG", "", regression_formula)
  regression_formula_red <- gsub("_LAG", "", regression_formula_red)
  dep_var <- sub("~.*", "", regression_formula_red)
  remainder <- unique(strsplit(sub(".*~", "", regression_formula_red), "\\+")[[1]])
  shock_var <- remainder[1]
  control_var <- remainder[2:length(remainder)]

  # prepare data for estimation
  estimation_data <- create_projection_data(
    data_obj = data_obj,
    dependent_var = dep_var,
    shock_var = shock_var,
    lagged_vars = control_var,
    proj_horizon = projection_horizon,
    id_vars = id_vars)

  # Test whether there is variability in the shock variable
  data.table::setDT(estimation_data)
  shock_unique <- estimation_data[complete.cases(estimation_data),]
  shock_unique <- dplyr::select(
    shock_unique, dplyr::all_of(c(shock_var, id_vars[1])))
  shock_unique <- unique(shock_unique)
  shock_unique <- dplyr::group_by(shock_unique, !!as.name(id_vars[1]))
  shock_unique <- dplyr::tally(shock_unique)
  units_wo_variation <- dplyr::filter(shock_unique, n<2)[[id_vars[1]]]
  if (sum(shock_unique$n > 1) <= min_obs_i){
    mes <- paste0("Less than five cross sectional units with ",
                  "variation in shock variable: ", shock_var)
    stop(mes)
  } else if (length(units_wo_variation)>0){
    estimation_data <- dplyr::filter(
      estimation_data, !(!!as.name(id_vars[1]) %in% units_wo_variation))
    mes <- paste0(
      "Remove the following cross-sectional units due ",
      "to lack of variation in shock variable: ",
      paste(units_wo_variation, collapse = ", "), "\n")
    warning(mes)
  }

  # Estimate projections
  projections <- paste0("k_", 1:projection_horizon)
  projection_list <- list()
  fe_estimates_list <- list()

  for (k in projections) {
    if (verbose){print(k)}

    current_formula <- as.formula(sub(dep_var, k, regression_formula))
    projection_list[[k]] <- plm::plm(
      formula = current_formula,
      data = estimation_data,
      index = id_vars,
      model = reg_model,
      effect = reg_effect
    )
    # Raise error if the shock variable is not the first parameter estimated:
    if (names(coef(projection_list[[k]]))[1] != shock_var){
      data.table::setDT(estimation_data)
      estimation_data_red <- estimation_data[, .SD, .SD = c(id_vars, shock_var)]
      # Are all elements the same at each time step for each individual?
      time_min <- min(estimation_data_red[[id_vars[2]]])
      time_max <- max(estimation_data_red[[id_vars[2]]])
      no_betw_var <- nrow(unique(
        estimation_data_red, by=c(id_vars[2], shock_var))
        ) == length(time_min:time_max)
      if(reg_model=="within" & (
        reg_effect %in% c("twoways", "time")) & no_betw_var){
        mes <- paste0("Effect of shock variable (", shock_var,
                      ") not correctly estimated. There is no variation across",
                      " cross sectional units, yet time fixed effects are ",
                      "included -> estimation fails due to collinearity.",
                      "Continue with individual effects only...")
        if (k==projections[1]){warning(mes)}

        projection_list[[k]] <- plm::plm(
          formula = current_formula,
          data = estimation_data,
          index = id_vars,
          model = reg_model,
          effect = "individual"
        )
        if (names(coef(projection_list[[k]]))[1] != shock_var){
          mes <- paste0("Effect of shock variable (", shock_var,
                        ") not correctly estimated for unknown reason.",
                        "Return list with k1 projection and estimation data.")
          warning(mes)

          return_list[["projections"]] <- projection_list
          return_list[["estimation_data"]] <- estimation_data
          return(return_list)
        }
      } else{
        mes <- paste0("Effect of shock variable (", shock_var,
                      ") not correctly estimated for unknown reason.",
                      "Return list with k1 projection and estimation data.")
        warning(mes)

        return_list[["projections"]] <- projection_list
        return_list[["estimation_data"]] <- estimation_data
        return(return_list)
      }
    }

    fe_frame <- data.frame(
      csu = names(plm::fixef(projection_list[[k]])),
      placeholder = plm::fixef(projection_list[[k]]),
      row.names = NULL
    )
    names(fe_frame) <- c("csu", k)
    fe_estimates_list[[k]] <- fe_frame
  }
  fe_estimates_frame <- fe_estimates_list %>%
    purrr::reduce(dplyr::left_join, by = "csu") %>%
    dplyr::mutate(var = dep_var)

  return_list[["projections"]] <- projection_list
  return_list[["fe_estimates"]] <- fe_estimates_frame
  return_list[["impulse_plot"]] <- create_plot(
    projection_list,
    g_title = as.character(regression_formula)
  )
  if (return_intermediate_data){
    return_list[["estimation_data"]] <- estimation_data
  }
  return(return_list)
}
