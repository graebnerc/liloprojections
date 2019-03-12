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
#' @param dep_var The dependent variable in the estimation
#' @param shock_var The shock variable in the estimation
#' @param control_var The control variables; will be included as level and dlag
#'   by default
#' @param regression_formula The general regression formula as character
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
#' @param dat_origins TODO: write help
#' @param eq_formula_remainder TODO: write help
#' @param ylabel_plot TODO: write help
#' @param lab_size TODO: write help
#' @param ready_data TODO: write help
#' @param start_year TODO: write help
#' @param transform_country_code TODO: write help
#' @return A list TODO: write help
get_projections <- function(data_obj,
                            regression_formula,
                            projection_horizon=8,
                            id_vars=c("Country", "Year"),
                            reg_model="within",
                            reg_effect="twoways"){
                            #dat_origins,
                            #eq_formula_remainder,
                            #ylabel_plot = "change in perc. p.",
                            #lab_size=10,
                            #ready_data=F, # set to true if dat_origins contains the final projection data
                            #start_year=0,
                            # transform_country_code=F) { # only years greater or equal to this year are considered
  return_list <- list()
  # Check input
  if (!is.character(regression_formula)){
    stop(paste0("Regression formula should by given as string, but is:",
                typeof(regression_formula)))
  }
  # Disect regression equation
  regression_formula <- gsub(" ", "", regression_formula)
  dep_var <- sub("~.*", "", regression_formula)
  remainder <- strsplit(sub(".*~", "", regression_formula), "\\+")[[1]]
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

  projections <- paste0("k_", 1:projection_horizon)
  projection_list <- list()
  fe_estimates_list <- list()

  for (k in projections) {
    print(k)
    current_formula <- as.formula(gsub(dep_var, k, regression_formula))
    projection_list[[k]] <- plm::plm(
      formula = current_formula,
      data = estimation_data,
      index = id_vars,
      model = reg_model,
      effect = reg_effect
    )

    # TODO: HOW DOES THIS WORK?
    # TODO: Add FE later
    # fe_estimates_list[[k]] <- data.frame(
    #   country = countrycode(names(fixef(projection_list[[k_]])), "country.name", "iso3c"),
    #   k = fixef(projection_list[[k_]]),
    #   row.names = NULL
    # )
  }
  # fe_estimates_frame <- fe_estimates_list %>%
  #   purrr::reduce(left_join, by = "country") %>%
  #   dplyr::mutate(var = var_name)
  # names(fe_estimates_frame) <- c("country", projections, "var")
  return_list[["projections"]] <- projection_list
  # return_list[["fe_estimates"]] <- fe_estimates_frame
  return_list[["impulse_plot"]] <- create_plot(projection_list,
                                               g_title = as.character(regression_formula) #,# gsub("_", " ", var_name),
                                               #g_y_axis = ylabel_plot, label_size = lab_size
  )
  return(return_list)
}
