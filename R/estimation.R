#' Estimate local projections
#'
#' \code{get_projections}
#'
#' \code{get_projections}
#'
#' @param var_cons TODO: write help
#' @param countries_considered TODO: write help
#' @param dat_origins TODO: write help
#' @param eq_formula_remainder TODO: write help
#' @param ylabel_plot TODO: write help
#' @param lab_size TODO: write help
#' @param ready_data TODO: write help
#' @param start_year TODO: write help
#' @param transform_country_code TODO: write help
#' @return A list TODO: write help
get_projections <- function(var_cons,
                            countries_considered,
                            dat_origins,
                            eq_formula_remainder,
                            ylabel_plot = "change in perc. p.",
                            lab_size=10,
                            ready_data=F, # set to true if dat_origins contains the final projection data
                            start_year=0,
                            transform_country_code=F) { # only years greater or equal to this year are considered
  return_list <- list()
  var_name <- var_cons
  if (ready_data==FALSE){
    data_name <- dat_origins[[var_name]]
    if (transform_country_code==T){
      raw_data <- read.csv(data_name) %>%
        dplyr::mutate(Country=countrycode(Country, "iso3c", "country.name")) %>%
        dplyr::filter(Country %in% countries_considered,
                      Year>=start_year)
    } else{
      raw_data <- read.csv(data_name) %>%
        dplyr::filter(Country %in% countries_considered,
                      Year>=start_year)
    }
  } else {
    #browser()
    raw_data <- dat_origins[[var_name]] %>%
      dplyr::filter(Country %in% countries_considered,
                    Year>=start_year)
  }

  projections <- paste0("k.", 1:8)
  projection_list <- list()
  fe_estimates_list <- list()

  for (k in projections) {
    print(k)
    k_ <- sub("\\.", "_", k)
    # reg_eq <- as.formula(paste0(k, "~ EShockentry + Unemployment_rate + Capital_accumulation_DLAG + Unemployment_rate_DLAG"))
    reg_eq <- as.formula(paste0(k, eq_formula_remainder[[var_name]]))
    projection_list[[k_]] <- plm(
      formula = reg_eq,
      data = raw_data,
      index = c("Country", "Year"),
      model = "within",
      effect = "twoways"
    )
    #print("142")
    fe_estimates_list[[k_]] <- data.frame(
      country = countrycode(names(fixef(projection_list[[k_]])), "country.name", "iso3c"),
      k = fixef(projection_list[[k_]]),
      row.names = NULL
    )
    #print("148")
  }
  #print("150")
  # browser()
  fe_estimates_frame <- fe_estimates_list %>%
    purrr::reduce(left_join, by = "country") %>%
    dplyr::mutate(var = var_name)
  names(fe_estimates_frame) <- c("country", projections, "var")
  #print("155")
  return_list[["projections"]] <- projection_list
  return_list[["fe_estimates"]] <- fe_estimates_frame
  return_list[["impulse_plot"]] <- create_plot(projection_list,
                                               g_title = gsub("_", " ", var_name),
                                               g_y_axis = ylabel_plot, label_size = lab_size
  )
  #print("vor return")
  return(return_list)
}
