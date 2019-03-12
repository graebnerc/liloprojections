#' Plot impulse response function
#'
#' \code{create_plot}
#'
#' \code{create_plot}
#'
#' @param projections_list A list with the projection objects
#' @param g_title A string with the title for the graph
#' @param g_y_axis The label for the y axis
#' @param label_size The size for the labels in the plot
#' @param transform_country_code Helper to transform country codes
#' @return A ggplot object
create_plot <- function(projections_list,
                        g_title,
                        g_y_axis = "increase in perc. p.",
                        label_size = 10) {
  coefs <- list()
  ses <- list()
  upperbounds <- list()
  lowerbounds <- list()
  g_title <- ifelse(g_title == "size of finance", "Size of financial sector",
                    ifelse(g_title == "exp to gdp", "Exports to GDP",
                           ifelse(g_title == "Gini net", "Gini (pre tax)", g_title)
                    )
  )

  for (k in names(projections_list)) {
    coefs[[k]] <- lmtest::coeftest(projections_list[[k]],
                           vcov = function(x) vcovBK(x,
                                                     type = "HC1",
                                                     cluster = "group"
                           )
    )[1, 1]
    ses[[k]] <- lmtest::coeftest(projections_list[[k]],
                         vcov = function(x) vcovBK(x,
                                                   type = "HC1",
                                                   cluster = "group"
                         )
    )[1, 2]
    upperbounds[[k]] <- coefs[[k]] + ses[[k]]
    lowerbounds[[k]] <- coefs[[k]] - ses[[k]]
  }

  plot_frame <- data.frame(
    coef = c(0, unlist(coefs)),
    se = c(0, unlist(ses)),
    upperbound = c(0, unlist(upperbounds)),
    lowerbound = c(0, unlist(lowerbounds)),
    Year = 0:8, row.names = NULL
  )

  final_plot <- ggplot(plot_frame, aes(x = Year, y = coef)) +
    geom_ribbon(
      data = plot_frame, aes(
        ymin = lowerbound,
        ymax = upperbound,
        linetype = NA
      ),
      alpha = .5,
      fill = "#acb6c0",
      color = "#acb6c0"
    ) +
    geom_abline(intercept = 0, slope = 0, colour = "#1e3752", linetype = 2) +
    geom_line(colour = "#1e3752", size=1.2) +
    labs(
      x = "Year",
      y = g_y_axis,
      title = g_title
    ) +
    scale_x_continuous(
      limits = c(0, 8),
      expand = c(0, 0)
    ) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border = element_blank(),
      axis.line = element_line(),
      title = element_text(size = label_size+1),
      axis.text = element_text(size = label_size-1),
      axis.title = element_text(size = label_size)
    )

  return(final_plot)
}
