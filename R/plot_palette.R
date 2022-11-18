#' Plot a set of charts to visualise a palette in the wild
#'
#' `plot_palette()` is designed to streamline the process of finding palettes
#' that will work well with real data. It uses `ggplot2` to create a set of
#' charts to demonstrate the look and feel of a specific palette
#'
#' @param palette a vector of hex colours for plotting
#' @param theme a ggplot theme to assist with the look and feel
#'
#' @export
plot_palette <- function(
  palette = c("#0077b6", "#e88d67", "#ea638c", "#92dce5", "#f7ec59", "#ff66d8", "#2b2d42", "#f8f7f9"),
  theme = theme_plot(),
  seed = runif(1, 1, 1000)
  ) {

    quarto::quarto_render(
      "plot_palette.qmd",
      execute_params = c("palette" = palette, "theme" = theme, "seed" = seed))

  }

