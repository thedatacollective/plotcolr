#' Plot Colours from Coolors website
#'
#' Visualise a palette from a coolors url.
#'
#' @param url the url for a specific coolors palette
#' @param theme
#' @param seed
#'
#' @return a ggplot object
#' @export
plot_coolors <- function(url,
                         theme = theme_plot(),
                         seed = 2022) {
  palette <- get_coolors(url)

  plot_palette(palette, theme = theme, seed = seed)
}

#' Get Coolors
#'
#' Get a vector of hex colours from a coolors url
#'
#' @param url the url for a specific coolors palette
#'
#' @return a vector of hex colours
#' @export
get_coolors <- function(url) {
  colour_ref <- sub("https://coolors.co/", "", url)
  base_colours <- unlist(strsplit(colour_ref, "-"))
  palette <- paste0("#", base_colours)

  palette
}

