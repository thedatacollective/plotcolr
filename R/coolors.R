#' Plot Colours from Coolors website
#'
#' Visualise a palette from a coolors url.
#'
#' @param url a palette url from the coolors.co website
#' @param theme a ggplot theme to assist with the look and feel
#' @param seed a seed to keep the plots consistent
#' @param cvd a colour vision deficiency to simulate
#'
#' @return a ggplot object
#' @export
plot_coolors <- function(url,
                         theme = theme_plot(),
                         seed = 2022,
                         cvd = c("none", "deutan", "protan", "tritan", "all")) {
  palette <- get_coolors(url)

  plot_palette(palette, theme = theme, seed = seed, cvd = cvd)
}

#' Print a Coolors Palette
#'
#' This function shows the colours in the palette with their associated HEX,
#' RGB and HSL values
#'
#' @param url a palette url from the coolors.co website
print_coolors <- function(url) {
  palette <- get_coolors(url)

  print_palette(palette)
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
