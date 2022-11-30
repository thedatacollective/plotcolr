#' Plot Colours from Coolors website
#'
#' Visualise a palette from a coolors url.
#'
#' @param palette a vector of hex colours for plotting
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
