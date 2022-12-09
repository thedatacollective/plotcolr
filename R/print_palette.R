#' Print a Colour Palette
#'
#' This function shows the colours in the palette with their associated HEX,
#' RGB and HSL values
#'
#' @param palette a vector of hex colours for plotting
print_palette <- function(palette = default_pal) {
  hue_sat_light <- c("Hue: ", "Sat: ", "Light: ")
  hsl_rounded <- lapply(hex2hsl(palette), round, 2)
  hsl_labeled <- lapply(hsl_rounded, function(x, names) {
    paste0(names, x)
  }, hue_sat_light)
  hsl_palette <- unlist(lapply(hsl_labeled, function(x) {
    paste0("HSL\n", paste0(x, collapse = "\n"))
  }))

  rgb_temp <- as.data.frame(t(grDevices::col2rgb(palette)))
  rgb_temp$red <- paste("Red:", rgb_temp$red)
  rgb_temp$green <- paste("Green:", rgb_temp$green)
  rgb_temp$blue <- paste("Blue:", rgb_temp$blue)
  rgb_palette <- paste("RGB\n", paste(rgb_temp$red, rgb_temp$green, rgb_temp$blue, sep = "\n"))

  detail <- paste(toupper(palette), rgb_palette, hsl_palette, sep = "\n\n")

  palette_data <- data.frame(
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    hex = palette,
    rgb = rgb_palette,
    hsl = hsl_palette,
    detail = detail
  )

  # create dataset for the palette
  ggplot2::ggplot(palette_data) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = palette),
      colour = "black") +
    ggplot2::facet_wrap(~hex, nrow = 1) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::geom_text(ggplot2::aes(x = 1, y = 0, label = detail), vjust = 1.1) +
    ggplot2::theme(
      legend.position = "none",
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.placement = "none",
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid = ggplot2::element_blank()
    )
}

#' Save Palette
#'
#' Save a png of the colour palette for reference
#'
#' @param filename the name and location of the file to be saved
#' @param plot a colour palette created by `print_palette()` or `print_coolors()`
#'
#' @export
save_palette <- function(filename, plot = last_plot()) {
    ggplot2::ggsave(filename, plot, width = 297, height = 210, units = "mm", bg = "white")
}
