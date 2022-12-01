#' Plot Palette
#'
#' Plot a set of charts with a set palette and theme. The function also
#' includes a parameter to assist with visualising a palette how a person
#' with colour vision deficiency may see it.
#'
#' `plot_palette()` is designed to streamline the process of finding palettes
#' that will work well with real data. It uses `ggplot2` to create a set of
#' charts to demonstrate the look and feel of a specific palette
#'
#' @param palette a vector of hex colours for plotting
#' @param theme a ggplot theme to assist with the look and feel
#' @param seed a seed to keep the plots consistent
#' @param cvd a colour vision deficiency to simulate
#'
#' @export
plot_palette <- function(palette = default_pal,
                         theme = theme_plot(),
                         seed = 2022,
                         cvd = c("none", "deutan", "protan", "tritan", "all")) {
  set <- cvd[1]

  # create plot sets
  base_plots <- create_plots(palette, theme, seed)
  deutan_plots <- create_plots(colorspace::deutan(palette), theme, seed)
  protan_plots <- create_plots(colorspace::protan(palette), theme, seed)
  tritan_plots <- create_plots(colorspace::tritan(palette), theme, seed)

  plot_title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Colour Palette Simulations",
      fontface = "bold",
      x = 0,
      hjust = 0
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 7))

  plot_footer <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Made with {plotcolr}  https://github.com/thedatacollective/plotcolr",
      x = 0,
      hjust = 0
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 7))

  ## No CVD
  if (set == "none") {
    return(cowplot::plot_grid(plot_title, base_plots, plot_footer, rel_heights = c(0.05, 1, 0.025), ncol = 1))
  }

  ## Deutan
  if (set == "deutan") {
    return(cowplot::plot_grid(plot_title, deutan_plots, plot_footer, rel_heights = c(0.05, 1, 0.025), ncol = 1))
  }

  ## Protan
  if (set == "protan") {
    return(cowplot::plot_grid(plot_title, deutan_plots, plot_footer, rel_heights = c(0.05, 1, 0.025), ncol = 1))
  }

  ## Tritan
  if (set == "tritan") {
    return(cowplot::plot_grid(plot_title, deutan_plots, plot_footer, rel_heights = c(0.05, 1, 0.025), ncol = 1))
  }

  if (set == "all") {
    return(cowplot::plot_grid(
      plot_title,
      cowplot::plot_grid(
        base_plots,
        deutan_plots,
        protan_plots,
        tritan_plots,
        ncol = 4
      ),
      plot_footer,
      rel_heights = c(0.05, 1, 0.025),
      ncol = 1
    ))
  }
}

#' Plot a set of charts to visualise a palette in the wild
#'
#' `plot_palette()` is designed to streamline the process of finding palettes
#' that will work well with real data. It uses `ggplot2` to create a set of
#' charts to demonstrate the look and feel of a specific palette
#'
#' @param palette a vector of hex colours for plotting
#' @param theme a ggplot theme to assist with the look and feel
#' @param seed a seed to keep the plots consistent
#'
#' @export
create_plots <- function(palette = default_pal,
                         theme = theme_plot(),
                         seed = 2022) {

  ## Define global variabes for R CMD check
  x <- y <- colour <- size <- fill <- NULL

  ## Control Information
  pal_length <- length(palette)

  ## Column Plot
  set.seed(seed)

  data_column <- data.frame(
    x = LETTERS[1:pal_length],
    y = stats::rnorm(pal_length, 5000, 1000)
  )

  plot_column <- ggplot2::ggplot(data_column, ggplot2::aes(x, y, fill = factor(x))) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      x = "x-axis title",
      y = "y-axis title"
    ) +
    theme +
    ggplot2::theme(legend.position = "none") +
    NULL

  ## Bar Chart (Horizontal)
  set.seed(seed - 2)

  data_bar <- data.frame(
    x = LETTERS[1:pal_length],
    y = stats::rnorm(pal_length, 5000, 1000)
  )

  plot_bar <- ggplot2::ggplot(data_bar, ggplot2::aes(x = y, y = x, fill = x)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      x = "x-axis title",
      y = "y-axis title"
    ) +
    theme +
    ggplot2::theme(legend.position = "none") +
    NULL

  ## Scatter Plot
  set.seed(seed)

  data_scatter <- data.frame(
    x = stats::rnorm(pal_length * 8, 50, 10),
    y = stats::rnorm(pal_length * 8, 10, 5),
    colour = rep(LETTERS[1:pal_length], 8)
  )

  plot_scat <- ggplot2::ggplot(data_scatter, ggplot2::aes(x, y, colour = colour)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_colour_manual(values = palette) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::labs(
      x = "x-axis title",
      y = "y-axis title"
    ) +
    theme +
    NULL

  plot_scatter <- plot_scat + ggplot2::theme(legend.position = "none")

  ## Line Chart
  set.seed(seed)

  data_line <- data.frame(
    x = rep(1:8, pal_length),
    y = unlist(lapply(1:pal_length, FUN = function(x) {
      stats::rnorm(8, x * 2, 2)
    })),
    colour = rep(LETTERS[1:pal_length], each = 8)
  )

  plot_line <- ggplot2::ggplot(data_line, ggplot2::aes(x, y, colour = colour)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_colour_manual(values = palette) +
    ggplot2::labs(
      x = "x-axis title",
      y = "y-axis title"
    ) +
    theme +
    ggplot2::theme(legend.position = "none") +
    NULL

  ## Treemap
  if (requireNamespace("treemapify", quietly = TRUE)) {
    set.seed(seed)
    data_tree <- data.frame(
      size = stats::rnorm(pal_length, mean = 1000, 500),
      colour = LETTERS[1:pal_length]
    )
    plot_treemap <- ggplot2::ggplot(data_tree, ggplot2::aes(area = size, fill = colour)) +
      treemapify::geom_treemap(colour = "white", size = 1) +
      treemapify::geom_treemap_text(
        ggplot2::aes(label = colour),
        place = "bottomleft", size = 16, color = "white",
        fontface = "bold", padding.y = grid::unit(8, "points")
      ) +
      ggplot2::scale_fill_manual(values = palette) +
      ggplot2::labs(
        x = "x-axis title",
        y = "y-axis title"
      ) +
      theme +
      ggplot2::theme(legend.position = "none") +
      NULL
  } else {
    text <- paste(
      "To visualise your palette on a treemap, install {treemapify}\n",
      "install.packages(\"treemapify\")"
    )
    plot_treemap <- ggplot2::ggplot() +
      ggplot2::annotate("text", 4, 24, label = text) +
      ggplot2::theme_void()
  }

  ## Choropleth
  if (requireNamespace("sf", quietly = TRUE)) {
    localities <- sf_qld$LGA_CODE20
    reps <- length(localities) / pal_length
    colours <- rep(LETTERS[1:pal_length], reps)
    map_colours <- data.frame(LGA_CODE20 = localities, fill = colours)
    data_choropleth <- merge(sf_qld, map_colours, by = "LGA_CODE20")

    plot_choropleth <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = data_choropleth, ggplot2::aes(fill = fill), size = 0.10) +
      ggplot2::scale_fill_manual(values = palette) +
      ggplot2::labs(
        x = "x-axis title",
        y = "y-axis title"
      ) +
      theme +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        legend.position = "none"
      ) +
      NULL
  } else {
    text <- paste(
      "To visualise your palette on a choropleth, install {sf}\n",
      "install.packages(\"sf\")"
    )
    plot_choropleth <- ggplot2::ggplot() +
      ggplot2::annotate("text", 4, 24, label = text) +
      ggplot2::theme_void()
  }

  ## Area Plot
  set.seed(seed)

  data_area <- data.frame(
    x = rep(1:20, each = pal_length),
    y = stats::runif(pal_length * 20, 100, 200),
    colour = rep(LETTERS[1:pal_length], times = 20)
  )

  plot_area <- ggplot2::ggplot(data_area, ggplot2::aes(x, y, fill = colour)) +
    ggplot2::geom_area(colour = "black", size = 0.10) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      x = "x-axis title",
      y = "y-axis title"
    ) +
    theme +
    ggplot2::theme(legend.position = "none") +
    NULL

  legend <- cowplot::get_legend(plot_scat)

  ## Combine Plots
  cowplot::plot_grid(
    plot_column, plot_bar, plot_scatter, plot_line, plot_area, plot_treemap, plot_choropleth, legend,
    rel_heights = c(1, 1, 1, 1, 1, 1, 2, .3),
    ncol = 1
  )
}

#' Save Palette Plots
#'
#' Save plots of palettes with sensible defaults
#'
#' @param filename the name and location of the file to be saved
#' @param plot Plot to save, defaults to last plot displayed.
#' @param cvd a colour vision deficiency to simulate
#'
#' @export
save_plots <- function(filename, plot = last_plot(), cvd = c("none", "deutan", "protan", "tritan", "all")) {
  layer_count <- length(plot$layers)

  if (cvd[1] == "all") {
    ggplot2::ggsave(filename, plot, width = 30, height = 30, bg = "white")
  } else {
    ggplot2::ggsave(filename, plot, width = 10, height = 30, bg = "white")
  }
}
