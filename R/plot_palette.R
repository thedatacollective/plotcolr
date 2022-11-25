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

  plot_list <- list()

  ## No CVD
  if (set %in% c("none", "all")) {
    plot_list <- append(
      plot_list,
      create_plots(palette, theme, seed)
    )
  }

  ## Deutan
  if (set %in% c("deutan", "all")) {
    plot_list <- append(
      plot_list,
      create_plots(colorspace::deutan(palette), theme, seed)
    )
  }

  ## Protan
  if (set %in% c("protan", "all")) {
    plot_list <- append(
      plot_list,
      create_plots(colorspace::protan(palette), theme, seed)
    )
  }

  ## Tritan
  if (set %in% c("tritan", "all")) {
    plot_list <- append(
      plot_list,
      create_plots(colorspace::tritan(palette), theme, seed)
    )
  }

  class(plot_list) <- "multiplot"
  plot_list
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

  ## Control Information
  pal_length <- length(palette)

  ## Column Plot
  set.seed(seed)

  data_column <- data.frame(
    x = LETTERS[1:pal_length],
    y = rnorm(pal_length, 5000, 1000)
  )

  plot_column <- ggplot2::ggplot(data_column, ggplot2::aes(x, y, fill = factor(x))) +
    ggplot2::geom_col() +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      title = "A Simulated Column Chart",
      subtitle = paste0("Made with {plotcolr} on ", Sys.Date()),
      x = "x-axis title",
      y = "y-axis title",
      caption = "Source: Simulated data for visualisation"
    ) +
    theme +
    NULL

  ## Bar Chart (Horizontal)
  set.seed(seed - 2)

  data_bar <- data.frame(
    x = LETTERS[1:pal_length],
    y = rnorm(pal_length, 5000, 1000)
  )

  plot_bar <- ggplot2::ggplot(data_bar, ggplot2::aes(x = y, y = x, fill = x)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      title = "",
      subtitle = "",
      x = "x-axis title",
      y = "y-axis title",
      caption = ""
    ) +
    theme +
    NULL

  ## Scatter Plot
  set.seed(seed)

  data_scatter <- data.frame(
    x = rnorm(pal_length * 8, 50, 10),
    y = rnorm(pal_length * 8, 10, 5),
    colour = rep(LETTERS[1:pal_length], 8)
  )

  plot_scatter <- ggplot2::ggplot(data_scatter, ggplot2::aes(x, y, colour = colour)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_colour_manual(values = palette) +
    ggplot2::labs(
      x = "x-axis title",
      y = "y-axis title",
    ) +
    theme +
    NULL

  ## Line Chart
  set.seed(seed)

  data_line <- data.frame(
    x = rep(1:8, pal_length),
    y = unlist(lapply(1:pal_length, FUN = function(x) {
      rnorm(8, x * 2, 2)
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
      y = "y-axis title",
    ) +
    theme +
    NULL

  ## Treemap
  if (requireNamespace("treemapify", quietly = TRUE)) {
    set.seed(seed)
    data_tree <- data.frame(
      size = rnorm(pal_length, mean = 1000, 500),
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
        y = "y-axis title",
      ) +
      theme +
      ggplot2::theme(legend.position = "none") +
      NULL
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
        y = "y-axis title",
      ) +
      theme +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      ) +
      NULL
  }

  ## Area Plot
  set.seed(seed)

  data_area <- data.frame(
    x = rep(1:20, each = pal_length),
    y = runif(pal_length * 20, 100, 200),
    colour = rep(LETTERS[1:pal_length], times = 20)
  )

  plot_area <- ggplot2::ggplot(data_area, ggplot2::aes(x, y, fill = colour)) +
    ggplot2::geom_area(colour = "black", size = 0.10) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      x = "x-axis title",
      y = "y-axis title",
    ) +
    theme +
    NULL

  ## Combine Plots
  list(plot_column, plot_bar, plot_scatter, plot_line, plot_area, plot_treemap, plot_choropleth)
  # multiplot(plot_column, plot_bar, plot_scatter, plot_line, plot_area, plot_treemap, plot_choropleth, cols = 2)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist = NULL, file, cols = 2, layout = NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  num_plots <- length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(
      seq(1, cols * ceiling(num_plots / cols)),
      ncol = cols,
      nrow = ceiling(num_plots / cols),
      byrow = TRUE
    )
  }

  if (num_plots == 1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(
      grid::viewport(
        layout = grid::grid.layout(nrow(layout), ncol(layout))
      )
    )

    # Make each plot, in the correct location
    for (i in 1:num_plots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      match_idx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(
        plots[[i]],
        vp = grid::viewport(
          layout.pos.row = match_idx$row,
          layout.pos.col = match_idx$col
        )
      )
    }
  }
}

# TODO: fix custom print method
print.multiplot <- function(x, ...) {
  multiplot(plotlist = x)
}