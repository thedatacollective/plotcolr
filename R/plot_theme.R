#' Default Palette
default_pal <- c("#0077b6", "#e88d67", "#ea638c", "#92dce5", "#f7ec59", "#ff66d8", "#2b2d42", "#f8f7f9")

#' Default Theme for plotcolr charts
#'
#' A default ggplot2 theme to help with the presentation of colour palettes
#'
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_size plot title font family and size
#' @param subtitle_family,subtitle_size subtitle font family and size
#' @param title_family,title_size subtitle font family and size
#' @param caption_family,caption_size caption font family and size
#'
#' @export
theme_plot <- function(base_family = "Arial",
                       base_size = 14,
                       plot_title_family = "Arial Bold",
                       plot_title_size = 28,
                       subtitle_family = base_family,
                       subtitle_size = 16,
                       title_family = base_family,
                       title_size = 14,
                       caption_family = base_family,
                       caption_size = 12) {
  ret <- ggplot2::theme_minimal(
    base_family = base_family,
    base_size = base_size
  )

  # position key items in the plotting area
  ret <- ret + ggplot2::theme(plot.title.position = "plot")
  ret <- ret + ggplot2::theme(plot.caption.position = "plot")
  ret <- ret + ggplot2::theme(legend.position = "bottom")

  # set text elements
  ret <- ret + ggplot2::theme(text = ggplot2::element_text(
    family = base_family,
    colour = "#3f3f3f"
  ))
  ret <- ret + ggplot2::theme(title = ggplot2::element_text(
    family = title_family,
    size = title_size,
    colour = "#3f3f3f",
    lineheight = 1.4
  ))
  ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(
    family = plot_title_family,
    size = plot_title_size,
    face = "bold",
    colour = "#1C1C1C",
    lineheight = 1.2
  ))
  ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(
    family = subtitle_family,
    size = subtitle_size,
    colour = "#1C1C1C",
    lineheight = 1.2,
    margin = ggplot2::margin(5, 0, 10, 0)
  ))

  # set axis defaults
  ret <- ret + ggplot2::theme(axis.text = ggplot2::element_text(size = base_size))
  ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(
    size = subtitle_size,
    hjust = .99,
    margin = ggplot2::margin(0, 10)
  ))
  ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(
    size = subtitle_size,
    hjust = 1
  ))

  # facet headings & panel detail
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(
    family = title_family,
    size = base_size,
    face = "bold",
    colour = "#1C1C1C",
    hjust = 0
  ))
  ret <- ret + ggplot2::theme(strip.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(panel.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(panel.spacing = ggplot2::unit(25, "points"))
  ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(
    colour = "#F4F4F4",
    size = 0.5
  ))

  # set legend elements
  ret <- ret + ggplot2::theme(legend.direction = "horizontal")
  ret <- ret + ggplot2::theme(legend.title = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.text = ggplot2::element_text(hjust = 0, size = base_size))
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.position = "bottom")

  # captions
  ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(
    family = caption_family,
    size = caption_size,
    face = "plain",
    colour = "#3f3f3f",
    hjust = 0
  ))

  ret
}
