
#' Get a set of shades from a base colour
#'
#' @param colour a hex colour value to determine a set of shades from.
#' @paran n the number of shades to calculate.
#'
#' @return a vector of hex colours
#' @export
get_shades <- function(colour, n = 10) {
  hsl <- unname(hex2hsl(colour))

  gaps <- (10 - 1) / (n - 1)

  shades <- lapply(1:n, FUN = function(x, shades, gaps) {
    shade <- shades[x]
    hsl[[1]]["l"] <- 0.1 * (0.5 + (x - 1) * gaps)
    unlist(hsl)
  }, hsl, gaps)

  shades_rgb <- lapply(shades, hsl2rgb)

  shades_hex <- lapply(shades_rgb, function(x) {
    grDevices::rgb(
      x["red.l"],
      x["green.l"],
      x["blue.l"],
      maxColorValue = 255
    )
  })

  unlist(shades_hex)
}

#' Convert HSL values to RGB
#'
#' @param hsl a vector of length three with values for Hue, Saturation & Light
#' @export
hsl2rgb <- function(hsl) {
  hue <- hsl[1]
  sat <- hsl[2]
  light <- hsl[3]

  hue <- hue / 60
  if (light <= 0.5) {
    t2 <- light * (sat + 1)
  } else {
    t2 <- light + sat - (light * sat)
  }
  t1 <- light * 2 - t2
  r <- hue2rgb(t1, t2, hue + 2) * 255
  g <- hue2rgb(t1, t2, hue) * 255
  b <- hue2rgb(t1, t2, hue - 2) * 255
  c(red = r, green = g, blue = b)
}

hue2rgb <- function(t1, t2, hue) {
  if (hue < 0) hue <- hue + 6
  if (hue >= 6) hue <- hue - 6
  if (hue < 1) {
    return((t2 - t1) * hue + t1)
  } else if (hue < 3) {
    return(t2)
  } else if (hue < 4) {
    return((t2 - t1) * (4 - hue) + t1)
  } else {
    return(t1)
  }
}

#' Convert an RGB colour to an HSL value
#'
#' @param col a vector of length three with values for Red, Green & Blue. The RGB assumes a range from 0-255.
#' @export
rgb2hsl <- function(col) {
  rgb <- unname(c(col[1] / 255, col[2] / 255, col[3] / 255))

  min <- rgb[1]
  max <- rgb[1]
  maxcolor <- 0

  for (i in 1:(length(rgb))) {
    if (rgb[i] <= min) {
      min <- rgb[i]
    }
    if (rgb[i] >= max) {
      max <- rgb[i]
      maxcolor <- i
    }
  }

  h <- NA_real_
  if (maxcolor == 1) {
    h <- (rgb[2] - rgb[3]) / (max - min)
  }

  if (maxcolor == 2) {
    h <- 2 + (rgb[3] - rgb[1]) / (max - min)
  }

  if (maxcolor == 3) {
    h <- 4 + (rgb[1] - rgb[2]) / (max - min)
  }

  if (is.na(h)) {
    h <- 0
  }

  h <- h * 60

  if (h < 0) {
    h <- h + 360
  }

  l <- (min + max) / 2

  if (min == max) {
    s <- 0
  } else {
    if (l < 0.5) {
      s <- (max - min) / (max + min)
    } else {
      s <- (max - min) / (2 - max - min)
    }
  }

  s <- s

  c(h = h, s = s, l = l)
}

#' Convert Hex Colours to HSL
#'
#' @param colour a vector of hex colours in the format '#123456'
#' @return a list object vectors representing the HSL values of the colours
hex2hsl <- function(colour) {
  matrix_rgb <- grDevices::col2rgb(colour)
  df_rgb <- as.data.frame(t(matrix_rgb))

  list_rgb <- lapply(1:nrow(df_rgb), function(x) {
    unlist(df_rgb[x, c("red", "green", "blue")])
  })

  names(list_rgb) <- colour

  lapply(list_rgb, rgb2hsl)
}
