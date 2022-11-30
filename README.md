
# plotcolr

<!-- badges: start -->
<!-- badges: end -->

The goal of `{plotcolr}` is to provide a straightforward way to be able to visualise a colour palette across a range of chart types.

`plot_palette()` does the main heavy lifting and will create a `ggplot2` object that can be rendered in interactive mode or saved to a file with `save_plots()`.

## Installation

You can install the development version of `{plotcolr}` from github with:

``` r
remotes::install_github("thedatacollective/plotcolr")
```

## Usage

The main function is `plot_palette()`.

```r
library(plotcolr)

# simulate the palette on a set of predefined charts
plot_palette(
  palette = c("#edd83d", "#69995d", "#f4442e" "#202c59")
)

# save the plots to a png file
save_plots("my_plotted_palette.png")
```

It can often be useful to see how a palette will look for different colour vision deficiencies.

```r
library(plotcolr)

# simulate the palette on a set of predefined charts
plot_palette(
  palette = c("#edd83d", "#69995d", "#f4442e" "#202c59"),
  cvd = "deutan"
)

# visualise the most common colour vision deficiencies
# along with the palette
plot_palette(
  palette = c("#edd83d", "#69995d", "#f4442e" "#202c59"),
  cvd = "all"
)
```

We also wanted to make it easy to make see how a generated palette would look using the [coolors](https://coolors.co) service. `plot_coolors()` takes a url from the coolors website and simulates the colours on some plots.

```r
plot_coolors(
  palette = c("#edd83d", "#69995d", "#f4442e" "#202c59")
)
```

---

Free software by:

![The data collective logo](inst/images/thedatacollective.png)

