
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggfree: ggplot2-style plots with just base R graphics

[ggplot2](https://ggplot2.tidyverse.org/) is a popular R graphics
package that is becoming synonymous with data visualization in R. The
community of developers working within the ggplot2 framework have
implemented some [rather nice
extensions](https://www.ggplot2-exts.org/gallery/) as well. However, I
personally find the ggplot2 syntax to be confusing, most probably
because I’ve been using the base graphics package in R to generate plots
for over a decade. I seldom see a ggplot2-based visualization that
couldn’t also be implemented in base graphics.

The overall purpose of `ggfree` is to make it easier to generate plots
in the style of [ggplot2](https://ggplot2.tidyverse.org/) and its
extensions, without ever actually using any ggplot2 code.

## Installation

For the time being, the simplest method is to use the `devtools`
package:

``` r
# install.packages('devtools')  # if you haven't already installed devtools
require(devtools)
devtools::install_github("ArtPoon/ggfree")
```

(You could also simply copy-paste the functions you want to use into
your R script.)

## Examples

### Fancy gridlines

The major and minor gridlines on a shaded background is one of the
distinctive features of the default ggplot2 theme. This implementation
uses `rect` and `abline` to obtain the same look.

``` r
require(ggfree)
#> Loading required package: ggfree
# prepare the plot region
plot(NA, xlim=range(faithful$eruptions), ylim=range(faithful$waiting),
     xlab='Duration of eruption (mins)', ylab='Waiting time (mins)')
add.grid()  # <-- a ggfree function!
points(faithful$eruptions, faithful$waiting, pch=21, bg='white')
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="500px" style="display: block; margin: auto;" />

We can also vary the colours, turn off major or minor gridlines, or skip
vertical or horizontal lines:

``` r
plot(NA, xlim=range(faithful$eruptions), ylim=range(faithful$waiting),
     xlab='Duration of eruption (mins)', ylab='Waiting time (mins)')
# now with non-default arguments!
add.grid(mode='x', bg.col='seashell2', lwd.minor=0)
points(faithful$eruptions, faithful$waiting, pch=21, bg='white')
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="500px" style="display: block; margin: auto;" />

### Slopegraphs

In general, a slopegraph is a method for visually comparing a paired set
of observations. To illustrate the use of slopegraphs, I’ve adapted a
carbon dioxide emissions data set similar to the one used by @clauswilke
to demonstrate slopegraphs in *Fundamentals of Data Visualization*. This
data sets is packaged with `ggfree`:

``` r
require(ggfree)
co2.emissions
#>                          per.cap.2000 per.cap.2010
#> Netherland Antilles              8.52         5.99
#> Bahrain                          7.97         6.33
#> Kuwait                           7.53         7.99
#> Aruba                            7.19         6.73
#> United States Of America         5.42         4.69
#> Luxembourg                       5.16         5.89
#> Trinidad And Tobago              5.03         9.84
#> Canada                           4.75         4.27
#> Australia                        4.69         4.81
#> Faeroe Islands                   4.10         3.53
```

There are two styles of slopegraphs that are implemented in `ggfree`.
For the first style (where the argument `type` is left at its default
value `'b'`), a vertical axis provides reference for the numerical
values:

``` r
slopegraph(co2.emissions, colorize=T)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="500px" style="display: block; margin: auto;" />
Setting `colorize` to `TRUE` causes the line segments to be coloured to
emphasize positive and negative slopes.

For the second style (setting `type='t'`), we substitute the raw
numerical values for data points, which makes the vertical axis
unnecessary:

``` r
par(family='Palatino')  # use a fancier font

# actual code here
slopegraph(co2.emissions, type='t', cex.text=0.6, names.arg=c(2000, 2010))

# make a nice title
title(expression(text=paste('CO'[2], ' emissions (metric tons) per capita')), 
      cex=0.8)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="500px" style="display: block; margin: auto;" />
