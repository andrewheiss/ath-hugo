---
title: Working with R, Cairo graphics, custom fonts, and ggplot
date: 2017-09-27
year: "2017"
month: "2017/09"
description: The Cairo graphics library makes it easy to embed custom fonts in PDFs and create high resolution PNGs.
images: 
- /blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/example-plot-1.png
tags: 
  - r
  - ggplot
  - dataviz
  - cairo
  - fonts
slug: working-with-r-cairo-graphics-custom-fonts-and-ggplot
---

Skip to instructions for [macOS](#mac) or [Windows](#windows)

---

R and ggplot can create fantastic graphs, but the default Arial/Helvetica font is too boring and standard. You can change the font used in a plot fairly easily three different ways:

1. All of the built-in ggplot themes have a `base_family` argument for setting the overall font family for the plot
2. `element_text()` has a `family` argument for changing fonts on individual plot elements
3. `geom_text()` and `annotate(geom = "text", ...)` have a `family` argument for changing fonts on text layers

For example:

``` r
# Load libraries
library(tidyverse)

# Create plot
p <- ggplot(mpg, aes(x = cyl, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  annotate("text", x = 5, y = 35, label = "There aren't a lot of\n5 cylinder cars",
           family = "Source Sans Pro Semibold", color = "#DC5B44", size = 4) +
  labs(title = "Highway miles per gallon and cylinders",
       subtitle = "This is an interesting relationship, I guess",
       caption = "Source: ggplot's built-in data",
       x = "Cylinders", y = "Highway miles per gallon") +
  theme_light(base_family = "Source Sans Pro") +
  theme(plot.caption = element_text(family = "Source Sans Pro ExtraLight"))
p
```

{{< figure src="example-plot-1.png" alt="Example nice plot" class="img-75" >}}

However, there are a couple difficulties when using custom fonts like this:

1. R on Windows does not automatically see custom fonts and will throw an error if you try to use them.
2. `ggsave()` on its own cannot correctly save PDF versions of plots with custom fonts—it cannot embed the fonts.

Fixing both of these issues is relatively easy. On Windows, you can either load fonts into R on the fly with `windowsFonts(name_of_font_inside_r = windowsFont("Name of actual font"))`, or you can use `extrafonts::load_fonts()` from the `extrafonts` library to permanently load fonts into R's internal database. A full example of this [is included below](#windows).

Embedding fonts in PDFs is also fairly easy. Instead of using R's default PDF-writing engine, you can use the [Cairo graphics library](https://www.cairographics.org/) (which, nowadays, is conveniently packaged with R). Cairo has full Unicode support and can handle embedding custom fonts just fine. To make `ggsave()` use the Cairo engine when writing a PDF, specify the device:

``` r
ggsave(..., filename = "whatever.pdf", ..., device = cairo_pdf)
```

You can also use Cairo's PNG engine when writing PNG files. R's default PNG-writing engine can sometimes have issues with correctly setting the resolution. In theory, if you specify a width and a height and a DPI, `ggsave()` will generate a file with those dimensions. However, if you place the PNG into Word, PowerPoint, InDesign, or any other programs, the graphic will be too large, for reasons unknown. If you save the graphic with the Cairo library, though, these programs will respect the size and DPI and place the image correctly.

``` r
ggsave(..., filename = "whatever.png", ..., dpi = 300, type = "cairo")
```

Using the Cairo PNG library makes a significant difference when you use the image in other programs. Notice how the Cairo-based PNG is actually 4 inches wide in Word, while R's default PNG takes up the full width of the page and uses a lower resolution:

{{< figure src="cairo_notcairo_word.png" alt="Cairo vs. not Cairo in Word" class="img-75" >}}

Finally, if you use R Markdown and knitr, you can specify the Cairo device for each output type in the document metadata:

``` yaml
---
title: "Whatever"
output:
  pdf_document:
    dev: cairo_pdf
  html_document:
    dev: png
---
```

---

&nbsp;

Here's how you can use `ggplot::ggsave()` and Cairo to create PDF with embedded custom fonts and PNGs with correct resolutions:

## Full instructions for macOS {#mac}

The Cairo graphics library should be installed behind the scenes when you install R—you should not need to install any R-specific Cairo libraries or anything for this to work. However, you *do* need to install an X11 window system first, like [XQuartz](https://www.xquartz.org/).

You can verify that you have Cairo support by running the `capabilities()` function; `TRUE` should show up under `cairo`:

``` r
capabilities()
#>        jpeg         png        tiff       tcltk         X11        aqua
#>        TRUE        TRUE        TRUE        TRUE        TRUE        TRUE
#>    http/ftp     sockets      libxml        fifo      cledit       iconv
#>        TRUE        TRUE        TRUE        TRUE       FALSE        TRUE
#>         NLS     profmem       cairo         ICU long.double     libcurl
#>        TRUE        TRUE        TRUE        TRUE        TRUE        TRUE
```

R on macOS should automatically see the fonts you have installed on your computer.

Here's a full example of loading and using a custom font on macOS:

``` r
# Load libraries
library(tidyverse)

# Create sample data
set.seed(1234)  # This makes R run the same random draw
df <- data_frame(x = rnorm(100),
                 y = rnorm(100))

# Create plot
p <- ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  labs(title = "This is a title",
       subtitle = "This is a subtitle") +
  annotate("text", x = 0, y = 0, label = "This is some text",
           family = "Papyrus", color = "darkred", size = 8) +
  theme_light(base_family = "Comic Sans MS")
p
```

{{< figure src="full-example-mac-1.png" alt="Example plot on macOS" class="img-75" >}}

``` r
# Save the plot as a PDF with ggsave and Cairo
# R will want to autocomplete cairo_pdf to cairo_pdf() (note the parentheses)
# This will not work with the parentheses; ensure there aren't any
ggsave(p, filename = "example.pdf", device = cairo_pdf,
       width = 4, height = 3, units = "in")

# You can also save the plot as a high resolution PNG using Cairo
# Note the difference here; instead of using device = cairo_pdf, you use
# type = "cairo". It's confusing and weird and that's just life.
ggsave(p, filename = "example.png", dpi = 300, type = "cairo",
       width = 4, height = 3, units = "in")
```

## Full instructions for Windows {#windows}

The Cairo graphics library should be installed behind the scenes when you install R—you should not need to install any special Cairo libraries or anything for this to work.

You can verify that you have Cairo support by running the `capabilities()` function; `TRUE` should show up under `cairo`:

``` r
capabilities()
#>        jpeg         png        tiff       tcltk         X11        aqua
#>        TRUE        TRUE        TRUE        TRUE       FALSE       FALSE
#>    http/ftp     sockets      libxml        fifo      cledit       iconv
#>        TRUE        TRUE        TRUE        TRUE       FALSE        TRUE
#>         NLS     profmem       cairo         ICU long.double     libcurl
#>        TRUE        TRUE        TRUE        TRUE        TRUE        TRUE
```

R on Windows cannot see the fonts you have installed on your computer. You can see a list of fonts R does have access to with the `windowsFonts()` function:

``` r
windowsFonts()
#> $serif
#> [1] "TT Times New Roman"
#>
#> $sans
#> [1] "TT Arial"
#>
#> $mono
#> [1] "TT Courier New"
```

You can add all your system fonts to that database by installing the `extrafont` library and running `font_import()`. This will take a while, though, and it will only pick up fonts that are currently installed. If you install a font later, R will not see it—you'll need to run `extrafont::font_import()` again.

Alternatively, you can load fonts into R on the fly, without loading the full database, using `windowsFonts(name_of_font_inside_r = windowsFont("Name of actual font"))`:

``` r
windowsFonts(`Comic Sans MS` = windowsFont("Comic Sans MS"))
```

Once you do this, the font will be loaded:

``` r
windowsFonts()
#> $serif
#> [1] "TT Times New Roman"
#>
#> $sans
#> [1] "TT Arial"
#>
#> $mono
#> [1] "TT Courier New"
#>
#> $`Comic Sans MS`
#> [1] "Comic Sans MS"
```

This only takes effect for your current R session, so if you are knitting a document or if you ever plan on closing RStudio, you'll need to incorporate this font assignment code into your script. If you don't want to do that, run `extrafont::load_fonts()` to load all the fonts—once you do this, you won't need to repeatedly run `windowsFonts()` to load fonts each time you run a script.

Here's a full example of loading and using a custom font on Windows:

``` r
# Load specific fonts into R's internal database
windowsFonts(`Comic Sans MS` = windowsFont("Comic Sans MS"))
windowsFonts(Papyrus = windowsFont("Papyrus"))
```

``` r
# Load libraries
library(tidyverse)

# Create sample data
set.seed(1234)  # This makes R run the same random draw
df <- data_frame(x = rnorm(100),
                 y = rnorm(100))

# Create plot
p <- ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  annotate("text", x = 0, y = 0, label = "This is some text",
           family = "Papyrus", color = "darkred", size = 8) +
  labs(title = "This is a title",
       subtitle = "This is a subtitle") +
  theme_light(base_family = "Comic Sans MS")
p
```

{{< figure src="full-example-windows2-1.png" alt="Example plot in Windows" class="img-75" >}}

``` r
# Save the plot as a PDF with ggsave and Cairo
# R will want to autocomplete cairo_pdf to cairo_pdf() (note the parentheses)
# This will not work with the parentheses; ensure there aren't any
ggsave(p, filename = "example.pdf", device = cairo_pdf,
       width = 4, height = 3, units = "in")

# You can also save the plot as a high resolution PNG using Cairo
# Note the difference here; instead of using device = cairo_pdf, you use
# type = "cairo". It's confusing and weird and that's just life.
ggsave(p, filename = "example.png", dpi = 300, type = "cairo",
       width = 4, height = 3, units = "in")
ggsave(p, filename = "example_no_cairo.png", dpi = 300,
       width = 4, height = 3, units = "in")
```
