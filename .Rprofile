# Start renv
source("renv/activate.R")

if (file.exists("~/.Rprofile")) {
  base::sys.source("~/.Rprofile", envir = environment())
}

# For more options, see https://bookdown.org/yihui/blogdown/global-options.html
options(
#   blogdown.hugo.version = "0.76.5",
  blogdown.hugo.version = "0.78.2",
  blogdown.use.processx = FALSE,  # Use processx instead of a background process
  # xfun.bg_process.verbose = TRUE,  # Show debugging logs
  blogdown.serve_site.startup = FALSE,  # Automatically serve the site on RStudio startup
  blogdown.knit.on_save = FALSE,  # Automatically knit Rmd on save
  blogdown.knit.serve_site = FALSE  # Stop serving the site when knitting individual files
)
