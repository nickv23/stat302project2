<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/nickv23/stat302project2.svg?branch=master)](https://travis-ci.com/nickv23/stat302project2)
  [![Codecov test coverage](https://codecov.io/gh/nickv23/stat302project2/branch/master/graph/badge.svg)](https://codecov.io/gh/nickv23/stat302project2?branch=master)
  <!-- badges: end -->

## Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):


``` r
# install.packages("devtools")
devtools::install_github("nickv23/stat302project2", build_vignette = TRUE, build_opts = c())
library(stat302project2)
# Use this to view the vignette in the stat302project2 HTML help
help(package = "stat302project2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "stat302project2")
```
