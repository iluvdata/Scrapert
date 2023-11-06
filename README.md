
# Scrapert

<!-- badges: start -->
<!-- badges: end -->

The goal of Scrapert is to scrape values from Xpert HPV result pdfs, save to a localbase.  Alternatively can be configured to store results in a remote database (e.g. REDCap).

## Installation

You can install the development version of Scrapert from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("iluvdata/Scrapert")
```

If the session is interactive (i.e. Rstudio) you will have to browse to http://localhost:5500 (using whatever port you've assigned in `config.yml`).  Otherwise R will attempt to launch a browser window. R will shutdown after closing the browser window after a short delay.

## Launching

This will copy a shell of a configuration file (`config.yml`) into the current working directory.  You can edit this file to adjust certain features of Scrapert (like port and timeout but most settings will be set by the app).   If you screw up and want to start over, just delete `config.yml` and relaunch.

``` r
library(Scrapert)
launch()
```

## Data Storage
A sqlite3 database `xpertdb` and the `config.yml` will be created in the working directory. These files should be backed up when the application is not running. Temporary files `scrapert.log`, `plumber.lock` and `.plumber.R` will be created and managed by application.
