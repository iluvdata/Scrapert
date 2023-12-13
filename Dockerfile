FROM rstudio/plumber:latest

RUN R -q -e  "install.packages('pak'); pak::pak('keyring'); pak::pak('iluvdata/Scrapert', upgrade=TRUE)"

EXPOSE 5500

ENTRYPOINT ["R", "-e", "library(Scrapert); launch(wd='/app', server=TRUE)"]