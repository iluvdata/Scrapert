FROM rstudio/plumber:latest

RUN R -q -e  "install.packages('pak'); pak::pak('keyring'); pak::pak('iluvdata/Scrapert')"

EXPOSE 5500

ENTRYPOINT ["Rscript", "-e", "pak::pak('iluvdata/Scrapert', upgrade=TRUE); library(Scrapert); launch(wd='/app', server=TRUE)"]