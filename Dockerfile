FROM rstudio/plumber:latest

RUN R -q -e  "install.packages('pak'); pak::pak('iluvdata/Scrapert')"

EXPOSE 5500

ENTRYPOINT ["R", "-e", "library(Scrapert); launch(server=TRUE)"]