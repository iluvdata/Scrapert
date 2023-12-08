FROM rstudio/plumber:latest

RUN R -e "remotes::install_github('iluvdata/Scrapert')"

EXPOSE 5500

ENTRYPOINT ["R", "-e", "library(Scrapert); launch(server=TRUE)"]