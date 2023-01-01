# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# get shiny server and R from the rocker project
FROM rocker/shiny-verse:4.1.2

RUN echo $LANG
# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev
  
RUN apt-get update
RUN apt-get install -y locales locales-all
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

RUN echo $LANG
# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinydashboard", \
              "googledrive", \
              "dplyr", \
              "tidyr", \
              "ggplot2", \
              "lubridate", \
              "shinyTime", \
              "lubridate", \
              "icons",\
              "cellranger",\
              "googlesheets4",\
              "shinythemes",\
              "remotes",\
              "plotly",\
              "viridis",\
              "here",\
              "extrafont"\
            ))'
RUN R -e 'library(remotes)'
RUN R -e 'remotes::install_github("mitchelloharawild/icons")'
# copy all files into a directory called app
COPY . /app
# copy authentication token secrets
COPY ./.secrets/* /app/.secrets/

# run app
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]