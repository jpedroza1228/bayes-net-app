# Use Rocker Shiny as base image
FROM rocker/shiny:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libv8-dev \
    libgraphviz-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN Rscript -e "install.packages(c( \
    'shiny', \
    'shinydashboard', \
    'shinythemes', \
    'shinymeta', \
    'bslib', \
    'tidyverse', \
    'bnlearn', \
    'Rgraphviz', \
    'here' \
  ), repos='https://cloud.r-project.org/')"

# Copy your Shiny app into the container
# Assumes your app.R and functions.R are in the ./app directory
COPY ./app /srv/shiny-server/app

# Change working directory
WORKDIR /srv/shiny-server/app

# Make Shiny the default app and ensure functions.R is sourced correctly
ENV SHINY_APP_DIR="/srv/shiny-server/app"

# Expose the default Shiny port
EXPOSE 3838

# Run Shiny app
CMD ["R", "-e", "shiny::runApp(appDir=getwd(), host='0.0.0.0', port=3838)"]
