# Use Rocker Shiny Verse as base image
FROM rocker/shiny-verse:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libv8-dev \
    libgraphviz-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Create app directory
RUN mkdir -p /srv/shiny-server/

# Copy application files to app directory
COPY app.R /srv/shiny-server/app.R
# COPY loan_data_only_cat.csv /srv/shiny-server/loan_data_only_cat.csv
# COPY blacklist.txt /srv/shiny-server/blacklist.txt
# COPY whitelist.txt /srv/shiny-server/whitelist.txt

# Install required R packages
RUN Rscript -e "install.packages(c('BiocManager', 'shinydashboard', 'shinythemes', 'shinymeta', 'bslib', 'tidyverse', 'bnlearn', 'here'), repos='https://cloud.r-project.org/')"
RUN Rscript -e "BiocManager::install('Rgraphviz')"

# Expose the default Shiny port
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
