FROM rocker/verse

# Install Java and system dependencies
RUN apt-get update && apt-get install -y \
    default-jdk \
    libarpack2-dev \
    libglpk-dev \
    libxt-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Configure Java for R
RUN R CMD javareconf

# Install required R packages
RUN R -e "install.packages(c( \
    'tidyverse', \
    'rJava', \
    'xlsx', \
    'cluster', \
    'rdist', \
    'RSpectra', \
    'plotly' \
), repos = 'https://cran.rstudio.com/')"
