FROM rocker/verse

RUN apt-get update && apt-get install -y \
    libarpack2-dev \
    libglpk-dev \
    libxt-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c(\
    'cluster', \
    'rdist', \
    'RSpectra', \
    'plotly'\
), repos = 'https://cran.rstudio.com/')"