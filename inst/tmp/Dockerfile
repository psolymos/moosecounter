FROM rocker/r-ver:4.1.1
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2 \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages(c('dplyr', 'DT', 'ggiraph','ggplot2', 'kableExtra', \
    'magrittr', 'mapproj', 'markdown', 'MASS', 'Matrix', 'openxlsx', \
    'partykit', 'pbapply', 'pscl', 'purrr', 'remotes', 'shiny', 'shinyBS', \
    'shinydashboard'))"
RUN R -e "remotes::install_github('psolymos/moosecounter')"
EXPOSE 3838
CMD ["R", "-e", "options(shiny.port = 3838, shiny.host = '0.0.0.0');moosecounter::run_app()"]
