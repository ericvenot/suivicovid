FROM rocker/r-ver:3.6.3
RUN apt-get update && apt-get install -y  default-jre-headless git-core libcurl4-openssl-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.4.0.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "0.8.5")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.1")'
RUN Rscript -e 'remotes::install_version("xlsx",upgrade="never", version = "0.6.3")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.0")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("rworldmap",upgrade="never", version = "1.3-6")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');suivicovid::run_app()"
