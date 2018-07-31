FROM ubuntu:17.10
MAINTAINER edward@dragonfly.co.nz

# Set New Zealand mirrors and set timezone to Auckland
RUN sed -i 's/archive/nz.archive/' /etc/apt/sources.list
RUN apt-get update
RUN apt-get install -y tzdata
RUN echo "Pacific/Auckland" > /etc/timezone
RUN dpkg-reconfigure -f noninteractive tzdata
ENV DEBIAN_FRONTEND noninteractive

# Set the locale to New Zealand
RUN apt-get -y install locales
RUN locale-gen en_NZ.UTF-8
RUN dpkg-reconfigure locales
ENV LANG en_NZ.UTF-8
ENV LANGUAGE en_NZ:en

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        r-base-dev r-recommended r-base \
        wget build-essential \
        gdal-bin libgdal-dev libproj-dev libgeos++-dev  proj-bin proj-data \
        git-core libssl-dev curl libpq-dev libssh2-1-dev libcurl4-openssl-dev libxml2-dev  \
        libudunits2-0 libudunits2-dev \
        jags pandoc && \
    apt upgrade --yes && \
    apt-get autoremove && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*


COPY Rprofile.site /etc/R/

# Data manipulation
RUN Rscript -e "install.packages('devtools')"
RUN Rscript -e "devtools::install_version('dplyr', '0.7.0')"
RUN Rscript -e "devtools::install_github('tidyverse/ggplot2@ffb40f3f0ad9b4')"
RUN Rscript -e "devtools::install_version('tidyverse', '1.1.1')"
RUN Rscript -e "devtools::install_version('data.table', '1.10.4')"

# Reproducibility
RUN Rscript -e "devtools::install_version('xtable', '1.8-2')"
RUN Rscript -e "devtools::install_version('rmarkdown', '1.6')"
RUN Rscript -e "devtools::install_version('kableExtra', '0.5.2')"
RUN Rscript -e "devtools::install_version('knitr', '1.16')"
RUN Rscript -e "devtools::install_version('bsplus', '0.1.0')"

# Plotting
RUN Rscript -e "devtools::install_version('RColorBrewer', '1.1-2')"
RUN Rscript -e "devtools::install_version('viridis', '0.4.0')"

# Bayesian modelling
RUN Rscript -e "devtools::install_version('rjags', '4-6')"
RUN Rscript -e "devtools::install_version('StanHeaders', '2.16.0-1')"
# A work around of this Stan install issue https://github.com/stan-dev/rstan/issues/447#issuecomment-325172186
RUN sed -i 's/R_XTRA_CXXFLAGS = /R_XTRA_CXXFLAGS = -DBOOST_PHOENIX_NO_VARIADIC_EXPRESSION/' /etc/R/Makeconf
RUN Rscript -e "devtools::install_version('rstan', '2.16.2')"
RUN Rscript -e "devtools::install_version('coda', '0.19-1')"
RUN Rscript -e "devtools::install_version('loo', '1.1.0')"

# GIS
RUN Rscript -e "devtools::install_version('shapefiles', '0.7')"
RUN Rscript -e "devtools::install_version('maptools', '0.9-2')"
RUN Rscript -e "devtools::install_version('rgeos', '0.3-23')"
RUN Rscript -e "devtools::install_version('mapproj', '1.2-5')"
RUN Rscript -e "devtools::install_version('rgdal', '1.2-7')"
RUN Rscript -e "devtools::install_github('r-spatial/sf@46aa370c0ca27')"
RUN Rscript -e "devtools::install_version('sp', '1.2-5')"
RUN Rscript -e "devtools::install_version('raster', '2.5-8')"

#Others
RUN Rscript -e "devtools::install_version('foreach', '1.4.3')"
