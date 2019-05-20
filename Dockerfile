FROM rocker/r-base
MAINTAINER Pharbers Chen <contact@pharbershub>
RUN apt-get update
RUN apt-get install -y libssl-dev libsasl2-dev
RUN apt-get install -y libssl-dev
RUN R -e 'install.packages("openssl", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("data.table", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("dplyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("tidyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("DT", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("plumber", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("mongolite", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("jsonlite", repos = "http://cran.cnr.berkeley.edu/")'
COPY API.R /API.R
COPY functions.R /functions.R
COPY Performance_eva.R /Performance_eva.R
EXPOSE 8000
CMD Rscript /API.R
