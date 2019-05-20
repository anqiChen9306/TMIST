library(dockerfiler)

dk <- Dockerfile$new()
dk$MAINTAINER(name = "Pharbers Chen", email = "contact@pharbershub")

dk$RUN("apt-get update")
dk$RUN("apt-get install -y libssl-dev libsasl2-dev")
dk$RUN("apt-get install -y libssl-dev")
# dk$RUN("apt-get install openssl@1.1")

dk$RUN(r(install.packages("openssl",repos = "http://cran.cnr.berkeley.edu/")))
dk$RUN(r(install.packages("data.table",repos = "http://cran.cnr.berkeley.edu/")))
dk$RUN(r(install.packages("dplyr",repos = "http://cran.cnr.berkeley.edu/")))
dk$RUN(r(install.packages("tidyr",repos = "http://cran.cnr.berkeley.edu/")))
dk$RUN(r(install.packages("DT",repos = "http://cran.cnr.berkeley.edu/")))
dk$RUN(r(install.packages("plumber",repos = "http://cran.cnr.berkeley.edu/")))
dk$RUN(r(install.packages("mongolite",repos = "http://cran.cnr.berkeley.edu/")))
dk$RUN(r(install.packages("jsonlite",repos = "http://cran.cnr.berkeley.edu/")))

# dk$RUN("mkdir /ntm_docker")
# dk$RUN("cd /ntm_docker")
dk$COPY("API.R", "/API.R")
dk$COPY("functions.R", "/functions.R")
dk$COPY("Performance_eva.R", "/Performance_eva.R")
dk$EXPOSE(8000)
dk$CMD("Rscript /API.R")

dk$write()

