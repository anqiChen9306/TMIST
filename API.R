
library(plumber)

pr <- plumb("./Performance_eva.R")

pr$run(host = '0.0.0.0', port = 8000)

# http://127.0.0.1:8000/ntm/5cc018a2f4ce4374c23cece6/5cd518adf4ce43ee2495d4db