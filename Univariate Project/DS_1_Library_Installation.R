#############################################################################
# Advanced Forecasting System                       Author: Derek Kane 8/2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package Installation - Time Series
#############################################################################

# Set a working directory

setwd("D:/R Scripts/")

#############################################################################
# Install the Libraries
#############################################################################

# R Version 3.6.1 - Previous



# RUN THIS STEP WHEN THE CLUSTER IS RESTARTED OR TERMINATED AND STARTED, 
# WE ARE DELETING RLANG 0.3.1 and installing 0.3.4, which is important for forecast to work
# remove.packages("rlang","/usr/local/lib/R/site-library/")

# Please download and install Rtools 3.5 from https://cran.r-project.org/bin/windows/Rtools/.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages("rlang", version="0.4.6", dependencies= TRUE,repos="https://cran.r-project.org")

# install.packages("rlang", version="0.3.4", dependencies= TRUE,
#                 lib="/databricks/spark/R/lib/", repos="https://cran.r-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages("devtools", dependencies= TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download package tarball from CRAN archive

url <- "https://cran.r-project.org/src/contrib/Archive/backports/backports_1.1.0.tar.gz" 
pkgFile <- "backports_1.1.0.tar.gz"
download.file(url = url, destfile = pkgFile)

# Install package
install.packages(pkgs=pkgFile, type="source", repos=NULL)

# Delete package tarball
unlink(pkgFile)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("forecast", version="8.12", repos="https://cran.r-project.org", dependencies = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("date", version="1.2-39", repos="https://cran.r-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("reshape", version="0.8.8", repos="https://cran.r-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("zoo", version="1.8.8", repos="https://cran.r-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("DMwR", version="0.4.1", repos="https://cran.r-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("lubridate", version="1.7.9", repos="https://cran.r-project.org")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("TTR", version="0.23", repos="https://cran.r-project.org")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("smooth", version="2.5.6", repos="https://cran.r-project.org")
#install_version("smooth", version="3.1.0", repos="https://cran.r-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# require(devtools)
# install_version("nloptr", version="1.2.1", repos="https://cran.r-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install.packages("tidyquant", dependencies= TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install.packages("rJava", dependencies= TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install.packages("RJDBC", dependencies= TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install.packages('prophet', version="0.6.1", dependencies= TRUE) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install.packages("xgboost", dependencies= TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install.packages("reticulate", dependencies= TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install.packages("tfruns", dependencies= TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)

packageurl <- "https://cran.r-project.org/src/contrib/Archive/tensorflow/tensorflow_1.14.0.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

# https://community.rstudio.com/t/python-module-tensorflow-was-not-found-similar-to-issue-144/36884/5


# library(tensorflow)
# install_tensorflow(method = "conda", envname = "tf_test")
# 
# library(reticulate)
# 
# 
# use_condaenv(condaenv = "tf_test", conda = "C:/Users/derek/Anaconda3/envs/tf_test/python.exe")
# 
# Sys.setenv(RETICULATE_PYTHON = "C:/Users/derek/Anaconda3/envs/tf_test/python.exe")
# Sys.setenv(TENSORFLOW_PYTHON="C:/Users/derek/Anaconda3/envs/tf_test/python.exe")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install.packages("zeallot", dependencies= TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)

packageurl <- "https://cran.r-project.org/src/contrib/Archive/keras/keras_2.2.4.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("ggplot", version="0.4.2", repos="https://cran.r-project.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(devtools)
install_version("neuralnet", version="1.44.2", repos="https://cran.r-project.org")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tensorflow)
install_tensorflow()
