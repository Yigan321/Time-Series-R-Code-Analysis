#Get session info details
sessionInfo()
#Installing V8 package via install.package method
install.packages("V8", version="3.4.0", repos="https://cran.r-project.org")
#Rstan is required for Prophet to work. Rstan id dependent package for prophet
install.packages("rstan", version="2.21.2", dependencies=TRUE, repos="https://cran.r-project.org")
#Installing Prophet
install.packages("prophet", version="1.0", dependencies = TRUE, repos="https://cran.r-project.org")
#Installing rlang 
install.packages("rlang", version="0.4.10",repos="https://cran.r-project.org")
#To check sessionInfo after rlang
sessionInfo()
#Installing forecast package
install.packages("forecast", version="8.14", dependencies = TRUE,repos="https://cran.r-project.org")
#Install devtools package
install.packages("devtools", version="2.3.2", repos="https://cran.r-project.org")
#Install date package
install.packages("date", version="1.2-39", repos="https://cran.r-project.org")
#Install zoo package
install.packages("zoo", version="1.8-9", repos="https://cran.r-project.org")
#Install TTR package
install.packages("TTR",version="0.24.2",repos="https://cran.r-project.org") 
#Install tidyquant package
install.packages("tidyquant", version="1.0.3", dependencies= TRUE, repos="https://cran.r-project.org")
#Install smooth package
install.packages("smooth", version="3.1.0", repos="https://cran.r-project.org")
#Install DMWR
install.packages( c("xts","quantmod") )
setwd("D:/R Scripts/")
install.packages("D:/R Scripts/DMwR_0.4.1.tar.gz")
install.packages( "D:/R Scripts/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )

#Install rjava packages
install.packages("rJava", version="0.9-13", epos="https://cran.r-project.org")
#Install RJDBC package
install.packages("RJDBC", version="0.2-8", epos="https://cran.r-project.org")
#Install xgboost package
install.packages("xgboost", version="1.3.2.1", epos="https://cran.r-project.org")
#Install reticulate package
install.packages("reticulate", version="1.18", epos="https://cran.r-project.org")
# TensorFlow Dependencies
install.packages("tfruns", version="1.5.0", epos="https://cran.r-project.org")
# TensorFlow
packageurl <- "https://cran.r-project.org/src/contrib/Archive/tensorflow/tensorflow_2.2.0.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
#Install Zeallot package
install.packages("zeallot", version="0.1.0", epos="https://cran.r-project.org")
# Keras Version 
packageurl1 <- "https://cran.r-project.org/src/contrib/Archive/keras/keras_2.3.0.0.tar.gz"
install.packages(packageurl1, repos=NULL, type="source")
#Install ggplot
install.packages("ggplot", version="3.3.3", epos="https://cran.r-project.org")
#Install neuralnet
install.packages("neuralnet", version="1.44.2", epos="https://cran.r-project.org")
#Install reshape
install.packages("reshape", version="0.8.8", repos="https://cran.r-project.org")
#Install lubridate
install.packages("lubridate", version="1.7.10", repos="https://cran.r-project.org")
#Installing dependent packages
install.packages("abind")
install.packages("ROCR")
install.packages("/dbfs/FileStore/my_packages/DMwR_0_4_1_tar.gz", repos=NULL, type="source")
#Installing greybox which is required for Smooth package. Smooth is dependent on greybox
install.packages("greybox", version="0.6.8", repos="https://cran.r-project.org")

