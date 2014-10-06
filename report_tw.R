require(RCurl)
#install.packages("knitr")
#install.packages("gridExtra")
#install.packages("pvclust")
#install.packages("wordcloud")
library(gridExtra)

#setupscript
#grab the latest tweets from our spreadsheet
#u      <- "https://docs.google.com/spreadsheet/pub?key=0Ah6ebMDR0G5WdHZtWHktZWNCOFBaVllRVVVITzFNOXc&single=true&gid=82&output=csv"
#tc     <- getURL(u, ssl.verifypeer=FALSE)
#tweets <- read.csv(textConnection(tc))
#OR READ THE LOCAL COPY
tweets <- read.csv("../data/lacerange.csv")

#if cores break uncomment this
#options(mc.cores=1)

#gather information on the tweets
#source("twitter_analysis.R")
#source("twitter_analysis.R")