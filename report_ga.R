# Install devtools package & rga - This is only done one time
# install.packages("devtools")
#library(devtools)
#install_github("rga", "skardhamar")
#library(rga)
#library(lubridate)
#library(ggplot2)

#GA analytics analysis
rga.open(instance = "ga", where = "../ga-api") # this will open a browser. You need to have turned on API access for GA 

ga.df <- ga$getData("ga:79584079",
                                  start.date = "2014-01-01",
                                  end.date = "2014-09-30",
                                  metrics = "ga:visits,ga:avgTimeOnSite",
                                  dimensions = "ga:date,ga:nthDay,ga:dayOfWeek", 
                                  max = 1500, sort = "ga:nthDay")

ga.df$nthDay <- as.integer(ga.df$nthDay)

# Nicely readable names of the week by making it a factor.
ga.df$dayOfWeek <- factor(ga.df$dayOfWeek, labels = c("Sunday", "Monday", "Tuesday", 
                                                      "Wednesday", "Thursday", "Friday", "Saturday"))

# The ga.pvs dataset is not only faceted by time (like above), but also by
# page and the source of traffic (medium).
ga.pvs <- ga$getData("ga:79584079",  start.date = "2014-01-01",
                     end.date = "2014-09-30",
                     metrics = "ga:pageviews,ga:visits,ga:visitors", dimensions = "ga:pageTitle,ga:medium,ga:nthDay", 
                     sort = "-ga:pageviews", batch = TRUE)
ga.pvs$nthDay <- as.integer(ga.pvs$nthDay)

qplot(data = ga.df, x = nthDay, y = visits) + geom_smooth(method = lm)  # add linear regression line with confidence interval in dark gray
qplot(data = ga.df, x = dayOfWeek, y = visits) + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim = quantile(ga.df$visits, c(0.1, 0.9)))
pageViewsByMediumAndDay <- aggregate(pageviews ~ medium + nthDay, data = ga.pvs, FUN = sum)
# Percentage of visits from Google, direct and through referrals over time
ggplot(pageViewsByMediumAndDay, aes(nthDay, weight = pageviews, fill = medium)) + geom_bar(position = "fill", binwidth = 7) + scale_y_continuous(labels = percent) +  xlab("nth day") + ylab("Percentage") + scale_fill_discrete("Traffic source\n(medium)", breaks = c("(none)", "organic", "referral"), labels = c("Direct", "Search", "Referral"))
