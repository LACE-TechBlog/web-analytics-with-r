library("stringr")
library("ggplot2")
library("gridExtra")

#there are a few people we know are using the tag differently, remove them
#do any others here
all.tweets = tweets[tweets$from_user != "_Laurenly_" , ]
all.tweets = tweets[tweets$from_user != "Pink_Laundry" , ]


#find top tweeters and calculate counts
top.tweeters<-as.data.frame(table(all.tweets[,2])) 
top.tweeters<-top.tweeters[ order(-top.tweeters[,2]), ]
top.tweeters$row.name <- NULL
colnames(top.tweeters)[1] <- "Username"
print("Top Twitters:")
print(head(top.tweeters,20), row.names = FALSE)
nrow(top.tweeters)


#find top hashtags
all.tweets$text <-tolower(all.tweets$text)
all.hashtags<-str_extract_all(all.tweets$text, "#\\S+")
hashtag.use<-as.data.frame(table(unlist(all.hashtags)))
hashtag.use<-hashtag.use[ order(-hashtag.use[,2]), ]
print("Top HashTags:")
print(head(hashtag.use,20), row.names = FALSE)


#plot them
countlinkSub <- subset(hashtag.use, Freq>2) # subset of just links appearing more than twice
colnames(hashtag.use)[1] <- "Hashtag"
# plot to see distribution of links
ggplot(hashtag.use, aes(reorder(Hashtag, Freq), Freq)) +
  xlab("Hashtag") +
  ylab("Number of messages containing the Hashtag")+
  geom_point() +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) +
  theme(axis.title.y = element_text(size = 14, angle=90)) +
  theme(plot.margin = unit(c(1,1,2,2), "lines"))
#ggsave("fig/hashtags_in_tweets.jpg", width=12, height=20)

# investigate the URLs contained in the Twitter messages https://github.com/meefen/twitter-hashtag-analytics
require(stringr)
require(ggplot2)
require(grid)
all.tweets$link <- sapply(all.tweets$text,function(tweet) str_extract(tweet,("http[[:print:]]+"))) # creates new field and extracts the links contained in the tweet
all.tweets$link <- sapply(all.tweets$text,function(tweet) str_extract(tweet,"http[[:print:]]{16}")) # limits to just 16 characters after http so I just get the shortened link.
countlink <- data.frame(URL = as.character(unlist(dimnames(sort(table(all.tweets$link))))), N = sort(table(all.tweets$link))) # get frequencies of each link and put in rank order
rownames(countlink) <- NULL # remove rownames
countlinkSub <- subset(countlink, N>2) # subset of just links appearing more than twice

# plot to see distribution of links
print(ggplot(countlinkSub, aes(reorder(URL, N), N)) +
  xlab("URL") +
  ylab("Number of messages containing the URL")+
  geom_point() +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) +
  theme(axis.title.y = element_text(size = 14, angle=90)) +
  theme(plot.margin = unit(c(1,1,2,2), "lines")))

#  ggsave("fig/urls_in_tweets.jpg", width=12, height=8)

#time plot http://www.r-bloggers.com/charting-twitter-time-series-data-with-tweet-and-unique-user-counts/
dt <- 30  # \Delta t, minutes
all.tweets$time <- as.POSIXct(strptime(all.tweets$time, "%d/%m/%Y %H:%M:%S", tz = "GMT"))
minDate <- min(all.tweets$time)
maxDate <- max(all.tweets$time) + 60 * dt
dateBreaks <- seq(minDate, maxDate, by=60 * dt)
tweetCount <- hist(all.tweets$time , breaks=dateBreaks, plot=FALSE)
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]
userCount <- sapply(binBreaks, function(d) length(unique(tweets$user[which((tweets$date >= d) & (tweets$date <= d + 60*dt))])))
plotData <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count), users=as.numeric(userCount))
ggplot(plotData) +
  geom_bar(aes(x=dates, y=tweets, color=users), stat="identity") +
  scale_x_datetime("Date") +
  scale_y_continuous("Number of tweets") 
ggsave("fig/tag_usage_over_time.jpg", width=12, height=8)


