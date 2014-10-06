# some basic and widely-used text mining techniques to identify the issues
# that captured the attention of Twitter-using anthropologists during the meeting.
require(tm)
#library("wordcloud")
#library("RColorBrewer")
#install.packages("topicmodels")
library(tm)
library("RWeka")
library("slam")


#prepare text
more.stopwords <- c("via", "rt", "mt", "amp")
corpus <- Corpus(VectorSource(all.tweets$text)) # create corpus object
corpus <- tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
corpus <- tm_map(corpus, mc.cores=1, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"), mc.cores=1)
corpus <- tm_map(corpus, function(x) removeWords(x, append(stopwords("english"), more.stopwords)))
corpus <- tm_map(corpus, tolower, mc.cores=1)
corpus <- tm_map(corpus, PlainTextDocument) #proplem with to lower means we need to make it type of plain text document again
tdm <- TermDocumentMatrix(corpus)

#make a wordcloud

# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
#wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
# save the image in png format
png("fig/hashtag.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

# Train a LDA model based on a sparse term-document matrix
# A LDA model with the optimal number of topics
#'''
#library("topicmodels") # have to install libgsl0-dev before installing this package on Ubuntu
#library("slam")
# create document term matrix and convert to data frame
#td.mat.sp <- removeSparseTerms(td.mat, sparse=0.99)
# td.mat.sp.df <- as.data.frame(inspect(td.mat.sp))
# check how many words are left
# nrow(td.mat.sp.df)
# transpose document term matrix
#td.mat.sp.t <- t(td.mat.sp)
# summary(col_sums(td.mat.sp.t)) # check median
# calculate tf-idf values
##term_tfidf <- tapply(td.mat.sp.t$v/row_sums(td.mat.sp.t)[td.mat.sp.t$i], td.mat.sp.t$j,mean) *
#  log2(nDocs(td.mat.sp.t)/col_sums(td.mat.sp.t > 0))
# summary(term_tfidf) # check median... note value for next line...
#td.mat.sp.t.tdif <- td.mat.sp.t[, term_tfidf >= as.numeric(summary(term_tfidf)[3])]
# keep only those terms that are slightly less frequent that the median
#td.mat.sp.t.tdif <- td.mat.sp.t[row_sums(td.mat.sp.t) > 0, ]
# summary(col_sums(td.mat.sp.t.tdif)) # have a look
# train a topic model for every number of topics between 2 and 50 (may take long)
#best.model <- lapply(seq(2, 50, by = 1), function(d) LDA(td.mat.sp.t.tdif, d))
# a list of logLiks for each model
#best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
# Find the number of topics which has the highest log likelihood
#best.model.logLik.df <- data.frame(topics=c(2:50),
#                                   LL=as.numeric(as.matrix(best.model.logLik)))
#optimal.num <- best.model.logLik.df$topics[
#  which(best.model.logLik.df$LL == max(best.model.logLik.df$LL))]
# plot the distribution of logliklihoods by topic
# ggplot(best.model.logLik.df, aes(x = topics, y = LL)) +
# xlab("Number of topics") +
# ylab("Log likelihood of the model") +
# geom_line() +
# geom_vline(xintercept=optimal.num, linetype="dotted", colour="red") +
# annotate("text", x=optimal.num, y=25, label=paste("num =",optimal.num), hjust=0)
# generate a LDA model with the best number of topics
#LDA(td.mat.sp.t.tdif, optimal.num)



#Random inspects
inspect(tdm[1:10,1:10]) # have a quick look at the term document matrix
findFreqTerms(tdm, lowfreq=20) # have a look at common words, in this case, those that appear at least 30 times, good to get high freq words and add to stopword list and re-make the dtm, in this case add aaa, panel, session
findAssocs(tdm, 'students', 0.30) # find associated words and strength of the common words. I repeated this function for the ten most frequent words.


mydata.df <- as.data.frame(inspect(tdm))
count<- as.data.frame(rowSums(mydata.df))
count$word = rownames(count)
colnames(count) <- c("count","word" )
count_with_hashtags<-count[order(count$count, decreasing=TRUE), ]

#ngram finder

#tokenizer for tdm with ngrams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
findFreqTerms(tdm, lowfreq = 15)

#create dataframe and order by most used
rollup <- rollup(tdm, 2, na.rm=TRUE, FUN = sum)
mydata.df <- as.data.frame(inspect(rollup))
colnames(mydata.df) <- c("count")
mydata.df$ngram <- rownames(mydata.df)
newdata <- mydata.df[order(-count),]
newdata<-newdata[order(newdata$count, decreasing=TRUE), ]


