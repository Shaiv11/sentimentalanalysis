library(ggplot2)
library(lubridate)
library(Scale)
library(reshape2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(syuzhet) 
library(dplyr ) 
library(jpeg)

folder <- "C:/Users/Shaival/Desktop/groups"
setwd(folder)

#get the data from whatsapp chat 

files <- list.files(folder, pattern = ".txt")


text = NULL
for(i in 1:length(files)){
  text[[i]] = readLines(files[i], encoding = "UTF-8", warn = FALSE)
}
#mes <- read.csv("chat.csv")


for (i in 1:length(files)) {
  
#text = readLines("group_1.txt")
  
#let us create the corpus
docs <- Corpus(VectorSource(text[[i]]))

#clean our chat data
trans <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, trans, "/")
docs <- tm_map(docs, trans, "@")
docs <- tm_map(docs, trans, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))

#application specific
docs <- tm_map(docs, removeWords, c("shaival" ,"mehta","nikkuu", "daaassss","jay","joshiddu", "chirag" ,"sanilddu","akash", "prajapatiddu","nikhil", "navadiyaddu","tullebbb"))

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

#create the document term matrix
dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat),decreasing=TRUE)

#Data frame
data <- data.frame(word = names(v),freq=v)
rownames(data) <- NULL
write.csv(data, paste("group_",i,".csv",sep = ""), append = FALSE)
head(data, 10)


#generate the wordcloud
#set.seed(1056)
#wordcloud(words = data$word, freq = data$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



#fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(as.character(text[[i]]))
#head(Sentiment)
t <- cbind(text[[i]],Sentiment)


#count the sentiment words by category
TotalSentiment <- data.frame(colSums(t[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#total sentiment score of all texts
mypath <- file.path("C:/Users/Shaival/Desktop/groups/plots",paste("myplot_",i, ".jpeg", sep = ""))

jpeg(mypath)

t <- ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score") 

print(t)

Sys.sleep(10)
dev.off()


}

