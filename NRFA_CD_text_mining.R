# https://www.red-gate.com/simple-talk/sql/bi/text-mining-and-sentiment-analysis-with-r/


# Installing and loading R packages ---------------------------------------


# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis
install.packages("ggplot2") # for plotting graphs
# Load
library("tm")
library("SnowballC")
library("wordcloud")
# library("RColorBrewer")
library("syuzhet")
library("ggplot2")


# Reading file data into R ------------------------------------------------

# Add the NRFA id to the text so that it can be identified later:
text <- data.frame(doc_id = Catchment_descriptors$STATION, 
                   text = with(text, paste(NAME, TEXT, sep = " - ")),
                   stringsAsFactors = FALSE)

# Create a dataframe source compatible with Corpus:
Catchment_descriptors$text = with(text, paste(NAME, TEXT, sep = " - "))

# Load the data as a corpus:
TextDoc <- Corpus(DataframeSource(text))
inspect(head(TextDoc))


# Cleaning up Text Data ---------------------------------------------------

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
  # Remove your own stop word
  # TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)


# Building the term document matrix ---------------------------------------

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descending value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 20 most frequent words
head(dtm_d, 20)

# Plot the most frequent words
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$word,
        col ="lightgreen", main ="Top 20 most frequent words",
        ylab = "Word frequencies")


# Generate the Word Cloud -------------------------------------------------

#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

findAssocs(TextDoc_dtm, 
           terms = c("groundwat","flashi","respons", "respond"),
           corlimit = 0.25)			

findAssocs(TextDoc_dtm,
           terms = findFreqTerms(TextDoc_dtm, lowfreq = 50),
           corlimit = 0.25)


# Analyse the emotion of the descriptions (obvs pointless) ----------------

d<-get_nrc_sentiment(text$text)
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")



# Actual analysis ---------------------------------------------------------


# Read the text file from local machine, choose file interactively:
Catchment_descriptors <- read.csv("../Data/Catchment_description_all.csv")

blank = 1:nrow(Catchment_descriptors)

Catchment_descriptors$TEXT <- gsub(pattern = " gw", replacement = " groundwater",
                         x = Catchment_descriptors$TEXT,  ignore.case = TRUE)
Catchment_descriptors$TEXT <- gsub(pattern = "gw ", replacement = "groundwater ",
                         x = Catchment_descriptors$TEXT,  ignore.case = TRUE)
Catchment_descriptors$TEXT <- gsub(pattern = "gw-", replacement = "groundwater ",
                                   x = Catchment_descriptors$TEXT,  ignore.case = TRUE)

Catchment_descriptors$groundwater <- blank %in% grep(pattern = "groundwater",
                                    x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$baseflow <- blank %in% grep(pattern = "baseflow",
                                 x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$permeable <- blank %in% grep(pattern = " permeable",
                                  x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$chalk <- blank %in% grep(pattern = "chalk",
                              x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$baseflow <- blank %in% grep(pattern = "baseflow",
                                 x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$spring <- blank %in% grep(pattern = "spring",
                                  x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$flashy <- blank %in% grep(pattern = "flashy",
                               x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$quick <- blank %in% grep(pattern = "quick",
                              x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$response <- blank %in% grep(pattern = "quick", 
                                 x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$response <- blank %in% grep(pattern = " response",
                                 x = Catchment_descriptors$TEXT, ignore.case = TRUE)

Catchment_descriptors$impermeable <- blank %in% grep(pattern = "impermeable",
                                  x = Catchment_descriptors$TEXT, ignore.case = TRUE)


write.csv(x = Catchment_descriptors, 
          file = "../Data/Catchment_description_all_with_counts 2.csv")


# Get catchment descriptors -----------------------------------------------

library(rnrfa)
cat = catalogue()
colnames(text)[1] <- "id"
cat = merge(x = cat, y = text, by = "id")
b = as.data.frame(cat)
write.csv(x = do.call(data.frame, cat),file = "../Data/Catchment_descriptors_with_counts.csv")
