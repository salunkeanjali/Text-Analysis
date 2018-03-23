# Text analysis using tidytext and dplyr in R using Jeopardy dataset

# install tidytext and dplyr packages
install.packages("tidytext", repos = "https://cran.r-project.org")
install.packages("dplyr", repos = "https://cran.r-project.org")
install.packages("ggplot2", repos = "https://cran.r-project.org")

# load libraries
library(dplyr)
library(tidytext)
library(ggplot2)

# change the working directory using setwd() 

setwd("C:/Users/anjal/Desktop")

# read the Jeopardy dataset
jeopardyData <- read.csv('JEOPARDY_CSV.csv', stringsAsFactors = FALSE)

# extracting the Question column into a dataset
questionData <- jeopardyData$Question

# convert the data to a data frame
text_df <- data_frame(line = 1:216930, text = questionData)
head(text_df)

# tokenize with standard tokenization using unnext_tokens from tidytext
token_data <- unnest_tokens(text_df, word, text)

# remove stop-words using anti_join function from dplyr
# stop_words come from tidytext package
token_data <- anti_join(token_data, stop_words)

# use the count() function of dplyr to view most common words
wordcount <- count(token_data,word, sort = TRUE)

# filter for n>5000 using filter function from dplyr
wordcountfiltered <- filter(wordcount, n > 2000)

# visualize with ggplot
ggplot(wordcountfiltered, aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


# the steps from count to ggplot can be combined using the piping operator %>%

token_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()


# Word Cloud of Question from Jeopardy
# Source: https://datascienceplus.com/building-wordclouds-in-r/
# Source: http://onepager.togaware.com/TextMiningO.pdf

# install appropriate packages
install.packages("tm", repos="https://cran.r-project.org")
install.packages("SnowballC", repos="https://cran.r-project.org")
install.packages("wordcloud", repos="https://cran.r-project.org")
install.packages("RColorBrewer", repos="https://cran.r-project.org")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

questionCorpus <- Corpus(VectorSource(jeopardyData$Question))

questionCorpus <- tm_map(questionCorpus, content_transformer(tolower))

questionCorpus <- tm_map(questionCorpus, removePunctuation)
questionCorpus <- tm_map(questionCorpus, PlainTextDocument)
questionCorpus <- tm_map(questionCorpus, removeWords, stopwords('english'))
questionCorpus <- tm_map(questionCorpus, stemDocument)

questionCorpus <- Corpus(VectorSource(questionCorpus))

wordcloud(questionCorpus, max.words = 50, random.order = FALSE, colors = brewer.pal(6, "Dark2"))
