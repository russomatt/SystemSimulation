# Matthew Russo
# Sytem Simulation Final
# Text Analysis of Presidential Debate between Hillary Clinton and Donald Trump looking for clarity

# citation for data:
# 2016 US Presidential Debate Github Repository. Retrieved July 10, 2017, from https://github.com/WiMLDS/election-data-hackathon/tree/master/2016-us-presidential-debates

##############################################################################################################
# import and download the packages

pacman::p_load(pacman, tm, SnowballC, dplyr, wordcloud, RColorBrewer)

##############################################################################################################
# Import the files

# The debate as a csv
debateCSV <- read.csv('/Users/matt/Desktop/hsrw/system_simulation/final/debate.csv')

# Split the csv into subsets of speakers 
debateClinton <- subset(debateCSV, debateCSV[,1]=="Clinton", select="Text")
debateTrump <- subset(debateCSV, debateCSV[,1]=="Trump", select="Text")

# output new csv
write.csv(debateClinton[1:50, ], "/Users/matt/Desktop/hsrw/system_simulation/final/debateClinton.csv")
write.csv(debateTrump[1:53, ], "/Users/matt/Desktop/hsrw/system_simulation/final/debateTrump.csv")

# write lines 
linesC <- readLines('/Users/matt/Desktop/hsrw/system_simulation/final/debateClinton.csv')
linesT <- readLines('/Users/matt/Desktop/hsrw/system_simulation/final/debateTrump.csv')

##############################################################################################################
# Create Corpuses for the date files

corpusC <- Corpus(VectorSource(linesC)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

corpusT <- Corpus(VectorSource(linesT)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

# Create a term-document matrices and remove sparse terms
tdmC <- DocumentTermMatrix(corpusC) %>%
  removeSparseTerms(1 - (4/length(corpusT)))

tdmT <- DocumentTermMatrix(corpusT) %>%
  removeSparseTerms(1 - (4/length(corpusT)))

# Sort the words by frequency
word.freqC <- sort(colSums(as.matrix(tdmC)))
word.freqT <- sort(colSums(as.matrix(tdmT)))

# Generate frequency table
tableC <- data.frame(word = names(word.freqC),
                      absolute.frequency = word.freqC,
                      relative.frequency = word.freqC/length(word.freqC))

tableT <- data.frame(word = names(word.freqT),
                      absolute.frequency = word.freqT,
                      relative.frequency = word.freqT/length(word.freqT))

# Set the row names to null
row.names(tableC) <- NULL
row.names(tableT) <- NULL

##############################################################################################################
# Draw first round of word clouds

set.seed(20)

wordcloud(names(word.freqC),word.freqC,min.freq=4,colors=brewer.pal(3,"Dark2"))
wordcloud(names(word.freqT),word.freqT,min.freq=4,colors=brewer.pal(3,"Dark2"))

##############################################################################################################
# Remove function words as well as some interjections such as well or also

# Words chosen to be removed:
# see, like, know, said, lester (name of the presenter), things, think, well, say, almost, clinton, ever, now, 
# much, don’t, just, want, can’t, you’re, thing, that’s, look, will, they’re, going, first, we’ve, i’ve, let’s,
# ever, back, also, actually, don’t, said, just, make, really, want, see, look, donald, get, that’s, will, can, 
# going, think, well

# create vector with word positions
vC <- c(1,6,8,11,12,16,19,22,23,24,28,30,33,34,35,37,38,42,44,45,49,50,52,53) # 20 

vT <- c(1,2,5,6,7,8,9,11,12,14,15,16,17,19,20,21,22,24,25,26,29,32,34,35) # 22

word.freqC2 <- word.freqC
word.freqT2 <- word.freqT

# remove vector of words from frequency table
word.freqC2 <- word.freqC2[-vC]
word.freqT2 <- word.freqT2[-vT]

##############################################################################################################
# Draw clouds again with less filler words

wordcloud(names(word.freqC2),word.freqC,min.freq=4,colors=brewer.pal(3,"Dark2"))
wordcloud(names(word.freqT2),word.freqT,min.freq=4,colors=brewer.pal(3,"Dark2"))

##############################################################################################################
# Clear

rm(list = ls())

