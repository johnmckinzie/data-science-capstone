# dataDir <- "final/en_US"
# con <- file(paste(dataDir, "en_US.twitter.txt", sep = '/'), "r") 
# 
# for (i in 1:2 ) {
#   line <- readLines(con, 1) ## Read the first line of text
#   line <- gsub("[!?.]", "", line)
#   print(line)
#   words <- strsplit(line, "\\s+")[[1]]
#   print(words)
# }
# 
# close(con) ## It's important to close the connection when you are done

library(tm)
a <- Corpus(DirSource("en_US_head"), readerControl = list(language="en-US"))
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removePunctuation)
a <- tm_map(a , stripWhitespace)
a <- tm_map(a, content_transformer(tolower))
a <- tm_map(a, removeWords, stopwords("english"))
# a <- tm_map(a, stemDocument, language = "english") 
# I also got it to work with stemming, but it takes so long...
adtm <-DocumentTermMatrix(a) 
adtm <- removeSparseTerms(adtm, 0.75)

inspect(adtm) 

findFreqTerms(adtm, lowfreq=10) # find terms with a frequency higher than 10
findAssocs(adtm, "usa",.5) # just looking for some associations  
findAssocs(adtm, "china",.5)

# Trigrams
# require(RWeka)
# TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# tdm <- TermDocumentMatrix(a, control = list(tokenize = TrigramTokenizer))
# tdm <- removeSparseTerms(tdm, 0.75)
# inspect(tdm[1:5,1:5])