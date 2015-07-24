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
library(RWeka)
# library(parallel)
options(mc.cores=1)

kBadWordUrl <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
kBadWordDest <- "bad_words.txt" # https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
kDataDir <- "data"
kDataSet <- "sample"
kLang <- "en_US"
kCourpusDir <- file.path(kDataDir, kDataSet, kLang)


GetBadWords <- function() {
  download.file(kBadWordUrl, kBadWordDest)
}

GetCorpusFiles <- function() {
  list.files(kCourpusDir)
}

files.corpus <- GetCorpusFiles()
print(files.corpus)

GetFileStats <- function() {
  files.corpus.stats <- data.frame(Date=as.Date(character()),
  File=character(), 
  User=character(), 
  stringsAsFactors=FALSE) 
  files.corpus.sizes <- lapply(files.corpus, function(file) {
    path <- paste(kCourpusDir, file, sep = "/")
    out <- system(paste("wc -lw", path), intern = TRUE)
    out <- strsplit(out," ")
    print(out)
  })
}

GetFileStats()

# remove
CleanAndTransformCorpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), readLines(kBadWordDest)), lazy = TRUE)
  corpus <- tm_map(corpus, stemDocument, language = "english", lazy = TRUE)
  corpus <- tm_map(corpus, PlainTextDocument)
  return (corpus)
}


library(tm)
c <- Corpus(DirSource(kCourpusDir), readerControl = list(language = "en-US"))
c <- CleanAndTransformCorpus(c)

# adtm <-DocumentTermMatrix(a) 
# adtm <- removeSparseTerms(adtm, 0.75)
# 
# inspect(adtm) 
# 
# # print(findFreqTerms(adtm, lowfreq=10)) # find terms with a frequency higher than 10
# print(findAssocs(adtm, "usa",.5)) # just looking for some associations  
# # findAssocs(adtm, "china",.5)

# tokenizers
TokenizerUnigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
TokenizerBigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TokenizerTrigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

# generate TDMs
tdm.unigram <- TermDocumentMatrix(c, control = list(tokenize = TokenizerUnigram))
tdm.bigram <- TermDocumentMatrix(c, control = list(tokenize = TokenizerBigram))
tdm.trigram <- TermDocumentMatrix(c, control = list(tokenize = TokenizerTrigram))

print(tdm.trigram[1:10, ])
# tdm <- removeSparseTerms(tdm, 0.75)
inspect(tdm.trigram[1:10, ])
