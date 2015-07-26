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
options(mc.cores=1)

kBadWordUrl <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
kDataDir <- "data"
kDataSet <- "final"
kDataSampleSet <- "sample"
kLang <- "en_US"
kCourpusDir <- file.path(kDataDir, kDataSet, kLang)
kCourpusSampleDir <- file.path(kDataDir, kDataSampleSet, kLang)
kBadWords = "bad_words.txt"

GetCorpusFiles <- function(dir = kCourpusDir) {
  list.files(kCourpusDir, dir)
}

# GetFileStats <- function(dir = kCourpusDir) {
#   currentwd <- getwd()
#   setwd(kCourpusDir)
#   
#   counts <- system(paste("wc -lw "| awk '{ print $3, $1, $2 }'"), intern = TRUE)
#   print(counts)
#   m <- matrix(c("Name", "Lines", "Words"), ncol = 3)
#   print(m)
#   
#   for (line in counts) {
#     print("line")
#     print(line)
#     values <- strsplit(line, " ")
#     print(values)
#     m <- rbind(m, strsplit(line, " ")[[1]])
#   }
#   
#   m <- m[-1, ]
#   print(m)
#   files.corpus.stats <- as.data.frame(m, header=TRUE)
#   print(files.corpus.stats[1, ])
#   colnames(files.corpus.stats) <- c("Name", "Lines", "Words")
#   print(files.corpus.stats)
# #   print(class(out))
# #   print(out)
#   
#   sizes <- system(paste("ls -l", kDataFilePatter, "| awk '{ print $5 }'"), intern = TRUE)
#   sizes <- as.integer(sizes)
#   sizes <- round(c(sizes, sum(sizes)) / (2^20))
#   print(sizes)
#   
#   files.corpus.stats$Size <- sizes
#   print(files.corpus.stats)
#   
#   
#   setwd(currentwd)
#   files.corpus.stats
# }
# 
# GetFileStats("*.txt")

# remove
CleanAndTransformCorpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, tm::content_transformer(function(x) iconv(x, from = "UTF-8", to = "ASCII", sub = "")))
  corpus <- tm_map(corpus, tm::content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), readLines(kBadWords)), lazy = TRUE)
  # corpus <- tm_map(corpus, stemDocument, language = "english", lazy = TRUE)
  corpus <- tm_map(corpus, PlainTextDocument)
  return(corpus)
}

c <- tm::Corpus(DirSource(kCourpusSampleDir), readerControl = list(language = "en-US"))
c <- CleanAndTransformCorpus(c)

# print(findFreqTerms(tdm, lowfreq=10)) # find terms with a frequency higher than 10
# print(findAssocs(adtm, "usa",.5)) # just looking for some associations  
# # findAssocs(adtm, "china",.5)

ConvertTDMToCounts <- function(tdm, limit = 20) {
  matrix <- as.matrix(tdm)
  matrix.sums <- head(sort(rowSums(matrix), decreasing = TRUE), limit)
  df <- data.frame("Gram" = names(matrix.sums), "Count" = matrix.sums)
  df <- df[ order(-df$Count), ]
  return(df)
}

# tokenizers
# TokenizerUnigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
TokenizerBigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TokenizerTrigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

# generate TDMs
# tdm.unigram <- tm::TermDocumentMatrix(c, control = list(tokenize = TokenizerUnigram))
tdm.bigram <- TermDocumentMatrix(c, control = list(tokenize = TokenizerBigram))
tdm.trigram <- TermDocumentMatrix(c, control = list(tokenize = TokenizerTrigram))

# convert to term counts
# counts.unigram <- ConvertTDMToCounts(tdm.unigram)
counts.bigram <- ConvertTDMToCounts(tdm.bigram)
counts.trigram <- ConvertTDMToCounts(tdm.trigram)

