library(tm)
library(RWeka)
library(splitstackshape)

GenerateData <- function() {
  kDataDir <- "data"
  kDataSet <- "final"
  kDataSampleSet <- "sample"
  kLang <- "en_US"
  kCourpusDir <- file.path(kDataDir, kDataSet, kLang)
  kCourpusSampleDir <- file.path(kDataDir, kDataSampleSet, kLang)
  kBadWords = "bad_words.txt"
  options(mc.cores=1)
  
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
  
  ConvertTDMToCounts <- function(tdm, limit = 20) {
    matrix <- as.matrix(tdm)
    matrix.sums <- sort(rowSums(matrix), decreasing = TRUE)
    df <- data.frame("Gram" = names(matrix.sums), "Count" = matrix.sums)
    df <- df[ order(-df$Count), ]
    return(df)
  }

  # unigrams
  TokenizerUnigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
  unigram.tdm <- tm::TermDocumentMatrix(c, control = list(tokenize = TokenizerUnigram))
  unigram.tdm <- removeSparseTerms(unigram.tdm, 0.9999)
  unigram.final <- ConvertTDMToCounts(unigram.tdm)
  save(unigram.final, file = "model_data/unigrams.RData")
  
  # bigrams
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  bigram.tdm <- tm::TermDocumentMatrix(c, control = list(tokenize = BigramTokenizer))
  bigram.tdm <- removeSparseTerms(bigram.tdm, 0.9999)
  bigram.counts <- ConvertTDMToCounts(bigram.tdm)
  bigram.final <- cSplit(bigram.counts, "Gram", sep = " ", drop = FALSE)
  save(bigram.final, file = "model_data/bigrams.RData")
  
  # trigrams
  TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
  trigram.tdm <- tm::TermDocumentMatrix(c, control = list(tokenize = TrigramTokenizer))
  trigram.tdm <- removeSparseTerms(trigram.tdm, 0.9999)
  trigram.counts <- ConvertTDMToCounts(trigram.tdm)
  trigram.final <- cSplit(trigram.counts, "Gram", sep = " ", drop = FALSE)
  save(trigram.final, file = "model_data/trigrams.RData")
  
  # trigrams
  QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
  quadgram.tdm <- tm::TermDocumentMatrix(c, control = list(tokenize = QuadgramTokenizer))
  quadgram.tdm <- removeSparseTerms(quadgram.tdm, 0.9999)
  quadgram.counts <- ConvertTDMToCounts(quadgram.tdm)
  quadgram.final <- cSplit(quadgram.counts, "Gram", sep = " ", drop = FALSE)
  save(quadgram.final, file = "model_data/quadgrams.RData")

  gc()
}