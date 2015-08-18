library(tm)

if (!exists("unigram.final")) {
  print("loading data")
  load("model_data/unigrams.RData")
  load("model_data/bigrams.RData")
  load("model_data/trigrams.RData")
  load("model_data/quadgrams.RData")
}

WordPredict <- function(sentence = "") {
  sentence <- removePunctuation(sentence)
  sentence <- removeNumbers(sentence)
  print(sentence)
  words <- unlist(strsplit(sentence, " "))
  words <- setdiff(words, stopwords(kind = "en-US"))
  words <- tolower(words)
  print("WORDS")
  print(class(words))
  print(words)
  words.count <- length(words)
  print(words.count)
  
  suggestions <- c()
  
  PredictUnigram <- function() {
    unigram.final[1, ][[1]]
  }
  
  PredictBigram <- function(word) {
    bigram.final[(bigram.final$Gram_1 == word), ]
  }

  PredictTrigram <- function(word1, word2) {
    trigram.final[(trigram.final$Gram_1 == word1) & (trigram.final$Gram_2 == word2), ]
  }  

  if (words.count > 2) {
    words <- tail(words, 2)
    words.count <- length(words)
  }
      
  if (words.count == 0) {
    print("0 unigram")
#     print(head(unigram.final))
    suggestions.unigram <- PredictUnigram()
    return(suggestions.unigram)
  } else if (words.count == 1) {
    print(words[[1]])
#     print(head(bigram.final))
    suggestions.bigram <- PredictBigram(words[[1]])

    if (nrow(suggestions.bigram) > 0) {
      print("1 bigram")
#       print(suggestions.bigram)
      return(suggestions.bigram[1, ]$Gram_2)
    } else {
      print("1 unigram")
      return(PredictUnigram())
    }
  } else if (words.count == 2) {
    print(words[1:2])
#     print(head(trigram.final))
    suggestions.trigram <- PredictTrigram(words[[1]], words[[2]])
#     print(suggestions.trigram)
    
    if (nrow(suggestions.trigram) > 0) {
      print("2 trigram")
      return(suggestions.trigram[1, ]$Gram_3)
    } else {
      print("2 bigram")
      suggestions.bigram <- PredictBigram(words[[1]])
      
      if (nrow(suggestions.bigram) > 0) {
        print("2 bigram")
        return(suggestions.bigram[1, ]$Gram_2)
      } else {
        print("2 unigram")
        return(PredictUnigram())
      }    
    }    
  }
}