dataDir <- "final/en_US"
con <- file(paste(dataDir, "en_US.twitter.txt", sep = '/'), "r") 

for (i in 1:2 ) {
  line <- readLines(con, 1) ## Read the first line of text
  line <- gsub("[!?.]", "", line)
  print(line)
  words <- strsplit(line, "\\s+")[[1]]
  print(words)
}

close(con) ## It's important to close the connection when you are done