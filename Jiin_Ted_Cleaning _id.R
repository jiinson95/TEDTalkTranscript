
# library setup
library(dplyr)
library(data.table)
library(ngram)
library(qdap)
library(stringr)

###############################################################################
# join dataset
###############################################################################
# read dataset
ted_main <- read.csv("G:\\My Drive\\MSA\\TextAnalytics\\ted_main.csv")
transcripts <- read.csv("G:\\My Drive\\MSA\\TextAnalytics\\transcripts.csv")

# join by url
ted <- inner_join(ted_main, transcripts)

# write csv file
write.csv(ted,"G:\\My Drive\\MSA\\TextAnalytics\\ted.csv", row.names = FALSE)

##############################################################################
# cleaning tags
##############################################################################
# read dataset
ted <- read.csv("G:\\My Drive\\MSA\\TextAnalytics\\ted.csv")

# having a unique key
ted$ID <- seq.int(nrow(ted))
write.csv(ted,"G:\\My Drive\\MSA\\TextAnalytics\\ted.csv", row.names = FALSE)

# library setup
library(rlist)
max(ted$ID)
# check weather all the rows contains '['
sum(substr(ted$tags, 1, 1) == '[')

# convert tags as character
ted$tags <- as.character(ted$tags)

# remove []
for (i in 1:length(ted$tags)){
  ted$clean.tags <- substr(ted$tags, 2, nchar(ted$tags)-1)
}

# create a tag_list
tag_list <- c()
for (i in 1:length(ted$clean.tags)){
  tag_list <- paste(tag_list, ',', ted$clean.tags[i])
}
tag_list <- substr(tag_list, 4, nchar(tag_list))

# make a list of unique tags
test <- as.list(unlist(strsplit(tag_list, ', ')))
test <- trimws(test)
tag_list <- unique(test)

# create binary columns 
# create column with 0 values
for (j in 1:length(tag_list)){
  ted[[paste(tag_list[j], sep = '')]] <- 0
}

# plug 1 if the tag exists 
for (i in 1:length(ted$tags)){
  for(j in 1:length(tag_list)){
    if (grepl(tag_list[j], ted$tags[i], fixed = TRUE)){
      ted[[i, paste(tag_list[j], sep = '')]] <-1}
  }
}

# write csv file
# write.csv(ted,"G:\\My Drive\\MSA\\TextAnalytics\\ted_binary.csv", row.names = FALSE)

######################################################################################
# find the audience response and put them in the list for now
######################################################################################
response <- c()
for (i in 1:length(doc)){
  if (grepl('[()]', doc[i])){
    response <- list.append(response, 
                            substring(doc[i], gregexpr(pattern = '[()]', doc[i])[[1]][1],
                                      gregexpr(pattern = '[()]', doc[i])[[1]][2]))
  }
}
response <- trimws(response)
response <- unique(response)

# remove NA
for (i in 1:length(response)){
  if(is.na(response[i])){
    response <- response[-i]
  }
}
sum(is.na(response)) # to check

# remove element that is starting with ')'
response_test <- response
sum(substr(response_test,1,1) == ")") #11
for (i in 1:length(response_test)){
  if(substr(response_test[i], 1, 1) == ")"){
    response_test <- response_test[-i]
  }
}
which(substr(response_test,1,1) == ")") #1: unfortunately remainder left
response_test[75]
response_test <- response_test[-75]
sum(substr(response_test,1,1) == ")") #0 <- confirmed
response <- response_test

# write list of unique audience response 
write.csv(response,"G:\\My Drive\\MSA\\TextAnalytics\\response.csv", row.names = FALSE)


################################################################################
# Transcripts Cleaning Process
################################################################################

# read data
ted <- read.csv("G:\\My Drive\\MSA\\TextAnalytics\\ted.csv")
trans <- ted[,c('ID', 'transcript')]
trans$transcript <- as.character(trans$transcript)


# library setup
library(stringr) # str_replace_all
library(NLP) #VCorpus
library(tm) #tm_map
library(rlist) #list.append
library(tidyr)

# breakdown transcript into sentense 
# need to consider "U.S", "U.N.", "(Appluse)This is ..." --> ". "
# doc_list <- str_split(trans, "[.]") # this makes list of list (nested)
trans$transcript <- str_split(trans$transcript, fixed(". ")) 

doc <- c()
for (i in 1:length(trans$transcript)){
  for(j in 1:length(trans$transcript[[i]])){
    doc <- list.append(doc, paste(trans$ID[i], '+', trans$transcript[[i]][j]))
  }  
}

write.csv(doc,"G:\\My Drive\\MSA\\TextAnalytics\\doc_id.csv", row.names = FALSE)

# split ID and transcript
id <-c()
transcript <- c()
for (i in 1:length(doc)){
  id[i] <- substr(doc[i], 1, gregexpr('[+]', doc[i])[[1]][1]-2)
}
for (i in 1:length(doc)){
  transcript[i] <- substr(doc[i], gregexpr('[+]', doc[i])[[1]][1]+2, nchar(doc[i]))
}

# create dataframe
df <- data.frame(matrix(NA, nrow = 238345, ncol = 2))
x <- c('ID', 'transcript')
colnames(df) <- x
df$ID <- id
df$transcript <- transcript
df$transcript[2]

# write csv file of doc list in case I lose it 
write.csv(df,"G:\\My Drive\\MSA\\TextAnalytics\\df.csv", row.names = FALSE)

##########################################################################################
# remove punctuation using doc sentence list
df$transcript <- str_replace_all(df$transcript, '[[:punct:]]', ' ')
# convert the text to lower case
df$transcript <- tolower(df$transcript)
df$transcript <- trimws(df$transcript)

# replace funcky symbols
df$transcript <- str_replace_all(df$transcript, 'â???"', ' ')
df$transcript <- str_replace_all(df$transcript, 'â???', ' ')
df$transcript <- str_replace_all(df$transcript, 'âT«', ' ')
df$transcript <- str_replace_all(df$transcript, 'âT', ' ')

# remove Numbers (no need to consider decimal)
df$transcript <- removeNumbers(df$transcript)
df$transcript[784]

# write csv file  
write.csv(df,"G:\\My Drive\\MSA\\TextAnalytics\\df_rem.csv", row.names = FALSE)
df <- read.csv("G:\\My Drive\\MSA\\TextAnalytics\\df_rem.csv")
##########################################################################################
# stop words
# define common English stopwords
stopwords <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
               "can", "like", "just", "get", 
  "a",	"about",	"above",	"after",	"again",	"against",	"all",	"am",	"an",	"and",	"any",	"are",	"aren't",	"as",	"at",	"be",	"because",	"been",	"before",	"being",	"below",	"between",	"both",	"but",	"by",	"can't",	"cannot",	"could",	"couldn't",	"did",	"didn't",	"do",	"does",	"doesn't",	"doing",	"don't",	"down",	"during",	"each",	"few",	"for",	"from",	"further",	"had",	"hadn't",	"has",	"hasn't",	"have",	"haven't",	"having",	"he",	"he'd",	"he'll",	"he's",	"her",	"here",	"here's",	"hers",	"herself",	"him",	"himself",	"his",	"how",	"how's",	"i",	"i'd",	"i'll",	"i'm",	"i've",	"if",	"in",	"into",	"is",	"isn't",	"it",	"it's",	"its",	"itself",	"let's",	"me",	"more",	"most",	"mustn't",	"my",	"myself",	"no",	"nor",	"not",	"of",	"off",	"on",	"once",	"only",	"or",	"other",	"ought",	"our",	"ours",	"ourselves",	"out",	"over",	"own",	"same",	"shan't",	"she",	"she'd",	"she'll",	"she's",	"should",	"shouldn't",	"so",	"some",	"such",	"than",	"that",	"that's",	"the",	"their",	"theirs",	"them",	"themselves",	"then",	"there",	"there's",	"these",	"they",	"they'd",	"they'll",	"they're",	"they've",	"this",	"those",	"through",	"to",	"too",	"under",	"until",	"up",	"very",	"was",	"wasn't",	"we",	"we'd",	"we'll",	"we're",	"we've",	"were",	"weren't",	"what",	"what's",	"when",	"when's",	"where",	"where's",	"which",	"while",	"who",	"who's",	"whom",	"why",	"why's",	"with",	"won't",	"would",	"wouldn't",	"you",	"you'd",	"you'll",	"you're",	"you've",	"your",	"yours",	"yourself",	"yourselves")
stopwords <- str_replace_all(stopwords, "'", ' ')
# define uncommon English stopwords
stopwords_1 <- c("ain't",	"aren't",	"can't",	"can't've",	"'cause",	"could've",	"couldn't",	"couldn't've",	"didn't",	"doesn't",	"don't",	"hadn't",	"hadn't've",	"hasn't",	"haven't",	"he'd",	"he'd've",	"he'll",	"he'll've",	"he's",	"how'd",	"how'd'y",	"how'll",	"how's",	"I'd",	"I'd've",	"I'll",	"I'll've",	"I'm",	"I've",	"i'd",	"i'd've",	"i'll",	"i'll've",	"i'm",	"i've",	"isn't",	"it'd",	"it'd've",	"it'll",	"it'll've",	"it's",	"let's",	"ma'am",	"mayn't",	"might've",	"mightn't",	"mightn't've",	"must've",	"mustn't",	"mustn't've",	"needn't",	"needn't've",	"o'clock",	"oughtn't",	"oughtn't've",	"shan't",	"sha'n't",	"shan't've",	"she'd",	"she'd've",	"she'll",	"she'll've",	"she's",	"should've",	"shouldn't",	"shouldn't've",	"so've",	"so's",	"that'd",	"that'd've",	"that's",	"there'd",	"there'd've",	"there's",	"they'd",	"they'd've",	"they'll",	"they'll've",	"they're",	"they've",	"to've",	"wasn't",	"we'd",	"we'd've",	"we'll",	"we'll've",	"we're",	"we've",	"weren't",	"what'll",	"what'll've",	"what're",	"what's",	"what've",	"when's",	"when've",	"where'd",	"where's",	"where've",	"who'll",	"who'll've",	"who's",	"who've",	"why's",	"why've",	"will've",	"won't",	"won't've",	"would've",	"wouldn't",	"wouldn't've",	"y'all",	"y'all'd",	"y'all'd've",	"y'all're",	"y'all've",	"you'd",	"you'd've",	"you'll",	"you'll've",	"you're",	"you've")
stopwords_1 <- str_replace_all(stopwords_1, "'", ' ')

# create volatile corpora
df_test <- df
df_test$transcript <- Corpus(VectorSource(df_test$transcript))
df_test$transcript[1]
# inspect the content of the document
inspect(df_test$transcript[1:5])

# Remove common English stopwords
df_test$transcript <- tm_map(df_test$transcript, removeWords, stopwords)
# Removing contractions since they are generally made of stop words
df_test$transcript <- tm_map(df_test$transcript, removeWords, stopwords_1)
inspect(df_test$transcript[1:5])

# dataframe <- data.frame(text=sapply(doc_stop, identity),stringsAsFactors=F)
# df <- data.frame(text = get("content", doc_stop), stringAsFactors = F)

# write.csv(dataframe,"G:\\My Drive\\MSA\\TextAnalytics\\stopwords.csv", row.names = FALSE)
# df <- data.frame(text = get("content", doc_stop), stringAsFactors = F)
# Eliminate extra white spaces
df_test$transcript <- tm_map(df_test$transcript, stripWhitespace)
inspect(df_test$transcript[1:5])

#The following code was written to remove the most common words after looking at the most common words after stemming that did not provide significant interpretation
# remainder <- c()
###################################################################################################
# stemming

#Creating a set of non-stemmed words for a word cloud (in case we make word cloud later)
docsnostem <- df_test
df_nostem <- data.frame(matrix(NA, nrow = 238345, ncol = 2))
x <- c('ID', 'transcript')
colnames(df_nostem) <- x
df_nostem$ID <- docsnostem$ID
df_nostem$transcript <- docsnostem$transcript$content
# write csv file for the no steming version
write.csv(df_nostem,"G:\\My Drive\\MSA\\TextAnalytics\\df_nostem.csv", row.names = FALSE)


library(SnowballC)
# Text stemming using Porter stemming
df_test$transcript <- tm_map(df_test$transcript, stemDocument)
test <- df_test$transcript$content

# FINAL MASTERPIECE
df_final <- data.frame(matrix(NA, nrow = 238345, ncol = 2))
x <- c('ID', 'transcript')
colnames(df_final) <- x
df_final$ID <- df_test$ID
df_final$transcript <- df_test$transcript$content
df_final$transcript[2]

# write csv file for final 
write.csv(df_final,"G:\\My Drive\\MSA\\TextAnalytics\\df_final.csv", row.names = FALSE)



