
#------------------------------------------------------------------------------
#                         SET-UPS & EXPLORATION
#------------------------------------------------------------------------------
setwd("~/Dropbox/Warwick/CS909_Data_Mining/ReutersCoursework")
reuters   <- read.csv("reutersCSV.csv", stringsAsFactors = FALSE)
reuters   <- data.frame(reuters)
reuters[,140] <- paste(reuters[,139] ,reuters[,140],sep = " ")
#------------------------------------------------------------------------------
notused   <- reuters[reuters$purpose == "not-used",]
test      <- reuters[reuters$purpose == "test",]
train     <- reuters[reuters$purpose == "train",]
reuters20 <- reuters[sample(nrow(reuters),20), ]
reuters20 <- (reuters20[,c(1,139,140)])
#------------------------------------------------------------------------------

# Checking against numbers in readme.txt->VII.Categories
csum <- data.frame(colSums(reuters[,4:138]))
# Expect Number of Categories = 135
sum(table(csum)) # TRUE
# Expect Number of Categories w/ 1+ Occurrences = 120
sum(table(csum)[2:nrow(table(csum))]) # FALSE : get 118 (which is -2)
# Expect Number of Categories w/ 20+ Occurrences = 57
#table(csum)[16] # get (18,1)
#table(csum)[17] # get (21,2)
sum(table(csum)[17:nrow(table(csum))]) # FALSE : get 56 (which is -1)

# TRUE NEGATIVES
# Identifying the documents which have no corresponding TOPICs
rsum <- rowSums(reuters[,4:138])
emptydocs <- cbind(reuters$pid,reuters$doc.title,rsum)
emptydocs <- emptydocs[emptydocs[,3]==0,1:2] # There are 20476 such documents

# Identifying the TOPICs which have no corresponding documents
emptytopics <- cbind(colnames(reuters[,4:138]),csum)
emptytopics <- emptytopics[emptytopics[,2]==0,] # There are 17 such TOPICs

# Table to show purpose vs. number of topics assigned
table(cbind(data.frame(reuters$purpose),rsum))

# Table to show purpose vs. missing documents
charlen <- CharLen(reuters[,140])
charlen <- cbind(charlen,reuters[,3])
table(charlen[charlen[,1]==0,]) # There are 1799 such DOC.TEXTs

# Graph to visualise frequency of length of documents
tmp <- cbind(charlen[order(charlen[,1]),],1:nrow(charlen))
plot(tmp[tmp[,2]=="train",c(1,3)], col=rgb(0,0,1,0.5), pch=20)
points(tmp[tmp[,2]=="test",c(1,3)], col=rgb(0,1,0,0.5), pch=20)
points(tmp[tmp[,2]=="not-used",c(1,3)],  col=rgb(1,0,0,0.5), pch=20)

# Looking at top 100 misspelt words - intent to spellcheck
#library("qdap")
misspeltWords <- vector()
for (i in 1:nrow(reuters)) { 
  misspeltWords <- c(misspeltWords,(which_misspelled(as.character(reuters[i,140]),suggest=FALSE)))
}
misspeltWords <- data.frame(table(misspeltWords))
misspeltWords[misspeltWords$Freq>=150,] # 'misspelt' words with frequency of occurence > 150
table(as.vector(misspeltWords[,2])) # frequency of frequency of recurring misspellings
# Will not implement this by creating a dictionary:
# would choose to spellcheck outside R
# assume high frequency 'misspellings' are actually correct -
#   want to correct low frequency 'misspellings' i.e. 'mistakes'
#   there are too many of these to do manually be creating dictionary

#------------------------------------------------------------------------------
#                         PRE-PROCESS
#------------------------------------------------------------------------------

source("01_preprocess.R")

# Load data set.
reuters   <- read.csv("reutersCSV.csv", stringsAsFactors = FALSE)
reuters   <- data.frame(reuters)
reuters   <- reuters[-12793,]

# Check that all TOPICs only have values {0,1} - (no missing values).
length(OneOrZeroCheck(reuters[,4:138])) == 0

# Check that the text columns have valid input.
tmp <- Security(reuters[,139:140]) # There are columns with invalid input so...
reuters[,139:140] <- ShellcodeToSpace(reuters[,139:140],tmp) # ...remove the invalid substrings.
rm(tmp)

# Concatenate the document titles and document text.
reuters[,140] <- paste(reuters[,139] ,reuters[,140],sep = " ")

# Clean for Part Of Speech Tagging.
reuters140 <- DFtoCP(reuters[,140])
reuters140 <- PassTM(reuters140)

# Part Of Speech Tagging.
  # Use default models.
sentence_annotator  <- Maxent_Sent_Token_Annotator()
word_annotator      <- Maxent_Word_Token_Annotator()
pos_annotator       <- Maxent_POS_Tag_Annotator()
  # Replace dates with the number 5 and whitespace.

poslist <- PosList(reuters140,,sentence_annotator,word_annotator,pos_annotator)
tmp <- entity_replace(reuters140,poslist) 
  # Extract the annotator objects and documents respectively.
pos140 <- tmp[[1]]
reuters140 <- LISTtoDF(tmp[2])
length(reuters140)==length(pos140) # check same size
  # Replace the number 5 with the word "isdate".
reuters140 <- DFtoCP(reuters140)
reuters140 <- tm_map(reuters140, content_transformer(Round3_Markers))
rm(tmp)

# Ready for Bag-Of-Words.
  # Stem the words in the document.
reuters140 <- tm_map(reuters140, stemDocument)

# stemCompletion not implemented:
#   stemCompletion not a default transformation in tm v0.6 - getTransformations()
#   regardless of implementation, it will not affect our analysis

#------------------------------------------------------------------------------
#                     FEATURE ENGINEERING & SELECTION
#------------------------------------------------------------------------------

setwd("~/Dropbox/Warwick/CS909_Data_Mining/ReutersCoursework")
source("00_functionStore.R")
source("01_preprocess.R")
source("02_features.R")

# Load pre-processed data.
reuters <- readRDS("reuters_preprocessed.rds")
pos140 <- readRDS("pos140_preprocessed.rds")
reuters140 <- readRDS("reuters140_preprocessed.rds")
numdocs <- 21577

# Check there are no documents of length 0.
tmp <- CPtoDF(reuters140)
length(which(tmp=="EMPTY"))==0
tmp <- CharLen(CPtoDF(reuters140))
length(which(tmp==0))==0
# There were no such documents, 
# otherwise they should be removed from the corpus before further analysis.
rm(tmp)


# Bag-Of-Words approach.
# Build a binary Document-Term Matrix
binarydtm <- DocumentTermMatrix(reuters140,control=list(weighting=weightBin))
# Discard sparse words
freq_topic <- data.frame(colSums(reuters[,4:138]))
bound <- freq_topic[order(freq_topic,decreasing = TRUE)[10],] # 253
findFreqTerms(binarydtm, bound) # gives 826 length dictonary (2.5% of words kept)
bound <- mean(freq_topic[,1]) #105.407407407
findFreqTerms(binarydtm, bound) # gives 1519 length dictionary (4.7% of words kept)
# Remove those terms which have at least a 99.5% percentage of sparse.
# (i.e., terms occurring 0 times in a document).
binarydtm <- removeSparseTerms(binarydtm, 1-(bound/numdocs))
binarymat <- as.matrix(binarydtm)
binarydf <- data.frame(binarymat)
# These are the top 10 most populous classes.
colnames(reuters)[order(freq_topic,decreasing = TRUE)[1:10]+3]
# Append the bag-of-words into a data frame.
binarydf <- cbind(reuters$pid,
                  reuters$purpose,
                  CPtoDF(reuters140),
                  reuters[,order(freq_topic,decreasing = TRUE)[1:10]+3],
                  binarydf)
# Reduce TOPIC assignments.
for (i in 1:numdocs) {
  while (sum(binarydf[i,4:13]) > 1) {
    binarydf[i,sample(4:13,1)] <- 0
  }
  cat("Row ",i,"now has single TOPIC.\n")
}
# Since relevant topic labels were removed,
# a new column is added to identify those documents that
# are true negatives for all the topic labels.
binarydf$topics.none <- rowSums(binarydf[,4:13])
binarydf$topics.none <- 1%%(2-binarydf$topics.none)
binarydf <- cbind(binarydf[,1:14],
                  binarydf[,ncol(binarydf)],
                  binarydf[,15:ncol(binarydf)-1])
# Tidy up.
rm(binarydtm,binarymat,bound,freq_topic)



# Latent Dirichlet Allocation approach.
# Build a binary Document-Term Matrix
freqdtm <- DocumentTermMatrix(reuters140,control=list(weighting=weightTf))
freqmat <- as.matrix(freqdtm)
summary(colSums(freqmat))
# Min.     1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.00      1.00      2.00     55.22      8.00 135800.00
tfidfdtm <- DocumentTermMatrix(reuters140,control=list(weighting=weightTfIdf))
tfidfmat <- as.matrix(tfidfdtm)
tmpdc <- apply(tfidfmat,2,mean)
summary(tmpdc)  # Min.      1st Qu.    Median      Mean   3rd Qu.      Max. 
              # 4.720e-07 8.779e-06 1.962e-05 1.419e-04 5.588e-05 3.158e-02
tmpdc <- as.numeric(which(tmpdc>1.96e-05))
tmpdc <- colnames(tfidfmat)[tmpdc]
tmpcp <- reuters140
for (i in 1:floor(length(tmpdc)/1000)) {
  cat("Running ",i,"\n")
  tmpcp <- tm_map(tmpcp, removeWords, tmpdc[1+1000*(i-1):1000*i])
}
tmpcp <- tm_map(tmpcp, removeWords, tmpdc[(length(tmpdc)-length(tmpdc)%%1000+1):length(tmpdc)])
write.csv(CPtoDF(tmpcp),file="LDAtexts.csv")
tmpcp <- read.csv("LDAtexts.csv")
tmpcp <- DFtoCP(tmpcp[,2])
freqdtm <- DocumentTermMatrix(tmpcp,control=list(weighting=weightTf))
freqmat <- as.matrix(freqdtm)
summary(colSums(freqmat))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.00      1.00      2.00     54.79      8.00 135800.00 

tmpcp <- read.csv("LDAtexts.csv")
tmpcp <- DFtoCP(tmpcp[sample(1:21578,100),2])
freqdtm <- DocumentTermMatrix(tmpcp,control=list(weighting=weightTf))
freqmat <- as.matrix(freqdtm)





#------------------------------------------------------------------------------
#                              CLASSIFIERS
#------------------------------------------------------------------------------



