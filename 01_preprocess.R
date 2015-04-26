#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

library("tm")
library("NLP")
library("openNLP")
library("openNLPdata")
library("openNLPmodels.en")
library("stringr")


#------------------------------------------------------------------------------

# Takes a column or row of a data frame (default assumes a column) and
# returns the number of characters in each entry of the input in a data frame.
# If there is a (multibyte) error, then it returns a count of -1.
CharLen <- function(df,entering = c("COLUMN","ROW")) {
  insert = switch(match.arg(entering),
                  COLUMN = 1,
                  ROW  = 0)
  # COLUMN version
  if (insert == 1) {
    df <- data.frame(df)
    tmpdf <- data.frame()
    for (i in 1:nrow(df)) {
      txt <- as.character(df[i,1])
      if ( class(try(nchar(txt),silent=TRUE)) == "try-error" ) {
        tmpdf <- rbind(tmpdf,-1)
      }
      #if else ( class(try(nchar(txt),silent=TRUE)) == "integer" ) {
      #  tmpdf <- rbind(tmpdf,-2)
      #}
      else {
        tmpdf <- rbind(tmpdf,nchar(txt))
      }
    }
  }
  # ROW version
  else if (insert == 0) {
    df <- data.frame(df)
    tmpdf <- data.frame()
    for (i in 1:ncol(df)) {
      txt <- as.character(df[1,i])
      if ( class(try(nchar(txt),silent=TRUE)) == "try-error" ) {
        tmpdf <- cbind(tmpdf,-1)
      }
      else {
        tmpdf <- cbind(tmpdf,nchar(txt))
      }
    }
  }
  return(tmpdf)
}


# Converts the contents of a corpus to a dataframe.
CPtoDF <- function(corpus) {
  data.frame(test = unlist(sapply(corpus,'[[',"content")),
             stringsAsFactors = FALSE)
}


# Converts a single column dataframe into a corpus.
DFtoCP <- function(df) {
  if (!is.null(ncol(df))) {
    if (ncol(df) != 1) {
      cat("Log... number of columns input should be 1
              number of columns is ",ncol(df),".\n")
      return()  
    }
  }
  cat("Log... number of columns input is 1.\nRunning...\n")
  VCorpus(VectorSource(df))
}


# Takes a corpus and returns:
# a list - an annotator object for each non-empty document, other wise "EMPTY",
# a second list - a list of transformed texts.
entity_replace <- function(cp,poslist) {
  entity_annotator1 <- Maxent_Entity_Annotator(kind = "date")
  entity_annotator2 <- Maxent_Entity_Annotator(kind = "person")
  entity_annotator3 <- Maxent_Entity_Annotator(kind = "location")
  entity_annotator4 <- Maxent_Entity_Annotator(kind = "money")
  entity_annotator5 <- Maxent_Entity_Annotator(kind = "organization")
  txtlist <- list()
  cat("Logging entity_replace loop progress...\n")
  for (i in 1:length(cp)) {
    tmp <- poslist[[i]]
    text <- as.character(cp[[i]])
    if (class(tmp)[1]=="character") {
      txtlist[i] <- "EMPTY"
      # put "EMPTY" as a placeholder: there was no text to annotate
    }
    else {
      tmp <- annotate(text, entity_annotator1, tmp)
      tmp <- annotate(text, entity_annotator2, tmp)
      tmp <- annotate(text, entity_annotator3, tmp)
      tmp <- annotate(text, entity_annotator4, tmp)
      tmp <- annotate(text, entity_annotator5, tmp)
      cat("extracting indexes\n")
      index1 <- extract_index(tmp, text, "date")
      index2 <- extract_index(tmp, text, "person")
      index3 <- extract_index(tmp, text, "location")
      index4 <- extract_index(tmp, text, "money")
      index5 <- extract_index(tmp, text, "organization")
      cat("replacing indexes\n")
      text <- replace_index(text, index1, 1)
      text <- replace_index(text, index2, 2)
      text <- replace_index(text, index3, 3)
      text <- replace_index(text, index4, 4)
      text <- replace_index(text, index5, 5)
      poslist[[i]] <- tmp
      txtlist[i] <- text
    }
    cat("Replacing ",i,"\n") # logging
  }
  cat("entity_replace loop done.")
  lists <- list(poslist,txtlist)
  return(lists)
}


# Extract index for "k"-entities in text x.
# Takes an annotator object, corresponding to text x, entity k and
# returns a data frame containing character indices in text x of the "k"-entities.
extract_index <- function(ann,x,k) {
  i <- 1
  kind <- c("date", "person", "location", "money", "organization")
  l <- length(ann)
  marker <- which(kind == k)
  index <- data.frame()
  while (i < marker) {
    for (j in l:1) {
      if (as.character(ann[j])[5] == paste("list(list(kind = \"","\"))",sep = kind[i])) {
        l <- l-1
      }
      else {
        break
      }
    }
    i <- i+1
  }
  for (j in l:1) {
    if (as.character(ann[j])[5] == paste("list(list(kind = \"","\"))",sep = k)) {
      indices <- c(as.character(ann[j])[3],as.character(ann[j])[4])
      indices <- as.numeric(indices)
      index <- rbind(indices,index)
    }
    else {
      break
    }
  }
  return(index)
}


# Converts the contents of a list of character vectors to a dataframe.
LISTtoDF <- function(list) {
  data.frame(matrix(unlist(list),byrow = TRUE),
             stringAsFactors = FALSE)[,1]
}
 

# Takes a data frame, column by column looks for !=(1|0) and
# returns a vector of the anomalous row numbers.
# For reutersCSV, run on cols 4:138.
OneOrZeroCheck <- function(df) {
  tmpv <- vector(mode="integer")
  for (i in 1:ncol(df)) {
    tmpv <- c(tmpv,which(df[,i] != 0 & df[,i] != 1))
  }
  return(tmpv)
}


# Applies preprocessing using functions available in the tm library.
PassTM <- function(cp) {
  tmp <- cp
  tmp <- tm_map(tmp, content_transformer(tolower)) # makes lower case for following...
  tmp <- tm_map(tmp, removeWords, stopwords("english"))# ...case-sensitive functions
  tmp <- tm_map(tmp, content_transformer(Round1_Num_Punc)) # non-case sensitive functions
  tmp <- tm_map(tmp, stripWhitespace) # erred on the side of extra spaces previously
  tmp <- tm_map(tmp, content_transformer(Round2_Patterns))
  tmp <- tm_map(tmp, stripWhitespace)
  # tm_filter not implemented:
  #   Reuters-21578 has been extensively cleaned from Reuters-22173
  #     for the purpose of making it a standard, widely distributed 
  #     text categorization test collection - no need to apply document filtering here.
  # stemDocument applied after POS tagging
}


# Applies Part Of Speech tagging to a string x (requires library("openNLP")).
Pos <- function(x,sentence_annotator,word_annotator,pos_annotator) {
  tmp <- NLP::annotate(x,list(sentence_annotator,word_annotator))
  tmp <- NLP::annotate(x,pos_annotator,tmp)
}


# Applies Pos to a corpus and
# returns a list of the results with index matching to input corpus.
# If there is no text to be tagged, "EMPTY" is returned in the list.
# Note: v may be explicitly excluded by PosList(cp,,sentence,word,pos)
PosList <- function(cp,v,sentence_annotator,word_annotator,pos_annotator) {
  tmp <- list()
  j <- 1
  if (missing(v)) {
    cat("Computing CharLen data frame.\n")
    v <- CharLen(CPtoDF(cp))
  }
  v <- which(v<=0)
  v <- c(v,length(cp)+1)
  cat("Logging PosList loop progress...\n")
  for (i in 1:length(cp)) {
    if (i==v[j]) {
      tmp[[i]] <- "EMPTY"
      j <- j+1
    }
    else {
      tmp[[i]] <- Pos(cp[[i]],sentence_annotator,word_annotator,pos_annotator)
    }
    cat("PosList ",i,"\n") # logging
  }
  cat("PosList done.\n")
  return(tmp)
}


# Takes a string and 
# returns it with characters indexed in df replaced with n and trailing spaces.
replace_index <- function(txt,df,n) {
  if (nrow(df) >= 1) {
    for (j in 1:nrow(df)) {
      replacement <- paste(n,paste(replicate(df[j,2]-df[j,1]," "),collapse = ""), sep = "")
      substring(txt,df[j,1],df[j,2]) <- replacement
    }
  }
  return(txt)
}


# Cleans up numbers and punctuation of the text x.
Round1_Num_Punc <- function(x) {
  x <- gsub("(.([0-9])+)+", " 9", x) # normalise most numbers
  x <- gsub("([0-9]+)", " 9", x) # normalise header numbers
  x <- VSub(c("9st","9nd","9rd","9th"),"9",x) # normalise ordinal as numbers
  x <- gsub("'","",x) # remove apostrophes (preserve [word]'s as [words])
  x <- gsub("[.]","",x) # remove full stops (preserve abbreviations)
  x <- gsub("[[:punct:]]"," ", x) # replace punctuation with spaces - not removePunctuation()
}


# Cleans up some character patterns of the text x.
Round2_Patterns <- function(x) {
  monthnames <- c(" january ", " february "," march "," april "," may "," june ",
                  " july ", " august "," september "," october "," november "," december ",
                  " sept "," jan "," feb "," mar "," apr "," may ",
                  " jun ","jul "," aug "," sep "," oct "," nov "," dec ")
  #monthnames <- c(paste("\ ",tolower(month.name),"\ "),paste("\ ",tolower(c("sept",month.abb)),"\ "))
  x <- VSub(monthnames, " January ", x) # normalise months (full word)
  x <- VSub(paste(" ",tolower(c("sept",month.abb))," "), " January", x) # normalise months (abbreviated)
  x <- stripWhitespace(x)
  x <- gsub("January January","January", x) # normalise months (Oct/Dec as single instance of month)
  x <- str_trim(x, side = "both")
  # note: not necessary to remove document ending REUTERS
  #       as should be uniformly distributed - statistically insignificant.
}


# Cleans up to make the text x more readible.
Round3_Markers <- function(x) {
  x <- stripWhitespace(x)
  x <- gsub("January", " ISDATE ", x)
  x <- gsub("1", " ISDATE ", x)
  x <- gsub("2", " ISPERSON ", x)
  x <- gsub("3", " ISLOC ", x)
  x <- gsub("4", " ISMONEY ", x)
  x <- gsub("5", " ISORG", x)
  x <- gsub("9", " ISNUMBER ", x)
  x <- stripWhitespace(x)
  x <- str_trim(x, side = "both")
}


# Takes a data frame and checks for shellcode (i.e. non-convertable bytes) and
# returns a vector.
Security <- function(df) {
  a <- data.frame()
  for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
      if (is.na(iconv(df[i,j]))) {
        a <- rbind(a,c(i,j))
      }
    }
  }
  if (length(a)==0) {
    cat("No errors detected in texts.")
  }
  else {
    colnames(a) <- c("row","column")
    cat("There is illegal input in texts...\n")
    return(a)
  }
}


# Takes two data frames: one containing character vectors, and
# the other the index of the texts to be transformed (output from Security()).
# Returns the data frame of character vectors with unusual characters replaced with spaces.
ShellcodeToSpace <- function(df,index) {
  if (is.null(index)) {
    # do nothing
  }
  else {
    for (i in 1:nrow(index)) {
    df[index[i,1],index[i,2]] <- iconv(df[index[i,1],index[i,2]],sub = " ")
    }
  }
  cat("ShellcodeToSpace complete.")
  return(df)
}


# Takes a character vector, string1 and txtstring,
# replaces everything in txtstring matching character vector with string1 and
# returns the transformed string.
VSub <- function(v, replacement, text) {
  for (i in 1:length(v)) {
    text <- gsub(v[i],replacement,text)
  }
  return(text)
}
