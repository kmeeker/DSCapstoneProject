create_corpus <- function(dir,filename,read_chunk=50) {
    library(tm)
    source("my_removePunctuation.R")

    con <- file(paste0(dir,filename), "r")
    
    # Vector to collect the corpora:
    CorpusCollection <- c()
    
    while ( length(lines <- readLines(con,read_chunk))>0 ) {
        
        try({      
            
            # Convert lines (documents) into a corpus:
            Documents <- Corpus(VectorSource(iconv(lines, "latin1", "UTF-8")))
            
            #
            # Do other things e.g. preprocessing...
            
            # Store these documents into the corpus vector:
            CorpusCollection <- rbind(CorpusCollection, Documents)
            
        })
    }
    close(con)
    
    # Collecting done. Create one huge corpus:
    Corpus <- Corpus(VectorSource(unlist(CorpusCollection[,"content"])))
    Corpus <- tm_map(Corpus, removeNumbers)
    Corpus <- tm_map(Corpus, tolower)
    Corpus <- tm_map(Corpus, my_removePunctuation, preserve_intra_word_contractions = TRUE,
                      preserve_intra_word_dashes = TRUE )
    Corpus <- tm_map(Corpus, PlainTextDocument)
}

large_corpus_DTMs <- function(dir,filename,read_chunk=50) {
    library(tm)
    source("my_removePunctuation.R")
    
    tokenize_bigrams <- function(x) {
        rownames(as.data.frame(unclass(tau::textcnt(x$content, method="string", n=2))))
    }
    
    con <- file(paste0(dir,filename), "r")
    
    # Vector to collect results
    wordDTM <- NULL
    ng2DTM <- NULL
    
    while ( length(lines <- readLines(con,read_chunk))>0 ) {
        
        try({      
            
            # Convert lines (documents) into a corpus:
            Corpus <- Corpus(VectorSource(iconv(paste(lines,collapse = ''), "latin1", "UTF-8")))
            
            #
            # Do other things e.g. preprocessing...
            #Corpus <- Corpus(VectorSource(unlist(Corpus)))
            Corpus <- tm_map(Corpus, removeNumbers)
            Corpus <- tm_map(Corpus, tolower)
            Corpus <- tm_map(Corpus, my_removePunctuation, preserve_intra_word_contractions = TRUE,
                             preserve_intra_word_dashes = TRUE )
            Corpus <- tm_map(Corpus, PlainTextDocument)

            wordDTM <- cbind(wordDTM,DocumentTermMatrix(Corpus))
            text_sample <- lines
            
            # 2-gram
            #ng2TDM <- c(ng2TDM,TDM(Corpus, min_ngram=2, max_ngram=2))
            #ng2Freq <- c(ng2Freq,word_freqs(ng2TDM))
            
        })
    }
    
    close(con)
    return(list(lines=text_sample,wordDTM=wordDTM,ng2DTM=ng2DTM))    
}


TDM <- function(myCorpus, min_ngram=1, max_ngram=1) {
    library(tm)
    library(RWeka)
    library(rJava)
   
    btm <- function(x) {
        unlist(lapply(ngrams(words(x), max_ngram), paste, collapse = " "), use.names = FALSE)
    }
    
    myTdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = btm))
}

# word_freqs2 <- function(myTdm) {
#     library(slam)
#     
#     stm <- as.simple_triplet_matrix(myTdm)
#     tf <- row_sums(stm)
# 
#     FreqMat <- data.frame(ST = names(tf), 
#                           Freq = tf)
#     FreqMat <- FreqMat[order(-FreqMat$Freq),] 
#     rownames(FreqMat) <- 1:nrow(FreqMat)
#     return(as.simple_triplet_matrix(FreqMat))
# }
# 
# word_freqs <- function(myTdm) {
#     myTdm <- as.matrix(myTdm)
#     FreqMat <- data.frame(ST = rownames(myTdm), 
#                           Freq = rowSums(myTdm), 
#                           row.names = NULL)
#     FreqMat <- FreqMat[order(-FreqMat$Freq),] 
# }

word_freqs <- function(myDTM) {
    FreqMat <- data.frame(ST = myDTM$dimnames, 
                          Freq = myDTM$v)
    FreqMat <- FreqMat[order(-FreqMat$Freq),] 
    rownames(FreqMat) <- 1:nrow(FreqMat)
    return(FreqMat)
}

term_summary <- function(tdm, wd_freq){
    library(ggplot2)
    library(tm)
    
    # word count 
    print(paste("number of terms: ",nTerms(tdm)))
    
    print(paste("freqs: ",head(wd_freq, 20)))
    
    p <- ggplot( wd_freq[1:20,], aes(ST, Freq) ) +
    geom_bar(aes(x=reorder(ST, -Freq)),stat='identity') +
    theme(axis.text.x=element_text(angle=45, hjust=1))
    print(p)

    plot_cloud(tdm)
}

term_summary2 <- function(dtm, wd_freq){
    library(ggplot2)
    library(tm)
    
    # word count 
    print(paste("number of terms: ",dtm$ncol))
    
    print(paste("freqs: ",head(wd_freq, 20)))
    
    p <- ggplot( wd_freq[1:20,], aes(ST, Freq) ) +
        geom_bar(aes(x=reorder(ST, -Freq)),stat='identity') +
        theme(axis.text.x=element_text(angle=45, hjust=1))
    print(p)
    
    plot_cloud(tdm)
}

plot_cloud <- function(dtm, n=200) {
    library(wordcloud)
    
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    
    set.seed(1234)
    tryCatch( wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=n, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
             , warning = function(w) {})
}


