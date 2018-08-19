sample_file <- function(dir,in_file, out_file, sample_size=10) {
    
    con <- file(paste0(dir,in_file), "r")
    con_out <- file(paste0(dir, out_file), "wt")
    
    set.seed(123)
    
    i = 0
    while (i<sample_size) {
        if (rbinom(1, 1, 0.5) == 1) {
            l <- readLines(con, 1)
            writeLines(l, con_out)
            i = i+1
        }
    }
    
    close(con)
    close(con_out)
}



large_corpus_tfs <- function(dir,filename,read_chunk=50) {
    library(tm)
    library(NLP)
    
    source("my_RemovePunctuation.R")
    
    btm <- function(x) {
        max_ngram=2
        unlist(lapply(ngrams(words(x), max_ngram), paste, collapse = " "), use.names = FALSE)
    }
    
    con <- file(paste0(dir,filename), "r")
    
    # Vector to collect results
    wordDTM <- NULL
    ng2DTM <- NULL
    wordFreq <- data.frame()
    ng2Freq <- data.frame()
    
    while ( length(lines <- readLines(con,read_chunk))>0 ) {
        
        try({      
            
            # Convert lines (documents) into a corpus:
            Corpus <- VCorpus(VectorSource(iconv(paste(lines,collapse = " "), "latin1", "UTF-8")))
            
            #
            # Do other things e.g. preprocessing...
            Corpus <- tm_map(Corpus, removeNumbers)
            Corpus <- tm_map(Corpus, tolower)
            Corpus <- tm_map(Corpus, my_removePunctuation, preserve_intra_word_contractions = TRUE,
                             preserve_intra_word_dashes = TRUE )
            Corpus <- tm_map(Corpus, PlainTextDocument)

            wordFreq <- term_freqs(tf=wordFreq,dtm=DocumentTermMatrix(Corpus))

            # 2-gram
            ng2Freq <- term_freqs(tf=ng2Freq,dtm=DocumentTermMatrix(Corpus, control=list(tokenize = btm)))
            })
        gc()
    }
    close(con)
    return(list(wordFreq=wordFreq,ng2Freq=ng2Freq))    
}

term_freqs <- function(tf,dtm) {
    
    FreqMat <- data.frame(term = dtm$dimnames[[2]], 
                          freq = dtm$v,
                          row.names = NULL)
    FreqMat <- rbind(tf,FreqMat)
    FreqMat <- aggregate(FreqMat$freq, by=list(term=FreqMat$term), FUN=sum)
    names(FreqMat) <- c("term","freq")
    
    FreqMat <- FreqMat[order(-FreqMat$freq),] 
    rownames(FreqMat) <- 1:nrow(FreqMat)
    return(FreqMat)
}

term_summary2 <- function(tf){
    library(ggplot2)
    library(tm)
    
    # word count 
    print(paste("number of terms: ",nrow(tf)))
    
    head(tf, 20)
    
    p <- ggplot( tf[1:20,], aes(term, freq) ) +
        geom_bar(aes(x=reorder(term, -freq)),stat='identity') +
        theme(axis.text.x=element_text(angle=45, hjust=1))
    print(p)
    
    plot_cloud(tf)
}

plot_cloud <- function(tf, n=200) {
    library(wordcloud)
    
    set.seed(1234)
    tryCatch( wordcloud(words = tf$term, freq = tf$freq, min.freq = 1,
              max.words=n, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
             , warning = function(w) {})
}


