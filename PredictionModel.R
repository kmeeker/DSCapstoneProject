sample_file <- function(indir, outdir, file, sample_size=10) {
    
    con <- file(paste0(indir, file), "r")
    con_out <- file(paste0(outdir, file), "wt")
    
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

clean_text <- function(x, ...) {
    x <- removeNumbers(x)
    x <- tolower(x)
    x <- my_removePunctuation(x, preserve_intra_word_contractions = TRUE, 
                         preserve_intra_word_dashes = TRUE)
}

term_freqs <- function(dtm) {
    
    FreqMat <- data.frame(term = dimnames(dtm)[[2]], 
                          freq = Matrix::colSums(dtm),
                          row.names = NULL)
    rownames(FreqMat) <- 1:nrow(FreqMat)
    return(FreqMat)
}

predict_next <- function(vocab, first, vocab3, n=1, lo_prob_cutoff=0.1) {
    library(dplyr)
    
    bi <- vocab[which(stringr::str_detect(vocab$term,paste0("^",first,"_"))),]
    bi <- bi[order(-bi$term_count),]
    n_bi <- nrow(bi)
    
    # Good-Turing: re-estimate the amount of probability mass to assign 
    # to Ngrams with low counts by looking
    # at the number of Ngrams of higher counts
    tri <- vocab3[which(stringr::str_detect(vocab3$term,paste0("^",first,"_"))),]
    n_tri <- nrow(tri)
    
    bi$prob <- bi$term_count/sum(bi$term_count)
    # adjust mass for low probs
    lo <- which(bi$prob<lo_prob_cutoff)
    bi[lo,"prob"] <- bi[lo,"prob"] * length(lo)/n_tri
    
    tf <- data.frame(wd=sapply(str_split(bi$term,"_"), "[[", 2), prob=bi$prob)
    
    pred<-sample_n(tf,n,weight=tf$prob)
    result <- data.frame(wd=as.character(pred$wd),prob=pred$prob)
    result <- result[order(-result$prob),]
    
    return(result)
}

rank_next <- function(vocab, first, second=NULL) {
    bi <- data.frame()
    
    if (is.null(second)) {
        bi <- vocab[which(stringr::str_detect(vocab$term,paste0("^",first))),]
        bi <- bi[order(-bi$term_count),]
        bi <- head(bi)
    } else {
        for (i in 1:length(second)) {
            bi<-rbind(bi,vocab[which(stringr::str_detect(vocab$term,paste0("^",first,"_",second[i],"$"))),])
            bi<-rbind(bi,vocab[which(stringr::str_detect(vocab$term,paste0("^",second[i],"$"))),])
        }
        bi <- bi[order(-bi$term_count),]
    }
    
    return(bi)
}


