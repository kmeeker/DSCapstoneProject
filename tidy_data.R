sample_file <- function(dir,filename, sample_size=10) {
    
    con <- file(paste0(dir,filename), "r")
    con_out <- file(paste0(dir,"sample_",filename), "wt")
    
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

max_line_length <- function(dir,filename) {
    
    con <- file(paste0(dir,filename), "r")
    
    m <- 0
    i <- 0
 
    tryCatch(
        while ( length(line <- readLines(con,1))>0 ) {
            m <- max(m,nchar(line))
            i <- i+1
        },
        error = function(e) { print(paste("error reading line num",i,":"),line) }
    )
    
    close(con)
    return(m)
}

find_regex <- function(dir,filename,regex) {
    library(stringr)
    
    
    con <- file(paste0(dir,filename), "r")
    con_out <- file(paste0(dir,"regex_",regex,"_",filename), "wt")
    
    i <- 0
    
    tryCatch(
        while ( length(line <- readLines(con,1))>0 ) {
            if (!is.na(matches <- str_match(line,regex))>0) {
                writeLines(line, con_out)
                flush(con_out)
                #print(matches[1])
                i <- i+1
            }
        },
        error = function(e) { print(paste("error reading line num",i,":",line)) }
    )
    
    close(con)
    close(con_out)
    return(i)
}