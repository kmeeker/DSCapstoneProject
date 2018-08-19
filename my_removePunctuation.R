library(tm)

my_removePunctuation <-
    function(x, ...)
        UseMethod("my_removePunctuation")
my_removePunctuation.character <-
    function(x,
             preserve_intra_word_contractions = FALSE,
             preserve_intra_word_dashes = FALSE,
             ucp = FALSE,
             ...)
    {
        
        # Assume there are no ASCII 0x01 (SOH) or ASCII 0x02 (STX) characters.
        if (preserve_intra_word_contractions)
            x <- gsub("(\\w)'(\\w)", "\\1\1\\2", x, perl = TRUE)
        if (preserve_intra_word_dashes)
            x <- gsub("(\\w)-(\\w)", "\\1\2\\2", x, perl = TRUE)
        
        # white space, single letter, followed by punctuation - emoticon
        #x <- gsub( "\\s.[^a-zA-z\\s]" , "", x, perl = TRUE)
        
        if (ucp)
            x <- gsub("\\p{P}+", "", x, perl = TRUE)
        else
            x <- gsub("[[:punct:]]+", "", x)

        if (preserve_intra_word_contractions)
            x <- gsub("\1", "'", x, fixed = TRUE)
        if (preserve_intra_word_dashes)
            x <- gsub("\2", "-", x, fixed = TRUE)
        
        x
    }
my_removePunctuation.PlainTextDocument <-
    content_transformer(my_removePunctuation.character)



