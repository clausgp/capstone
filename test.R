library(data.table)
library(stringi)
library(dplyr)

load("udt.Rda")
load("bdt.Rda")
load("tdt.Rda")
load("fdt.Rda")

setorder(udt, -terms_counts)
setorder(bdt, term_1, -terms_counts)
setorder(tdt, term_1, -terms_counts)
setorder(fdt, term_1, -terms_counts)

cleaner <- function(x){
    x %>% unlist() %>% 
        stri_replace_all("", regex="[^a-zA-Z ]") %>%
        stri_replace_all("", regex="\\b(http|www).+\\b") %>% 
        stri_replace_all(" ", regex=" +") %>%
        tolower() %>% stri_trim_both()%>% 
        stri_extract_all_words() %>% unlist()
}

predict <- function(ngram, w_1){
    if (ngram==1){
        ret <- udt$terms[1]
    }
    if (ngram==2){
        ret <- bdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(1, "")
        else
            ret <- ret[1]
    }
    if (ngram==3){
        ret <- tdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(2, stri_extract_last(w_1, regex="(?<=_)[a-z]+$"))
        else
            ret <- ret[1]
    }
    if (ngram==4){
        ret <- tdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(3, stri_extract_last(w_1, regex="(?<=_)[a-z]+_[a-z]+$"))
        else
            ret <- ret[1]
    }
    ret
}
predicter <- function(x){
    outstr <- character(0)
    
    for (i in 1:length(x)){
        if (i==1){
            ngram <- i
            w_1 <- ""
        }
        if (i==2){
            ngram <- i
            w_1 <- x[i-1]
        }
        if (i==3){
            ngram <- 3
            w_1 <- paste(x[i-2], x[i-1], sep="_")
        }
        if (i>3){
            ngram <- 4
            w_1 <- paste(x[i-3], x[i-2], x[i-1], sep="_")
        }
        if (length(outstr)==0)
            outstr <- predict(ngram, w_1)
        else
            outstr <- paste(outstr, predict(ngram, w_1))
        #print(paste(i, ngram, w_1, outstr))
    }
    data.frame(i=x, o=outstr %>% stri_extract_all_words() %>% unlist())
}


#instr <- "this i4s a test, string"
instr <- blogs.file[3]
instr <- cleaner(instr)
predicter(instr)