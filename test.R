library(data.table)
library(stringi)
library(dplyr)

load("udt.Rda")
load("bdt.Rda")
load("tdt.Rda")
load("fdt.Rda")
load("qdt.Rda")

setorder(udt, -terms_counts)
setkey(tdt, term_1, terms_counts)
setorder(bdt, term_1, -terms_counts)
setorder(tdt, term_1, -terms_counts)
setorder(fdt, term_1, -terms_counts)
setorder(qdt, term_1, -terms_counts)

cleaner <- function(x){
    x %>% unlist() %>% 
        stri_replace_all("", regex="[^a-zA-Z ]") %>%
        stri_replace_all("", regex="\\b(http|www).+\\b") %>% 
        stri_replace_all(" ", regex=" +") %>%
        tolower() %>% stri_trim_both()%>% 
        stri_extract_all_words() %>% unlist()
}

predict <- function(ngram, w_1, nr=3, prev=""){
    if (ngram==1){
        ret <- udt$terms[1:nr]
    }
    if (ngram==2){
        ret <- bdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(1, "", nr, prev)
        else
            ret <- ret
    }
    if (ngram==3){
        ret <- tdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(2, stri_extract_last(w_1, regex="(?<=_)[a-z]+$"), nr, prev)
        else
            ret <- ret
    }
    if (ngram==4){
        ret <- fdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(3, stri_extract_last(w_1, regex="(?<=_)[a-z]+_[a-z]+$"),
                           nr, prev)
        else
            ret <- ret
    }
    if (ngram==5){
        ret <- qdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(4, stri_extract_last(w_1, regex="(?<=_)[a-z]+_[a-z]+_[a-z]+$"),
                           nr, prev)
        else
            ret <- ret
    }
    ret <- gsub(x=ret, pattern=paste(prev, collapse="|"), replacement="",
                 ignore.case=TRUE)
    ret <- ret[ret!=""]
    ret.nr <- length(ret)
    if (ret.nr>=nr)
        ret <- ret[1:nr]
    else
        if (ngram>1)
            ret <- c(ret, predict(ngram-1, stri_extract_last(w_1,
                                            regex="(?<=^[a-z]{1,100}_)[a-z_]+$"),
                                    nr=nr-ret.nr, prev=ret))
    ret
}
predict.next <- function(x, nr.out=3){
# x cleaned with cleaner
    i = length(x)
    if (i==1){
        ngram <- i+1
        w_1 <- x
    }
    if (i==2){
        ngram <- i+1
        w_1 <- paste(x[i-1], x[i], sep="_")
    }
    if (i==3){
        ngram <- i+1
        w_1 <- paste(x[i-2], x[i-1], x[i], sep="_")
    }
    if (i>3){
        ngram <- 5
        w_1 <- paste(x[i-3], x[i-2], x[i-1], x[i], sep="_")
    }
    predict(ngram, w_1, nr=nr.out)
        
}
predict.sentence <- function(x, type="char"){
# x cleaned with cleaner
    outstr <- character(0)
    for (i in start:length(x)){
        if (i==1){
            ngram <- i
            w_1 <- ""
        }
        if (i==2){
            ngram <- i
            w_1 <- x[i-1]
        }
        if (i==3){
            ngram <- i
            w_1 <- paste(x[i-2], x[i-1], sep="_")
        }
        if (i==4){
            ngram <- i
            w_1 <- paste(x[i-3], x[i-2], x[i-1], sep="_")
        }
        if (i>4){
            ngram <- 5
            w_1 <- paste(x[i-4], x[i-3], x[i-2], x[i-1], sep="_")
        }
        if (length(outstr)==0)
            outstr <- predict(ngram, w_1)
        else
            outstr <- paste(outstr, predict(ngram, w_1))
        #print(paste(i, ngram, w_1, outstr))
    }
    if (type=="char")
        outstr
    else
        data.frame(i=x, o=outstr %>% stri_extract_all_words() %>% unlist())
}


instr <- "this i4s a test, string"
# instr <- blogs.file[3]
# instr <- cleaner(instr)
# predicter(instr)