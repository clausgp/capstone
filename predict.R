# predicter functions for stupid backoff ngram model (1-gram to 8-gram)

library(data.table)
library(stringi)
library(dplyr)

load("udt.Rda")
load("bdt.Rda")
load("tdt.Rda")
load("fdt.Rda")
load("qdt.Rda")
load("sdt.Rda")
load("spdt.Rda")
load("odt.Rda")

# clean function to be used before predict call
clean <- function(x){
    x %>% unlist() %>% 
        stri_replace_all("", regex="[^a-zA-Z ]") %>%
        stri_replace_all("", regex="\\b(http|www).+\\b") %>% 
        stri_replace_all(" ", regex=" +") %>%
        tolower() %>% stri_trim_both()%>% 
        stri_extract_all_words() %>% unlist()
}

# predict function called from predict.next
predict <- function(ngram, w_1, nr=3, prev=NULL){
    if (ngram==1){
        ret <- udt$terms[1:(nr*2)]
    }
    if (ngram==2){
        ret <- bdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(1, "", nr, prev)
    }
    if (ngram==3){
        ret <- tdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(2, stri_extract_last(w_1, regex="(?<=_)[a-z]+$"), nr, prev)
    }
    if (ngram==4){
        ret <- fdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(3, stri_extract_last(w_1, regex="(?<=_)[a-z]+_[a-z]+$"),
                           nr, prev)
    }
    if (ngram==5){
        ret <- qdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(4, stri_extract_last(w_1, regex="(?<=_)[a-z]+(_[a-z]+){2}$"),
                           nr, prev)
    }
    if (ngram==6){
        ret <- sdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(5, stri_extract_last(w_1, regex="(?<=_)[a-z]+(_[a-z]+){3}$"),
                           nr, prev)
    }
    if (ngram==7){
        ret <- spdt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(6, stri_extract_last(w_1, regex="(?<=_)[a-z]+(_[a-z]+){4}$"),
                           nr, prev)
    }
    if (ngram==8){
        ret <- odt[term_1==w_1, last]
        if (length(ret)==0)
            ret <- predict(7, stri_extract_last(w_1, regex="(?<=_)[a-z]+(_[a-z]+){5}$"),
                           nr, prev)
    }
    if (!is.null(prev)){
        ret <- ret[!ret %in% prev] 
    }
    ret.len <- length(ret)
    if (ret.len >= nr)
        ret <- ret[1:nr]
    else
        if (ngram > 1)
            ret <- c(ret, predict(ngram-1, stri_extract_last(w_1,
                                                    regex="(?<=^[a-z]{1,1000}_)[a-z_]+$"),
                                  nr=nr-ret.len, prev=c(ret, prev)))
    ret
}

# predict next word after sentence x, suggest nr amount of words
predict.next <- function(x, nr=3){
    # x cleaned with clean
    x.len = length(x)
    if (x.len > 6)
        ngram <- 8
    else
        ngram <- x.len + 1
    w_1 <- paste(x, collapse="_")
    predict(ngram, w_1, nr=nr)
}

# predict current word in sentence x, suggest nr amount of words
predict.curr <- function(x, nr=3, prev=NULL){
    # x cleaned with clean
    x.len = length(x)
    if (x.len==1){
        ret <- stri_subset_regex(udt$terms, pattern=paste0("^", x[1]))
    }
    if (x.len==2){
        ret <- stri_subset_regex(bdt[term_1==x[1], last], pattern=paste0("^", x[2]))
    }
    if (x.len==3){
        ret <- stri_subset_regex(tdt[term_1==paste(x[-3], collapse="_"), last],
                                 pattern=paste0("^", x[3]))
    }
    if (x.len==4){
        ret <- stri_subset_regex(fdt[term_1==paste(x[-4], collapse="_"), last],
                                 pattern=paste0("^", x[4]))
    }
    if (x.len==5){
        ret <- stri_subset_regex(qdt[term_1==paste(x[-5], collapse="_"), last],
                                 pattern=paste0("^", x[5]))
    }
    if (x.len==6){
        ret <- stri_subset_regex(sdt[term_1==paste(x[-6], collapse="_"), last],
                                 pattern=paste0("^", x[6]))
    }
    if (x.len==7){
        ret <- stri_subset_regex(spdt[term_1==paste(x[-7], collapse="_"), last],
                                 pattern=paste0("^", x[7]))
    }
    if (x.len>7){
        y <- x[(x.len-8+1):x.len]
        ret <- stri_subset_regex(odt[term_1==paste(y[-8], collapse="_"), last],
                                 pattern=paste0("^", y[8]))
    }
    # if (is.na(ret[1]) & x.len > 1)
    #     ret <- predict.curr(x[-1], nr=nr, prev=prev)
    if (!is.null(prev)){
        ret <- ret[!ret %in% prev]
        ret <- c(prev, ret)
    }
    if (length(ret) >= nr)
        ret <- ret[1:nr]
    else if (x.len > 1)
        ret <- predict.curr(x[-1], nr=nr, prev=ret)
    ret
}

# abandoned - why is it still here?
predict.sentence <- function(x, type="char"){
    # x cleaned with clean
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
        if (i==5){
            ngram <- i
            w_1 <- paste(x[i-4], x[i-3], x[i-2], x[i-1], sep="_")
        }
        if (i==6){
            ngram <- i
            w_1 <- paste(x[i-5], x[i-4], x[i-3], x[i-2], x[i-1], sep="_")
        }
        if (i==7){
            ngram <- i
            w_1 <- paste(x[i-6], x[i-5], x[i-4], x[i-3], x[i-2], x[i-1], sep="_")
        }
        if (i > 7){
            ngram <- 8
            w_1 <- paste(x[i-7], x[i-6], x[i-5], x[i-4], x[i-3], x[i-2], x[i-1], sep="_")
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