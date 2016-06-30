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
            ret <- predict(4, stri_extract_last(w_1, regex="(?<=_)[a-z]+_[a-z]+_[a-z]+$"),
                           nr, prev)
    }
    if (!is.null(prev)){
        ret <- ret[!ret %in% prev]
        # ret <- gsub(x=ret, pattern=paste(prev, collapse="|"), replacement="",
        #             ignore.case=TRUE)
        # ret <- ret[ret!=""]    
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
predict.next <- function(x, nr=3){
# x cleaned with cleaner
    x.len = length(x)
    if (x.len > 3)
        ngram <- 5
    else
        ngram <- x.len + 1
    if (x.len==1)
        w_1 <- x
    if (x.len==2)
        w_1 <- paste(x[x.len-1], x[x.len], sep="_")
    if (x.len==3)
        w_1 <- paste(x[x.len-2], x[x.len-1], x[x.len], sep="_")
    if (x.len>3)
        w_1 <- paste(x[x.len-3], x[x.len-2], x[x.len-1], x[x.len], sep="_")
    predict(ngram, w_1, nr=nr)
}
predict.curr <- function(x, nr=3, prev=NULL){
# x cleaned with cleaner
    x.len = length(x)
    if (x.len==1){
        ret <- stri_subset_regex(udt$terms, pattern=paste0("^", x[1]))
    }
    if (x.len==2){
        ret <- stri_subset_regex(bdt[term_1==x[1], last], pattern=paste0("^", x[2]))
    }
    if (x.len==3){
        ret <- stri_subset_regex(tdt[term_1==paste(x[1], x[2], sep="_"), last],
                          pattern=paste0("^", x[3]))
    }
    if (x.len==4){
        ret <- stri_subset_regex(fdt[term_1==paste(x[1], x[2], x[3], sep="_"), last],
                          pattern=paste0("^", x[4]))
    }
    if (x.len>4){
        y <- x[(x.len-5+1):x.len]
        ret <- stri_subset_regex(qdt[term_1==paste(y[1], y[2], y[3], y[4], sep="_"),
                                     last], pattern=paste0("^", y[5]))
    }
    if (is.na(ret[1]) & x.len > 1)
        ret <- predict.curr(x[-1], nr=nr, prev=prev)
    if (!is.null(prev)){
        ret <- ret[!ret %in% prev]
        ret <- c(prev, ret)
    }
    ret.len <- length(ret)
    if (ret.len >= nr)
        ret <- ret[1:nr]
    else if (x.len > 1)
        ret <- predict.curr(x[-1], nr=nr, prev=ret)
    ret
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


# instr <- "this i4s a test, string"
# instr <- blogs.file[3]
# instr <- cleaner(instr)
# predicter(instr)