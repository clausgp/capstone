# modelling

library(quanteda)
library(stringi)
library(data.table)

library(pryr)
library(rbenchmark)

setwd("Rwork\\coursera data science specialization\\capstone")

# only using blogs for now
blogs.file <- readLines("final/en_US/en_US.blogs.txt", encoding="UTF-8")
blogs.file <- blogs.file[stri_count_words(blogs.file)>=5]
blogs.file <- iconv(blogs.file, from="UTF-8", to="ASCII", sub="")

pct <- 0.05
set.seed(1234)
blogs <- sample(blogs.file, pct*length(blogs.file))

blogs.corp <- corpus(blogs)

# unigram
bdfm <- dfm(blogs.corp, ngrams=1, concatenator=" ")
nf <- bdfm@Dim[2]
bf <- topfeatures(bdfm, n=nf)
sf <- sum(bf)
udt <- data.table(f=attributes(bf)$names, n=as.integer(bf))
setkey(udt, f, n)
udt$pct <- udt$n/sf

# bigram
bdfm <- dfm(blogs.corp, ngrams=2, concatenator=" ")
nf <- bdfm@Dim[2]
bf <- topfeatures(bdfm, n=nf)
sf <- sum(bf)
bdt <- data.table(f=attributes(bf)$names, n=as.integer(bf))
bdt$first <- stri_extract_first(bdt$f, regex="^\\S+(?= )")
bdt$last <- stri_extract_last(bdt$f, regex="(?<= )\\S+$")
setkey(bdt, f, first, last, n)
bdt$s <- bdt$n/udt[bdt$first,n]

# trigram
bdfm <- dfm(blogs.corp, ngrams=3, concatenator=" ")
nf <- bdfm@Dim[2]
bf <- topfeatures(bdfm, n=nf)
sf <- sum(bf)
tdt <- data.table(f=attributes(bf)$names, n=as.integer(bf))
tdt$first <- stri_extract_first(tdt$f, regex="^\\S+ \\S+(?= )")
tdt$last <- stri_extract_last(tdt$f, regex="(?<= )\\S+$")
setkey(tdt, f, first, last, n)
tdt$s <- tdt$n/bdt[tdt$first, n]

# rewrite this!!
# testing
tdt.test <- tdt[1:100]
benchmark(t1 <- tdt.test$n/sapply(tdt.test$first, function(x) bdt$n[bdt$f==x]), t2 <- tdt.test[,n]/bdt[tdt.test[,first],n])
tdt$s <- tdt$n/sapply(tdt$first, function(x) bdt$n[bdt$f==x])




head(udt,10)
object_size(udt)

head(bdt,10)
object_size(bdt)

head(tdt,10)
object_size(tdt)

# bf <- topfeatures(bdfm, n=bdfm@Dim[2])
# dbf <- data.frame(f=attributes(bf), n=bf, row.names=NULL)
# dbf <- data.table(f=attributes(bf)$names, n=bf)
# dbf$first <- stri_extract_first(dbf$f, regex="^\\S+ \\S+(?= )")
# dbf$last <- stri_extract_last(dbf$f, regex="(?<= )\\S+$")


save(udt, file="udt.Rda")
save(bdt, file="bdt.Rda")
save(tdt, file="tdt.Rda")
