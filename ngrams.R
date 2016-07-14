# ngrams from modelling.R are combined

library(dplyr)
library(data.table)
library(stringi)
library(pryr)

# join uni-grams

load("budt.Rda")
load("nudt.Rda")
load("tudt.Rda")

udt <- b.udt %>% rbind(n.udt) %>%
    rbind(t.udt) %>%
    group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts), doc_counts = sum(doc_counts))
# nu.terms <- sum(udt$terms_counts)
setorder(udt, -terms_counts)
udt <- udt[doc_counts>2]
udt$doc_counts <- NULL

# udt$p <- udt$terms_counts/nu.terms
# udt$logp <- log(udt$p)
# setorder(udt, terms)

save(udt, file="udt.Rda")

# join bi-grams

load("bbdt.Rda")
load("nbdt.Rda")
load("tbdt.Rda")

bdt <- b.bdt %>% rbind(n.bdt) %>%
    rbind(t.bdt) %>%
    group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts), doc_counts=sum(doc_counts))
# nb.terms <- sum(bdt$terms_counts)
bdt <- bdt[doc_counts > 2]

bdt$term_1 <- stri_extract_first(bdt$terms, regex="^[a-z]+(?=_)")
bdt$last <- stri_extract_last(bdt$terms, regex="(?<=_)[a-z]+$")
# bdt$p <- bdt$terms_counts/nb.terms
# bdt$logp <- log(bdt$p)
bdt$terms <- NULL
bdt$doc_counts <- NULL
setorder(bdt, term_1, -terms_counts)

# setorder(bdt, terms)

save(bdt, file="bdt.Rda")

# join 3 grams

load("btdt.Rda")
load("ntdt.Rda")
load("ttdt.Rda")

tdt <- b.tdt %>% rbind(n.tdt) %>%
    rbind(t.tdt) %>%
    group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts), doc_counts=sum(doc_counts))
# nt.terms <- sum(tdt$terms_counts)
tdt <- tdt[doc_counts>2]

tdt$term_1 <- stri_extract_first(tdt$terms, regex="^\\S+_\\S+(?=_)")
tdt$last <- stri_extract_last(tdt$terms, regex="(?<=_)[a-z]+$")

# tdt$p <- tdt$terms_counts/nt.terms
# tdt$logp <- log(tdt$p)
tdt$terms <- NULL
tdt$doc_counts <- NULL
setorder(tdt, term_1, -terms_counts)

save(tdt, file="tdt.Rda")

# join 4 grams

load("bfdt.Rda")
load("nfdt.Rda")
load("tfdt.Rda")

fdt <- b.fdt %>% rbind(n.fdt) %>%
    rbind(t.fdt) %>%
    group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts), doc_counts=sum(doc_counts))
# nf.terms <- sum(fdt$terms_counts)
setorder(fdt, -terms_counts)
fdt <- fdt[doc_counts>2]

fdt$term_1 <- stri_extract_first(fdt$terms, regex="^[a-z]+_[a-z]+_[a-z]+(?=_)")
fdt$last <- stri_extract_last(fdt$terms, regex="(?<=_)[a-z]+$")

# fdt$p <- fdt$terms_counts/nf.terms
# fdt$logp <- log(fdt$p)
fdt$terms <- NULL
fdt$doc_counts <- NULL
setorder(fdt, term_1, -terms_counts)

save(fdt, file="fdt.Rda")


# join 5 grams

load("bqdt.Rda")
load("nqdt.Rda")
load("tqdt.Rda")

qdt <- b.qdt %>% rbind(n.qdt) %>%
    rbind(t.qdt) %>%
    group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts), doc_counts=sum(doc_counts))
# nq.terms <- sum(qdt$terms_counts)
setorder(qdt, -terms_counts)

qdt <- qdt[doc_counts>2]

qdt$term_1 <- stri_extract_first(qdt$terms, regex="^[a-z]+_[a-z]+_[a-z]+_[a-z]+(?=_)")
qdt$last <- stri_extract_last(qdt$terms, regex="(?<=_)[a-z]+$")

# qdt$p <- qdt$terms_counts/nq.terms
# qdt$logp <- log(qdt$p)
qdt$terms <- NULL
qdt$doc_counts <- NULL
setorder(qdt, term_1, -terms_counts)

save(qdt, file="qdt.Rda")