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

tdt$term_1 <- stri_extract_first(tdt$terms, regex="^[a-z]+_[a-z]+(?=_)")
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

fdt$term_1 <- stri_extract_first(fdt$terms, regex="^[a-z]+(_[a-z]+){2}(?=_)")
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

qdt$term_1 <- stri_extract_first(qdt$terms, regex="^[a-z]+(_[a-z]+){3}(?=_)")
qdt$last <- stri_extract_last(qdt$terms, regex="(?<=_)[a-z]+$")

# qdt$p <- qdt$terms_counts/nq.terms
# qdt$logp <- log(qdt$p)
qdt$terms <- NULL
qdt$doc_counts <- NULL
setorder(qdt, term_1, -terms_counts)

save(qdt, file="qdt.Rda")

# join 6 grams

load("bsdt.Rda")
load("nsdt.Rda")
load("tsdt.Rda")

sdt <- b.sdt %>% rbind(n.sdt) %>%
    rbind(t.sdt) %>%
    group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts), doc_counts=sum(doc_counts))
# ns.terms <- sum(sdt$terms_counts)
setorder(sdt, -terms_counts)

sdt <- sdt[doc_counts>2]

sdt$term_1 <- stri_extract_first(sdt$terms, regex="^[a-z]+(_[a-z]+){4}(?=_)")
sdt$last <- stri_extract_last(sdt$terms, regex="(?<=_)[a-z]+$")

# sdt$p <- sdt$terms_counts/ns.terms
# sdt$logp <- log(sdt$p)
sdt$terms <- NULL
sdt$doc_counts <- NULL
setorder(sdt, term_1, -terms_counts)

save(sdt, file="sdt.Rda")

# join 7 grams

load("bspdt.Rda")
load("nspdt.Rda")
load("tspdt.Rda")

spdt <- b.spdt %>% rbind(n.spdt) %>%
    rbind(t.spdt) %>%
    group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts), doc_counts=sum(doc_counts))
# nsp.terms <- sum(spdt$terms_counts)
setorder(spdt, -terms_counts)

spdt <- spdt[doc_counts>2]

spdt$term_1 <- stri_extract_first(spdt$terms, regex="^[a-z]+(_[a-z]+){5}(?=_)")
spdt$last <- stri_extract_last(spdt$terms, regex="(?<=_)[a-z]+$")

# spdt$p <- spdt$terms_counts/nsp.terms
# spdt$logp <- log(spdt$p)
spdt$terms <- NULL
spdt$doc_counts <- NULL
setorder(spdt, term_1, -terms_counts)

save(spdt, file="spdt.Rda")

# join 8 grams

load("bodt.Rda")
load("nodt.Rda")
load("todt.Rda")

odt <- b.odt %>% rbind(n.odt) %>%
    rbind(t.odt) %>%
    group_by(terms) %>%
    summarise(terms_counts = sum(terms_counts), doc_counts=sum(doc_counts))
# no.terms <- sum(odt$terms_counts)
setorder(odt, -terms_counts)

odt <- odt[doc_counts>2]

odt$term_1 <- stri_extract_first(odt$terms, regex="^[a-z]+(_[a-z]+){6}(?=_)")
odt$last <- stri_extract_last(odt$terms, regex="(?<=_)[a-z]+$")

# spdt$p <- spdt$terms_counts/nsp.terms
# spdt$logp <- log(spdt$p)
odt$terms <- NULL
odt$doc_counts <- NULL
setorder(odt, term_1, -terms_counts)

save(odt, file="odt.Rda")