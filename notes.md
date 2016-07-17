# Notes

### The model

The model i used ended up being a version of the stupid-backoff ngram model. And i used a practical approach to tacle the problems at hand.  
To use an ngram model, the assumption from Markov-chains is used, that the next state (word), is only dependent on a finite number of earlier states (words). I my model case, the highest ngram used is a 8-gram. When no 

When examining the different models, i never really liked the smoothing aspect of many of the models where you are scraping probability mass from some word sequences you know exists in your training set, to word sequences that dont exists in your training set. I my opinion you end up giving probability to alot of word sequences that dont have any validity. They are just plain wrong, and shouldnt be given probability. I our case i like the model to only ever predict, what we know from our training set, and then try to put as many data as possible into the training set.  

I like the simplicity in the stupid-backoff model, and that it is close to the best with large amount of data, so i went in that direction using 90% of the supplied en-us data from [HC Corpora,](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)
pushing to use as high an ngram as possible. With text2vec i could go as high as 8-gram on my 8gb laptop. To enhance my model-theoretic abilities it would probably have been wise to work on a Knesser-Ney model. But not this time.

The models summary stats can be seen from this table, ordered from 1-gram to 8-gram. The model has been pruned to only include ngrams that exists in at least 5 different documents.

```
##    NAME      NROW MB
## 1:  udt   145,687  9
## 2:  bdt 1,276,663 31
## 3:  tdt 1,380,283 46
## 4:  fdt   673,733 33
## 5:  qdt   212,095 15
## 6:  sdt    64,145  6
## 7: spdt    26,759  3
## 8:  odt    16,134  2
```
The benefits of going as high as 8-gram is probably limited, with the amount of data used.

### Testing

Testing was done on the rest of the 10% of blogs, twitter and news documents, that wasnt included in the training set.  
My first model was only pruned to ngrams appearing in atleast 3 documents, but because the app, was very slow on shinyapps.io i have pruned it further down to appearing in atleast 5 documents.
The results for both models can be seen in this table, where s1, s3 and s5 shows hits in 1, 3 or 5 suggested words.  

#### Model pruned to ngrams appearing in atleast 3 documents 

```
##      type test.size    s1    s3    s5    s1.pct    s3.pct    s5.pct
## 1   blogs     79614 11168 17504 20785 0.1402768 0.2198608 0.2610722
## 2 twitter    206825 32273 48300 56790 0.1560401 0.2335308 0.2745800
## 3    news     96142 17813 26445 30446 0.1852780 0.2750619 0.3166774
```

#### Model pruned to ngrams appearing in atleast 5 documents

```
##      type test.size    s1    s3    s5    s1.pct    s3.pct    s5.pct
## 1   blogs     79614 10362 16416 19631 0.1301530 0.2061949 0.2465772
## 2 twitter    206825 29993 45576 54019 0.1450163 0.2203602 0.2611822
## 3    news     96142 16296 24589 28374 0.1694993 0.2557571 0.2951260
```

Perplexity isnt calculated because the model is trimmed to save space, instead this practical test is done, to show model accuracy. 

### Improvements to the model

Given time and more knowledge on my part, i would have liked to combine the ngram model, with some context/relationship/classification model where a sentence like "i cant see without my reading" would guess that the next word most probably is a noun and that has to be 'scored' in context with both see and reading.  
Inclusion of start and end sentence word-symbols could also improve the model.  
If it was up to me i also wouldnt combine twitter and news data in the same model. The data should be so different that separate models would be more beneficial. But for this small project the data is combined for peer evaluation.

### Sources used

* text2vec is used to create the model, as i found it the most efficient for creating ngrams.
* stringi is used to do string manipulation and regular expressions.
* data.table is used to store sorted data for easy extraction.
* dplyr - for basic manipulation
* shiny - for shiny stuff
* shinyjs - for even more shiny stuff
* knitr - to include rmarkdown in shiny
* pryr - easier display of object size
* rbenchmark - during testing efficient ngram generation/regex methods
