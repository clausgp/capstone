Next word prediction
========================================================
author: Claus Gaarde Pedersen
date: Juli 2016
autosize: true
transition: zoom
transition-speed: slow

What is this about
========================================================
type: section
navigation: section

Capstone project for the Johns Hopkins University Data science specialization.

Natural language processing using a model to predict the next word in a sentence.

Algorithm
========================================================
type: section

Does the slide deck contain a description of the algorithm used to make the prediction?

N-gram model with stupid backoff
1-grams to 5-grams are created from the full files of blogs and news. Only n-grams with counts over 2 are used
Twitter is deliberately left out, to not let the use of abreviations contaminate the model.

I have implemented the algorithm with a practical approach focussing mostly on ranking the mostly likely prediction instead of a prober statistical distribution. 

Description of shiny app
========================================================
type: section

The shiny app can be found [here](http:www.rstudio.com)

In the text box you can type in sentence.  
After typing a space or punctuation, the model will present likely candidates of next words  
One of the candidates can be selected by its button or else continue to type  

Slide with links
========================================================
type: prompt

The model have been built with R in [Rstudio](http:www.rstudio.com) with the use of

packages :
 - text2vec
 - stringi
 - data.table
 - dplyr
 - shiny
 - pryr
 - rbenchmark

