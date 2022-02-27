# BrainStorm
Optimizing user's search queries for personalized social media results (Twitter, Medium)

# Project Description
Optimizing search results, though an important component
of the internet and the web world, is relatively less
discussed in research and practice. Few scholars have
identified that search can be time and money consuming
and this can be appalling when users are unsuccessful in
obtaining desired results in the process. Research on personalized
search aggregation have also been used mainly
for marketing and business purpose. The common method
in the field is ranking users preference and matching output
that serves recommendation systems. However, single
user-independent ranking model are often insufficient
to satisfy different users’ result preferences. Using BERT trained corpus,
the project creates word embeddings for
the universe of Bing COVID search queries.  Using their cosine similarity, I obtained
the vectors with the lowest distance on Twitter contents for recommendations.
The search query data with COVID-19
intent was used for this analysis.

# Method
I use Google’s BERT to
access the library for the learning of word embeddings
and classification. These words are pre-trained on a large
corpus and can be plugged in a variety of downstream
task models to automatically improve their performance.
Features that will be optimized in the classification
from each search result considering the Bing data are
popularity feature of each query and query content. This
will also allow me to identity search themes and top
words, and Twitter API to access recent tweets
