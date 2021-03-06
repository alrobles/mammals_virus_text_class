Mammal parasite paper recommender
================

## Methodology

This is a brief summary of the methodology we use

-   From global parasite databse () we extract all the papers  
-   Because we only have the name title of the paper we extract the
    title and search in Cross Reference (<https://www.crossref.org/>)
    the doi and the abstract if is avaliable.
-   Also we search with the DOI and title in PubMed
    (<https://pubmed.ncbi.nlm.nih.gov/>)
-   We finally build a table with title, DOI, abstract and year.
-   We label this information as “gmpd”
-   We generate a 1000 random abstracts sample in order to has a second
    class we call “unknown”
-   We tokenize each abstract and create a vocabulary for each word in
    abstract
-   We also take in accout the bigrams and remove the common stoping
    words in english.
-   We keep with a vocabulary of terms with a minimum term count of 20
    overall the papers
-   We vectorize or vocabulary and create a document term matrix wich
    contains
-   With the DTM we train a penalized logistic regression model. We
    model the two clases (gmpd and uknown) and interpret the probability
    as a measure of how an abstracts has information close to GMPD or
    not.
-   We build a shiny app that search an arbitrary string in PubMed and
    evaluates the abstracts of each paper founded acoording to our
    linear model

## App deploy

The app is accesible in
<https://alroble8.shinyapps.io/mammals_virus_text_class/>
