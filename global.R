library(shiny)
library("shinyWidgets")
library(text2vec)
library(glmnet)
library(shiny)
library(tidyverse)
library("shinycssloaders")
library(DT)
glmnet_classifier <- readr::read_rds("glmnet_classifier.rds")
vocab = read_rds("vocab.rds")
readr::read_csv("data-raw/2022-04-06 17_28_36_df_title_abstracts_doi.csv") %>% 
  na.exclude()

gmpd_reviewd_db <- read_csv("data-raw/gmpd_reviewd_db.csv")


get_pmc_id <- function(pmcid){
  if(is.na(pmcid)){
    return("")
  }
  if(is.null(pmcid)){
    return("")
  }
  
  if(pmcid == ""){
    return("")
  }
  else{
    return(paste0(
      "https://www.ncbi.nlm.nih.gov/pmc/articles/"
      ,pmcid
      ,"/pdf/"
    ))
      
  }
}





get_pubmed_table <- function(search_query, limit_search = 100){
  {
    another_search <- rentrez::entrez_search(db="pubmed", term=search_query, retmax = limit_search)
    if(length(another_search$ids) == 0){
      df = tibble(search_title = NA, search_abstract = NA, classification_score = NA,  pmid=NA, pmcid=NA, doi=NA )
    } else {
      paper_rec <- rentrez::entrez_fetch(db="pubmed", id=another_search$ids, rettype="xml", parsed=TRUE)
      paper_rec_list <- XML::xmlToList(paper_rec)
      
      df <-  purrr::map_df(paper_rec_list, function(x){
        search_title <- x$MedlineCitation$Article$ArticleTitle
        search_abstract <- x$MedlineCitation$Article$Abstract$AbstractText 
        pmid <- x$MedlineCitation$PMID$text
        
        if(length(search_title) > 2){
          search_title = paste0(unlist(search_title), collapse = " ")
        }
        
        if(length(search_abstract) > 2){
          search_abstract = paste0(unlist(search_abstract), collapse = " ")
        }
        
        if(length(pmid) > 2){
          pmid = paste0(unlist(pmid), collapse = " ")
        }
        
        if(is.list(search_title)  && identical(names(search_title) , c("text", ".attrs") ) ){
          search_title = search_title$text
        }
        
        if(is.list(search_abstract)  && identical(names(search_abstract) , c("text", ".attrs") ) ){
          search_abstract = search_abstract$text
        }
        
        tibble(
          search_title = paste0(unlist(search_title), collapse = " ")
          ,search_abstract = paste0(unlist(search_abstract), collapse = " ")
          ,pmid = paste0(unlist(pmid), collapse = " ")
        ) 
      })  
    }
  }
  
  
  return(df)
}

get_class_score <- function(model = glmnet_classifier, vocabulary = vocab, search_abstract){
  prep_fun = function(x) {
    # make text lower case
    x = str_to_lower(x)
    # remove non-alphanumeric symbols
    x = str_replace_all(x, "[^[:alnum:]]", " ")
    # collapse multiple spaces
    str_replace_all(x, "\\s+", " ")
  }
  
  vocabulary <- vocab
  vectorizer <-  vocab_vectorizer(vocabulary)
  
  classification_score <- if(nchar(search_abstract) > 10){
    trial_text <-search_abstract
    trial_text <- prep_fun(trial_text)
    it_test = text2vec::itoken(trial_text, progressbar = FALSE)
    dtm_test = create_dtm(it_test, vectorizer)
    preds = 1 -  predict(model, dtm_test, type = 'response')[,1]
    return(preds)
  } else {
    return(0)
  }
}  


