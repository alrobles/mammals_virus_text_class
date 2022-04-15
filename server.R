library(shiny)
library("shinyWidgets")
library(text2vec)
library(glmnet)
library(shiny)
library(tidyverse)
library("shinycssloaders")
library(DT)
function(input, output, session) {
  
  # Show Selected Value in Console
  # observe({
  #   print(input$searchme)
  # })
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$searchme)
  })
  
  pubMedSearch <- reactive({

    if(input$go){
      if (input$searchme == "")
        return(NULL)
      
      
      else{
        pubmed_table <- get_pubmed_table(input$searchme, limit_search = input$obs)
        
        pmid <- pull(pubmed_table, pmid)
        if(length(pmid) > 200){
          pmid
          x <- seq_along(pmid)
          pmid_list <- split(pmid, ceiling(x/200))
          pubmed_table_id <- pmid_list %>% purrr::map_df(rcrossref::id_converter)
          
        } else {
          pubmed_table_id <- rcrossref::id_converter(pull(pubmed_table, pmid))
        }
        #pubmed_table_id <- rcrossref::id_converter(pull(pubmed_table, pmid))
        pubmed_table_id <- pubmed_table_id$records %>% select(pmid, pmcid, doi)
        pubmed_table <- full_join(pubmed_table, pubmed_table_id, by = "pmid")
        pubmed_table <- pubmed_table %>% 
          mutate(classification_score = map_dbl(search_abstract, function(x){ round( get_class_score(glmnet_classifier, vocab, x), 4)} )  )
        pubmed_table <- pubmed_table %>% arrange(desc(classification_score))
        pubmed_table <- left_join(pubmed_table, gmpd_reviewd_db) %>% 
          mutate(gmpd = ifelse(is.na(gmpd), "NO", "YES"))
        pubmed_table <- pubmed_table %>% 
          mutate(pdf = purrr::map_chr(pmcid, get_pmc_id))
        
        pubmed_table <- pubmed_table %>% 
          mutate(pdf = purrr::map_chr(pdf, function(x) paste0("<a href='",x,"'>",x,"</a>") ))
        
        
        
      } 
      if(all(is.na(pubmed_table))){
        return("No record found")
      } else{
        pubmed_table
      }
    }
    
    
    
    
  })
  
  output$tablepubmedsearch <- DT::renderDT(expr = {
    input$go
    
    datatable(
      pubMedSearch(), 
      plugins = "ellipsis",
      extensions = 'Buttons', 
      
      escape = FALSE,
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        columnDefs = list(list(
          targets = c(1,2),
          render = JS("$.fn.dataTable.render.ellipsis( 25, false )")
        ))
      )
    )
    
  }
    
    
    )
}

