get_nord_table <- 
  function(url, xpath = 'h4'){
    Sys.sleep(0.1)
    webpage <- url %>% 
      rvest::read_html()
    
    
    # get h3 headings
    headings <- webpage %>% html_nodes(xpath) %>% html_text()
    
    # get raw text
    raw.text <- webpage %>% html_text()
    
    # split raw text on h3 headings and put in a list
    list.members <- list()
    raw.text.2 <- raw.text
    for (h in headings) {
      # split on headings
      b <- strsplit(raw.text.2, h, fixed=TRUE)
      # split members using \n as separator
      c <- strsplit(b[[1]][1], '\n', fixed=TRUE)
      # clean empty elements from vector
      c <- list(c[[1]][c[[1]] != ""])
      # add vector of member to list
      list.members <- c(list.members, c)
      # update text
      raw.text.2 <- b[[1]][2]
    }
    # remove first element of main list
    list.members <- list.members[2:length(list.members)]
    # add final segment of raw.text to list
    c <- strsplit(raw.text.2, '\n', fixed=TRUE)
    c <- list(c[[1]][c[[1]] != ""])
    list.members <- c(list.members, c)
    # add names to list
    names(list.members) <- headings
    
    #writeLines(text = raw.text, con = "rawText.txt")
    
    df <- list.members %>% enframe() %>% unnest() %>% spread(name, value)
    df
  }

list.members[[1]]
