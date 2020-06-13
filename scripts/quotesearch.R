#sentences in quotes search job script

library(tidyverse)
library(rvest)
library(stringr)

get_first_google_link <- function(searchterm, sleeptime = NULL, root = FALSE) {
  url = URLencode(paste0("https://www.google.com/search?q=",searchterm))
  page <- xml2::read_html(url)
  # extract all links
  nodes <- rvest::html_nodes(page, "a")
  links <- rvest::html_attr(nodes,"href")
  # extract first link of the search results
  link <- links[startsWith(links, "/url?q=")][1]
  # clean it
  link <- sub("^/url\\?q\\=(.*?)\\&sa.*$","\\1", link)
  # get root if relevant
  if(root) link <- sub("^(https?://.*?/).*$", "\\1", link)
  # check if sleeptime was supplied, and is not negative, then sleep for that time
  if(!is.null(sleeptime)) if(sleeptime > 0) Sys.sleep(sleeptime) 
  return(link)
}

docs_quotes <- as.list(list.files(path = "./docs"))
names(docs_quotes) <- docs_quotes
docs_quotes <- lapply(docs_quotes, function(x) {
  path <- paste("./docs/", x, sep = "")
  content <- read_file(path)
  sentences_q <- strsplit(content, "[.]")
  sentences_q <- unlist(sentences_q)
  sentences_q <- as.list(sentences_q)
  q_urls <- lapply(sentences_q, function(y) {
    y < - paste('"', y, '"', sep = "")
    u <- get_first_google_link(y, sleeptime = 2, root = FALSE)
    y <- list(sentence = y, url = u)
  })
  x <- list(doc = x,path = path,content = content, search = q_urls)
})

saveRDS(docs_quotes, file = "./data/docs_quotes.RDS")