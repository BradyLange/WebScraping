# Developer: Brady Lange
# Date: 11/26/18
# Description: Web scraping techniques.

# Load libraries
library(tidyverse)
library(magrittr)
library(rvest)
library(stringr)

# URL for website to be scraped
url <- "https://www.indeed.com/jobs?q=computer+science+internship&l="

# Retrieve website's HTML 
webpage <- read_html(url)

# Extract HTML selector from complete HTML code
elem_data <- html_nodes(webpage,".jobtitle")

# Convert element data to text
scraped_text <- html_text(elem_data)

# XML Web Scraping
library(XML)
library(data.table)
pages <- seq(10, 1000, by = 10)

urls <- rbindlist(lapply(pages, function(x)
{
  url <- paste("https://www.indeed.com/jobs?q=computer+science+internship&start=", x, sep = "") 
  data.frame(url)
}), fill = T)

jobLocations <- rbindlist(apply(urls, 1, function(url) 
{
  doc1 <- htmlParse(url)
  locations <- getNodeSet(doc1, '//*[@id="p_4b106814597bb65b"]/span')
  data.frame(sapply(locations, function(x) 
  {
    xmlValue(x)
  }))
}), fill = T)


# Web scrape multiple URL's (pages)
library(purrr)
# Dynamic URL (%d) 
url <- "https://www.indeed.com/jobs?q=computer+science+internship&start=%d"

map_df(seq(10, 100, by = 10), function(x) 
{
  page <- sprintf(url, x) %>%
    read_html(.)
  tibble(Title = html_nodes(page, ".jobtitle") %>%
           html_text(.))
}) 


# Web scrape using server to handle JavaScript - Selenium 
# -- rvest can't handle JavaScript well, connect to server -- 
# URL for website to be scraped
url <- "https://www.monster.com/jobs/search/?q=computer-science-internship"

# Function that scrapes Monster.com's job application descriptions
scrape_desc <- function(url, num_pages = 1)
{
  # Error check
  # Monsters webpage can only load 9 pages 
  if (num_pages > 9)
  {
    print("Monster's job search listing can only load up to 9 pages. Setting number of pages to 9.")
    num_pages <- 9
  }
  as.character(url)
  as.numeric(num_pages)

  tryCatch(
  {
    # Load Selenium 
    library(RSelenium)
  }, 
  error = function(e) 
  {
    # Install/Update Selenium package
    install.packages("RSelenium")
  }
  )

  # Connect to Chrome browser
  chromeDriver <<- rsDriver() 
  client <- chromeDriver$client
  # Allow time to connect to server (1 second)
  Sys.sleep(1)
  # Set wait time for the driver to search for elements (5 seconds)
  client$setImplicitWaitTimeout(milliseconds = 5000)
  # Set wait time to time-out on page-load (10 seconds)
  client$setTimeout(type = "page load", milliseconds = 10000)
  
  print("Loading web page...")
  
  # Instantiate variable for the returned text 
  all_text <- NULL
  # Navigate to URL to be web scraped 
  client$navigate(url)
  Sys.sleep(0.25)
  # Load more jobs applications
  for (i in 1:num_pages)
  {
    # Retrieve the load button element on the webpage in element form
    load_elem <- client$findElement(using = "css selector", "#loadMoreJobs")
    # Click load more applications button
    load_elem$clickElement()
    # Let page load for 0.10 seconds
    Sys.sleep(0.10)
  }
  # Retrieve the updated URL
  url <- client$getCurrentUrl()
  # Navigate to the update URL
  client$navigate(url)
  # Let page load for 0.10 seconds
  Sys.sleep(0.10)
  # Retrieve the application cards on the webpage in element form
  app_card_elem <- client$findElements(using = "css selector", "#SearchResults .card-content")
  num_card_elem <- length(app_card_elem)
  print(str_c("Scraping ", url, " for job application descriptions..."))
  # Scrape applications description text
  for (i in 1:num_card_elem)
  {
    # Prevent stale element references, refresh element references
    app_card_elem <- client$findElements(using = "css selector", "#SearchResults .card-content")
    # Click application card
    app_card_elem[[i]]$clickElement()
    # Let page load for half of a second
    Sys.sleep(0.50)
    # Find the element that is located in the dynamic webpage, wrapped in try block to avoid pages that don't contain selector
    desc_elem <- try(client$findElement(using = "css selector", "#JobBody .card-content"))
    # Retrieve the elements text, split it by next line character, trim the white space, unnest tokens, keep only unique tokens
    # to avoid inaccurate analysis for each job application 
    text <- tibble(text = desc_elem$getElementText()[[1]] %>% 
      str_split("\n", simplify = T) %>% 
      str_trim(.)) %>%
      unnest_tokens(input = text, output = token) %>%
      unique(.)
    # If no text is in return variable set text to the return variable
    if (is.null(all_text))
    {
      all_text <- text
    }
    # Row bind specific elements text to all of the other applications text
    else 
    {
      all_text <- rbind(all_text, text)
    }
  }
  # Stop the Selenium server
  chromeDriver[["server"]]$stop() 
  
  # Print all of the applications description text
  print("Finished web scraping successfully!")
  all_text
}

apps_text <- scrape_desc(url, 9)

# Selenium methods:
# Connect to Chrome browser 
chromeDriver <- rsDriver() 
client <- chromeDriver$client

l <- client$refresh()
l$

# Find the element that is located in the dynamic webpage 
desc_elem <- client$findElement(using = "css selector", "#JobBody .card-content")

# Find multiple elements that is located in the dynamic webpage 
desc_elems <- client$findElements(using = "css selector", "#JobBody .card-content")

# Retrieve the elements text, split it by next line character, and trim the white space
tibble(text = desc_elem$getElementText()[[1]] %>% 
  str_split("\n", simplify = T) %>% 
  str_trim(.))

# Retrieve the elements text by unlisting the list type, split it by next line character, and trim the white space
tibble(text = unlist(lapply(desc_elems, function(x) x$getElementText())) %>% 
  str_split("\n", simplify = T) %>% 
  str_trim(.))

# Close server connection
client$close()

# Stop the Selenium server
chromeDriver[["server"]]$stop() 

# Remove server variable 
rm(chromeDriver)
# Garbage collect server variable
gc(chromeDriver)
