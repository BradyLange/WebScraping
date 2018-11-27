# Developer: Brady Lange
# Date: 11/26/18
# Description: Web scraping techniques.

# Load libraries
library('rvest')

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

