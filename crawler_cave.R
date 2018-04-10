

#=================================
#           Phò Project
#=================================
library(tidyverse)
library(magrittr)
library(rvest)
library(stringr)
library(httr)
library(httr)
rm(list = ls())

# Hàm lấy link giới thiệu về các em phò: 


get_link_pho <- function(link) {
  Sys.sleep(5)
  m <- read_lines(link)
  m1 <- m
  m <- m[str_detect(m, "a href=")]
  m <- m[!str_detect(m, "id")]
  m <- m[str_detect(m, "itemprop=")]
  
  u <- str_locate_all(m, "https:/") %>% 
    unlist() %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame()
  
  v <- str_locate_all(m, "title=") %>% 
    unlist() %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    as.data.frame()
  
  str_sub(m, u$V1, v$V2 - 9) %>% 
    as.character() -> link
  
  
  
  m1 <- m1[str_detect(m1, "li class=")]
  
  id <- m1 %>% 
    str_sub(10, 30) %>% 
    str_replace_all("[^0-9]", "") %>% 
    as.numeric() 
  
  return(data.frame(link = link, 
                    id_product = id, 
                    link_id = paste0(link, id)))
}


# Hàm lấy ra link _ id product của tất cả các em phò ở một khu vực: 

all_sex_worker_links <- function(base_url, n_pages) {
  link <- paste0(base_url, 1:n_pages)
  link <- paste0(link, "/")
  
  all_df <- data.frame()
  for (i in 1:n_pages) {
    job_inf <- tryCatch({get_link_pho(link[i])}, 
                        error = function(i){return(data.frame())})
    all_df <- bind_rows(all_df, job_inf)
    
  }
  
  return(all_df)
}



# Hàm lấy thông tin về các em với thông tin đầu vào là: (1) link của 
# các em, và (2) mã niêm yết. 

get_information_sex_worker <- function(link) {
  link <- as.character(link)
  
  link1 <- link %>% str_sub(1, str_count(link) - 4)
  id <- link %>% str_sub(str_count(link) - 3, str_count(link))
  
  x1 <- paste0(paste0('//*[@id="product-', id), '"]/div[2]/div[1]/div/ul/li[1]')
  x2 <- paste0(paste0('//*[@id="product-', id), '"]/div[2]/div[1]/div/ul/li[2]')
  x3 <- paste0(paste0('//*[@id="product-', id), '"]/div[2]/div[1]/div/ul/li[3]')
  x4 <- paste0(paste0('//*[@id="product-', id), '"]/div[2]/div[1]/div/ul/li[4]')
  x5 <- paste0(paste0('//*[@id="product-', id), '"]/div[2]/div[1]/div/ul/li[5]')
  x6 <- paste0(paste0('//*[@id="product-', id), '"]/div[2]/div[1]/div/ul/li[6]')
  x7 <- paste0(paste0('//*[@id="product-', id), '"]/div[2]/div[1]/div/ul/li[7]')


  Sys.sleep(5)
  m <- read_html(link1)
  nick_name <- m %>% 
    html_nodes(xpath = x1) %>% 
    html_text() %>% 
    as.character()
  
  if (str_count(nick_name) == 0) {nick_name <- NA}
  
  location <- m %>% 
    html_nodes(xpath = x2) %>% 
    html_text()
  
  if (str_count(location) == 0) {location <- NA}
  
  
  price <- m %>% 
    html_nodes(xpath = x3) %>% 
    html_text()
  
  if (str_count(price) == 0) {price <- NA}
  
  phone_number <- m %>% 
    html_nodes(xpath = x4) %>% 
    html_text()
  
  if (str_count(phone_number) == 0) {phone_number <- NA}
  
  pass <- m %>% 
    html_nodes(xpath = x5) %>% 
    html_text()
  
  if (str_count(pass) == 0) {pass <- NA}
  
  
  listed_date <- m %>% 
    html_nodes(xpath = x6) %>% 
    html_text()
  
  if (str_count(listed_date) == 0) {listed_date <- NA}
  
  
  status <- m %>% 
    html_nodes(xpath = x7) %>% 
    html_text()
  
  if (str_count(status) == 0) {status <- NA}

  
  all_df <- data.frame(nick_name = nick_name, 
                       location = location, 
                       price = price, 
                       phone_number = phone_number, 
                       pass = pass, 
                       listed_date = listed_date, 
                       status = status, 
                       link_source = link1)
  
  return(all_df)
}



# Hàm lấy ra thông tin của các em khi đầu vào là một dãy link: 

get_information_for_group_sex_worker <- function(list_link) {
  list_link <- as.character(list_link)
  n <- length(list_link)
  all_df <- data.frame()
  for (i in 1:n) {
    job_inf <- tryCatch({get_information_sex_worker(list_link[i])}, 
                        error = function(i){return(data.frame())})
    all_df <- bind_rows(all_df, job_inf)
    
  }
  return(all_df)
}

# Lấy dữ liệu các em phò ở HN: 
hn <- all_sex_worker_links("https://gaigoivl.com/khu-vuc/gai-goi-ha-noi/page/", 14)
pho_hn <- get_information_for_group_sex_worker(hn$link_id)

# Lấy dữ liệu các em phò SG: 
sg <- all_sex_worker_links("https://gaigoivl.com/khu-vuc/gai-goi-sai-gon-tp-hcm/page/", 15)
pho_sg <- get_information_for_group_sex_worker(sg$link_id)







