#=======================================================
#    Project: webscrap for jobs'information
#    author: hh
#    date: 5/2022
#======================================================
# remove old environment

rm(list = ls())

# load and install packages

library(rvest) # for scrap data from webs
library(dplyr) # for data frame
library(stringi) # for text
library(stringr) # for text

# take url of page 1 in result of finding and delete number one at the end of url
url <- "https://timviecnhanh.com/vieclam/timkiem?q=&field_ids[]=23&exp_session_id=f4695e97-2326-4673-87f3-a6503e39723b&action=search&page="

# function to collect all links in one page

colect_link <- function(link){
  link %>% 
    read_html() %>% 
    html_elements("#__next > main > div > div.content > div:nth-child(4) > div > div > div.col-md-8 > div.jsx-2317772544 > table") %>% 
    html_elements("a") %>% 
    html_attr("href")  -> link_page
  link_page[str_detect(link_page,pattern = "max_box")] ->link_job_page
  str_c("https://timviecnhanh.com", link_job_page) -> fulllink_page
  
  return(fulllink_page)
}
# get links all page

url %>% 
  read_html() %>% 
  html_elements("#__next > main > div > div.content > div:nth-child(4) > div > div > div.col-md-8 > div.jsx-2317772544 > div.jsx-2317772544.mt-3.mt-0-mb > h3 > span.jsx-2317772544.title > span") %>% 
  html_text() %>% 
  str_replace_all(pattern = "[^1-9]","") %>% 
  as.numeric() -> numberlink
number_pages_link <- ceiling(numberlink/20)

all_url <-str_c(url, 1:number_pages_link)

# get link of all works

lapply(all_url, colect_link) %>% unlist()-> all_link_jobs

# columns' name to make table data
name_col <- c("Muc luong","Kinh nghiem","Trinh do","Tinh/Thanh pho","Nganh nghe","So luong tuyen dung",
              "Gioi tinh","Tinh chat cong viec","Hinh thuc lam viec","Thoi gian thu viec")
name_col2 <- c("THời gian cập nhat", "So luot xem")
pat <- "- Muc luong:|- Kinh nghiem:|- Trinh do:|- Tinh/Thanh pho:|- Nganh nghe:|- So luong tuyen dung:|- Gioi tinh:|- Tinh chat cong viec:|- Hinh thuc lam viec:|- Thoi gian thu viec:"
pat2 <- "Cap nhat: |Luot xem: "

# function to get data in one work
job_infor <- function(linkn){
  # get content
  linkn %>% 
    read_html() -> content_link
  
  # get job info
  
  content_link %>% 
    html_nodes(xpath = '//*[@id="__next"]/main/div/div[3]/div[5]/div/div/div[1]/div/article/div[3]') %>% 
    html_text() %>% 
    stri_trans_general("Latin-ASCII") %>% 
    str_replace_all(pattern = pat, "~") %>% 
    str_split(pattern = "~", simplify = TRUE) %>% 
    matrix(nrow =1) %>% 
    as.data.frame() %>% 
    select(-1) -> df_info
  names(df_info) <- name_col
  
  # get number view
  
  content_link %>% 
    html_nodes(xpath = '//*[@id="__next"]/main/div/div[3]/div[5]/div/div/div[1]/div/article/div[1]') %>% 
    html_text() %>% 
    stri_trans_general("Latin-ASCII") %>% 
    str_replace_all(pattern = pat2, "") %>% 
    str_split(pattern = "\\|", simplify = TRUE) %>% 
    matrix(nrow = 1) %>% 
    as.data.frame() %>%
    rename("ngay cap nhat" = V1, "So luot xem" = V2) %>% 
    mutate(Nguon = linkn) -> df_add
  
  # combind data frames
  bind_cols(df_info, df_add) -> df_full
  
  return(df_full)
}

# get data in all pages, make table and write file
lapply(all_link_jobs, job_infor)-> all_job_info

do.call("bind_rows", all_job_info) -> all_job_table
write.csv(all_job_table, "all_job_table.csv", row.names = TRUE)

## get detail infor about sth
# about muc luong
all_job_table %>% 
  group_by(`Muc luong:`) %>% 
  count()-> muc_luong

write.csv(muc_luong, "muc_luong.csv", row.names = TRUE)

# about trinh do
all_job_table %>% 
  group_by(`Trinh do:`) %>% 
  count()-> Trinh_do
write.csv(Trinh_do, "TRinh_do.csv", row.names = TRUE)

# about Tinh/thanhpho
all_job_table %>% 
  group_by(`Tinh/Thanh pho:`) %>% 
  count() -> Tinh_thanhpho

write.csv(Tinh_thanhpho, "Tinh_thanhpho.csv", row.names = TRUE)

# about hot view
all_job_table %>% 
  arrange(`So luot xem`) -> arrange_soluotxem
head(arrange_soluotxem) -> hot_view

write.csv(hot_view, "hot_view.csv", row.names = TRUE)

