setwd("C:/Users/chenkailiu/Desktop/Case/1_服務中客戶/觀光局/期末報告/排抽獎文/2_維度標記_2018")

library(rio)
library(readxl)
library(parallel)
library(tidyverse)
library(stringr)
library(taRifx) #remove.factors

dimension_kw <- read_excel("dim_keyword.xlsx")
raw_data <- input_initial

#資料設定
dimension<-as.data.frame(dimension_kw[,2]) %>% remove.factors()#關鍵字(可用正規寫)
dimension_name<-dimension_kw[,1]#維度名稱

#內容欄位設定
dimension_data_content <- raw_data$內容 %>% as.tibble()

#標題欄位設定(是否標記標題)
dimension_data_title <- NULL
#dimension_data_title <- raw_data$標題 %>% as.tibble()

dimension_data_n <- 1:nrow(raw_data) %>% as.data.frame()

cl_num <- detectCores()
cl <- makeCluster(cl_num - 1)

#純英文標記調整
dim_notc_label <- as.character()
fun_dim_nonc <- function(x){grepl("^[A-Za-z0-9]+$",x)}
clusterExport(cl,"fun_dim_nonc")
dim_notc_label <- parLapply(cl,dimension,fun_dim_nonc) %>% unlist()
dim_notc_label <- sub("TRUE",replacement = 1, dim_notc_label)
dim_notc_label <- sub("FALSE",replacement = 0, dim_notc_label)
dim_notc_label <- dim_notc_label %>% as.integer() %>% as.data.frame()
colnames(dim_notc_label) <- "notchinese"
dimension2 <- cbind(dimension,dim_notc_label)

#進行標記
dimension_data_freq <- dimension_data_n
dimension_data_doc <- dimension_data_n

label_notc <- "[\u4e00-\u9fa5| ]subname[\u4e00-\u9fa5| ]|^subname[\u4e00-\u9fa5| ]|[\u4e00-\u9fa5| ]subname$|^subname$"
fun_notc_sub <- function(x){gsub("subname",x,label_notc)}
clusterExport(cl,"label_notc")

pb<-txtProgressBar(min = 1, max = nrow(dimension2), style = 3)
for(i in 1:nrow(dimension2))
{
  fun_dim <- function(x){str_count(x,dimension2[i,1])}
  clusterExport(cl,"i")
  clusterExport(cl,"str_count")
  clusterExport(cl,"dimension2")
  clusterExport(cl,"fun_dim")
  
  if( dimension2[i,2] == 1 )
  {
    dimension2[i,1] <- parLapply(cl,dimension2[i,1],fun_notc_sub) %>% unlist() %>% tibble()
  }
  dim_data_content <- parLapply(cl,dimension_data_content,fun_dim) %>% unlist() %>% as.integer() %>% as.tibble()
  dim_data_content[is.na(dim_data_content)==TRUE,] <- 0
  
  if(is.null(dimension_data_title[1,])==FALSE)#標標題和內容
    {
      dim_data_title <- parLapply(cl,dimension_data_title,fun_dim) %>% unlist() %>% as.integer() %>% as.tibble()
      dim_data_title[is.na(dim_data_title)==TRUE,] <- 0
      dim_data_freq <- dim_data_content+dim_data_title
    }else{dim_data_freq <- dim_data_content}
  
  dim_data_doc <- dim_data_freq
  dim_data_doc[dim_data_doc[,1]>1,] <- 1
  
  dimension_data_freq<-cbind(dimension_data_freq,dim_data_freq)
  dimension_data_doc<-cbind(dimension_data_doc,dim_data_doc)
  setTxtProgressBar(pb, i)
}
stopCluster(cl)


#欄位命名
t_dimension<-as.character(t(dimension_name))
colnames(dimension_data_freq)<-c("編號",t_dimension)
colnames(dimension_data_doc)<-c("編號",t_dimension)

#輸出
#export(dimension_data_freq, "dimension_data_freq.xlsx")
#export(dimension_data_doc, "dimension_data_doc.xlsx")
write.csv(dimension_data_freq,file="dimension_data_freq.csv",row.names = FALSE)
write.csv(dimension_data_doc,file="dimension_data_doc.csv",row.names = FALSE)