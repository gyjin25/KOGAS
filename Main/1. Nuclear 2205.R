
setwd("/Users/user/Dropbox/KOGAS/Main")
getwd()
dir()

library(readxl)
library(tidyverse)
library(lubridate)

# ============= Reading All Sheets in xlsx =================== 

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


# Daily Generation (발전량 자료)

# ============================================================
# 2022 - 07 ==================================================


this_month <- "07"
last_month <- "06"
days <- "31"

total_obs <- seq(ymd("2001-01-01"), 
                 ymd(paste("2022-",this_month,"-",days,sep="")), by = "days") %>% length()




paste("df22",this_month,sep="")
  
assign(paste("df22",this_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               this_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 발전연료2, 계) %>% 
         arrange(일자) %>% 
         filter(발전연료2=='7.원자력') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0)) # 27 ( 6월 신한울 추가 )

load(paste("/Users/user/Dropbox/KOGAS/R Data/df_nc_22",
           last_month,".RData",sep=""))

df_nc <- plyr::rbind.fill(df_nc, get(paste("df22",this_month,sep=""))) %>% replace(is.na(.), 0)

save(df_nc, file=paste("/Users/user/Dropbox/KOGAS/R Data/df_nc_22",
                       this_month,".RData",sep=""))

# ================================================================================
# Matching colnames
 
df_nc %>% head()

df_nc <- df_nc %>% relocate("신고리#4", .after = "신고리#3")
names <- df_nc %>% colnames()
names

colnames(df_nc) <- c("date","nc_kr1_Sum","nc_kr2_Sum","nc_kr3_Sum","nc_kr4_Sum","nc_nkr1_Sum","nc_nkr2_Sum","nc_nkr3_Sum","nc_nkr4_Sum","nc_nws1_Sum","nc_nws2_Sum","nc_ws1_Sum","nc_ws2_Sum","nc_ws3_Sum","nc_ws4_Sum","nc_yg1_Sum","nc_yg2_Sum","nc_yg3_Sum","nc_yg4_Sum","nc_yg5_Sum","nc_yg6_Sum","nc_uc1_Sum","nc_uc2_Sum","nc_uc3_Sum","nc_uc4_Sum","nc_uc5_Sum","nc_uc6_Sum","nc_nuc1_Sum") # 2206 신한울#1 추가가

# Loading previous data

nc <- read.table("/Users/user/Dropbox/KOGAS/Raw Data/DPG_nc.txt", sep="\t", header=T)
nc_old <- nc[1:5478,] # until 2015-12-31

nc_old %>% head(3)

Date <- seq(lubridate::ymd("2001-01-01"), 
            lubridate::ymd(paste("2022-",this_month,"-",days,sep="")), by = "days") %>% 
  as.character() %>% str_replace_all("-","") %>%  as.data.frame() %>% set_names("date") 



nc_final <- data.frame(Date, 
                       plyr::rbind.fill(nc_old %>% select(-date), 
                                        df_nc %>% select(-date))) %>% 
  replace(is.na(.),0)


nc_final %>% dim() # 7882 / 28

# 일별 발전량 (Daily Generation of NC)

write.csv(nc_final, 
          paste("/Users/user/Dropbox/KOGAS/Final CSV File/원자력 (NC) 호기별 일별 발전량.csv"), 
          row.names=FALSE)





# ======================================
# 용량 자료 구축 =======================
# ====================================== 


DF <- rep(0) # Date of Start
SQ <- rep(0) # Days from 2001-01-01 to Start Date 

nc_final %>% head()

for (i in 2:28) { 
  
  DF[i-1] <- nc_final[which(nc_final[,i]!= 0)[1],1] 
  SQ[i-1] <- which(nc_final[,i]!= 0)[1]

}

names <- nc_final %>% select(-'date') %>% colnames()

data.frame(names,DF,SQ) # 17번째 관측치 한빛 5부터 새로 들어옴 2001년 12월 19일

# ======================================
# 용량 기초 구조 파일 ==================
# ====================================== 

pp_info <- read_xlsx("/Users/user/Dropbox/KOGAS/PP_Info_20220119.xlsx") %>% 
  select(Code,Cat,Cap) %>% 
  filter(Cat == 'NC') %>% select(-Cat) %>% 
  spread(key = Code, value = Cap) %>% 
  as.data.frame()

pp_info[1,]

colnames(pp_info) <- paste("nc_", colnames(pp_info), "_Sum", sep = "")

pp_info <- pp_info[,names] # Re-ordering colnames 
pp_info

nc_capa <- nc_final %>% select(-"date") %>% replace(!0, 0)
nc_capa %>% colnames == pp_info %>% colnames # All TRUE = ordering correct
capa_seq <- pp_info[1,] %>% as.numeric() # capacity
capa_seq
my_seq <- SQ # ordering date
my_seq %>% length()
data <- data.frame(1:27, my_seq, capa_seq)
data

df_nc_change <- nc_final %>% select(-"date")
df_nc_change %>% head() # 27
df_nc_change %>% dim()
nc_capa %>% head()

# =======================================================================
# =======================================================================

for (i in 17:27) {
  
  nc_capa[as.numeric(data[i,][2]), as.numeric(data[i,][1])] <- 
    df_nc_change[as.numeric(data[i,][2]), as.numeric(data[i,][1])]/24
  
  for (j in 0:180){
  
    nc_capa[as.numeric(data[i,][2])+j+1, as.numeric(data[i,][1])] <- min(max(
      nc_capa[as.numeric(data[i,][2])+j, as.numeric(data[i,][1])], 
      df_nc_change[as.numeric(data[i,][2])+j+1, as.numeric(data[i,][1])]/24), as.numeric(data[i,][3]))
    
  }
  
  for (k in 181:total_obs){
    
    nc_capa[as.numeric(data[i,][2])+k+1, as.numeric(data[i,][1])] <-
      as.numeric(data[i,][3])
    
  }
}

nc_capa %>% dim() # 15712
df_nc_change %>% dim() # 7882 27

for (i in 1:16) {
  
  nc_capa[,i] <- data[i,][3]
  
}


# ====================================
# REVERSE 180동안 없으면 0 
# 이건 수동 원자력은 종료일 명확 
# 현재까지 월성#1 & 고리#1 폐쇄

nc_reverse <- nc_final[nrow(nc_final):1,]
DF_reverse <- rep(0)
SQ_reverse <- rep(0)

for (i in 2:28) { 
  DF_reverse[i-1] <- nc_reverse[which(nc_reverse[,i] > 100)[1],1] 
  SQ_reverse[i-1] <- which(nc_reverse[,i] > 100)[1]
}

DF_reverse; SQ_reverse

df_reverse <- data.frame(names,DF_reverse,SQ_reverse)
df_reverse %>% arrange(-SQ_reverse)

# ==================================================================================

capa_final <- data.frame(nc_final['date'], nc_capa[1:total_obs,]*24)
capa_final %>% dim()
capa_final %>% colnames()

capa_final %>% head()

capa_final[5993:total_obs,14] <- 0 # ws1 close 2017-05-29 
capa_final[6013:total_obs,2] <- 0 # kr1 close 2017-06-18

write.csv(capa_final,
          "/Users/user/Dropbox/KOGAS/Final CSV File/원자력 (NC) 호기별 일별 용량.csv", 
          row.names = FALSE)





