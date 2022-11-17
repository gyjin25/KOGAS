
setwd("/Users/user/Dropbox/KOGAS/Main")
getwd()
dir()

library(readxl)
library(tidyverse)

# ============= Reading All Sheets in xlsx =================== 

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

# ============================================================
# 2022 - 05 ==================================================

this_month <- "07"
last_month <- "06"
days <- "31"

date <-  seq(ymd(paste("2022-",this_month,"-","01",sep="")), 
             ymd(paste("2022-",this_month,"-",days,sep="")), by = "days")

# Last Month 

assign(paste("df",last_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               last_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 계) %>% 
         arrange(일자) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0)) # 448

# This Month

assign(paste("df",this_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               this_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 계) %>% 
         arrange(일자) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0)) # 446

assign(paste("df22",this_month,sep=""),
       data.frame(date,
                  rowSums(get(paste("df",this_month,sep=""))
                          [,2:dim(get(paste("df",this_month,sep="")))[2]])) %>%
         set_names("date","total"))

load(paste("/Users/user/Dropbox/KOGAS/R Data/df_tot_22",
           last_month,".RData",sep=""))

df_final %>% head()

df_final <- plyr::rbind.fill(df_final, get(paste("df22",this_month,sep=""))) %>% 
  replace(is.na(.), 0)

df_final %>% tail()


save(df_final, file=paste("/Users/user/Dropbox/KOGAS/R Data/df_tot_22",
                       this_month,".RData",sep=""))


df_final %>% head()


# 20010101 ~ 20180228 Data 병합 ================================

bplt <- read.table("/Users/user/Dropbox/KOGAS/Raw Data/DPGbplt.txt", sep="\t", header=T)
bplt %>% head() # 01-01-01
bplt %>% tail() # 18-02-18

# 중복 제외 (원자력 + 국내탄 + 유연탄 + 열병합 + 복합 + 나머지(res_tot))
bplt_sum <- rowSums(bplt %>% select("nc_tot","ac_tot","bc_tot","chp_tot","lng_tot","res_tot"))
bplt_sum %>% head()

# res_tot = 나머지 all sum 인지 확인
sum(bplt %>% select("thd_tot","hoil_tot","ic_tot","sun_tot","wind_tot","tide_tot","fuel_tot",
                "mchp_tot","etc_tot") %>% rowSums() != bplt %>% select("res_tot")) # 0 : 맞다

bplt_final_sum <- data.frame(seq(as.Date("2001-01-01"), as.Date("2018-02-28"), by=1), bplt_sum) %>% 
  set_names(c("date","total"))

bplt_final_sum %>% tail() # 2018-02-28 & 6268 관측치 확인 완료

# ===========================================================

df_final_sum <- rbind(bplt_final_sum,df_final)

df_final_sum %>% head() # 2001-01-01
df_final_sum %>% tail() # 2022-07-31
df_final_sum[,2] %>% ts.plot()

write.csv(df_final_sum, "/Users/user/Dropbox/KOGAS/Final CSV File/총발전량.csv", row.names=FALSE)



