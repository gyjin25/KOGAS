
setwd("/Users/user/Dropbox/KOGAS/Main")
getwd()

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

# ===========================================================


# Daily Generation (발전량 자료)

# ============================================================
# 2022 - 05 ==================================================

this_month <- "07"
last_month <- "06"
days <- "31"

total_obs <- seq(ymd("2001-01-01"), 
                 ymd(paste("2022-",this_month,"-",days,sep="")), by = "days") %>% length()

obs <- seq(ymd("2016-01-01"), # 2016-01-01 이후 총 관측치 개수
           ymd(paste("2022-",this_month,"-",days,sep="")), by = "days") %>% length()


# Last Month 

assign(paste("df22",last_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               last_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 발전연료2, 계) %>% 
         arrange(일자) %>% 
         filter(발전연료2=='5.LNG(난방)') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0)) # 73

# This Month

assign(paste("df22",this_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               this_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 발전연료2, 계) %>% 
         arrange(일자) %>% 
         filter(발전연료2=='5.LNG(난방)') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0)) # 73


# Load past chp generation data

load(paste("/Users/user/Dropbox/KOGAS/R Data/df_chp_22",
           last_month,".RData",sep=""))

# combine old & new chp gen data

df_chp <- plyr::rbind.fill(df_chp, get(paste("df22",this_month,sep=""))) %>% replace(is.na(.), 0)


save(df_chp, file=paste("/Users/user/Dropbox/KOGAS/R Data/df_chp_22",
                        this_month,".RData",sep=""))

df_chp %>% dim() # 75 columns
df_chp %>% colnames()

# ============================================================
# LH 아산

# Last Month 

assign(paste("df22",last_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               last_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 발전연료2, 계) %>% 
         arrange(일자) %>% 
         filter(발전연료2=='기타') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0) %>% 
         select("아산GT#1","아산GT#2","아산ST#1" )) 


# This Month

assign(paste("df22",this_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               this_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 발전연료2, 계) %>% 
         arrange(일자) %>% 
         filter(발전연료2=='기타') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0) %>% 
         select("아산GT#1","아산GT#2","아산ST#1" )) 


load(paste("/Users/user/Dropbox/KOGAS/R Data/df_LH_22",
           last_month,".RData",sep=""))

df_LH %>% colnames()
df2205 %>% colnames()

df_LH <- plyr::rbind.fill(df_LH, get(paste("df22",this_month,sep=""))) %>% replace(is.na(.), 0)

save(df_LH, file=paste("/Users/user/Dropbox/KOGAS/R Data/df_LH_22",
                       this_month,".RData",sep=""))


# ============================================================
# CHP 발전량

df_chp["아산GT#1"] <- df_LH$`아산GT#1`
df_chp["아산GT#2"] <- df_LH$`아산GT#2`
df_chp["아산ST#1"] <- df_LH$`아산ST#1`

chp <- read.table("/Users/user/Dropbox/KOGAS/Raw Data/DPG_chp.txt", sep="\t", header=T)

date <- seq(ymd("2016-01-01"), # 2016-01-01 이후 날짜
            ymd(paste("2022-",this_month,"-",days,sep="")), by = "days")

df_chp_final <- as.data.frame(date)
df_chp_final %>% head()

# 호기 통합 chp dataframe 제작

df_chp_final['chp_su_Sum'] <- rowSums(df_chp[,c("서울#4","서울#5")]) # 2

df_chp_final['chp_bd_Sum'] <- rowSums(df_chp[,c("분당GT#1","분당GT#2","분당GT#3","분당GT#4","분당GT#5","분당GT#6","분당GT#7","분당GT#8","분당ST#1","분당ST#2")]) # 10

df_chp_final['chp_is_Sum'] <- rowSums(df_chp[,c("일산GT#1","일산GT#2","일산GT#3","일산GT#4","일산GT#5","일산GT#6","일산ST#1","일산ST#2")]) # 8

df_chp_final['chp_ay_Sum'] <- rowSums(df_chp[,c("안양GT#1","안양GT#2","안양GT#3","안양GT#4","안양ST#1")]) # 5

df_chp_final['chp_ay21_Sum'] <- rowSums(df_chp[,c("안양열병합2-1CC GT","안양열병합2-1CC ST")]) # 2

df_chp_final['chp_ay22_Sum'] <- rowSums(df_chp[,c("안양열병합2-2CC GT","안양열병합2-2CC ST")]) # 2


df_chp_final['chp_bc_Sum'] <- rowSums(df_chp[,c("부천GT#1","부천GT#2","부천GT#3","부천ST#1")]) # 4

df_chp_final['chp_pk_Sum'] <- rowSums(df_chp[,c("판교GT","판교ST")]) # 2

df_chp_final['chp_pj_Sum'] <- rowSums(df_chp[,c("파주GT#1","파주GT#2","파주ST#1")]) # 3

df_chp_final['chp_kk_Sum'] <- rowSums(df_chp[,c("광교GT","광교ST")]) # 2

df_chp_final['chp_hws_Sum'] <- rowSums(df_chp[,c("화성GT#1","화성GT#2","화성ST#1")]) # 3

df_chp_final['chp_sd_Sum'] <- rowSums(df_chp[,c("송도GT#1","송도GT#2","송도ST#1")]) # 3

df_chp_final['chp_yj_Sum'] <- rowSums(df_chp[,c("양주GT#1","양주GT#2","양주ST#1")]) # 3

df_chp_final['chp_dgh_Sum'] <- df_chp[,c("대구그린파워 GT")] # 1

df_chp_final['chp_sj_Sum'] <- rowSums(df_chp[,c("세종GT#1","세종GT#2","세종ST#1")]) # 3

df_chp_final['chp_bn_Sum'] <- rowSums(df_chp[,c("별내GT#1","별내GT#2","별내ST#1")]) # 3

df_chp_final['chp_lh_Sum'] <- rowSums(df_chp[,c("아산GT#1","아산GT#2","아산ST#1")]) # 3

df_chp_final['chp_sw_Sum'] <- rowSums(df_chp[,c("수완GT#1","수완GT#2","수완ST#1")]) # 3

df_chp_final['chp_icair_Sum'] <- rowSums(df_chp[,c("인천공항GT#1","인천공항GT#2","인천공항ST#1")]) # 3

df_chp_final['chp_hn_Sum'] <- rowSums(df_chp[,c("하남열병합GT#1","하남열병합ST#1")]) # 2

df_chp_final['chp_cc_Sum'] <- df_chp[,c("춘천열병합GT")] # 1

df_chp_final['chp_dt_Sum'] <- rowSums(df_chp[,c("동탄열병합1GT","동탄열병합1ST","동탄열병합2GT","동탄열병합2ST")]) # 4

df_chp_final['chp_jk_Sum'] <- rowSums(df_chp[,c("부산정관1GT","부산정관1ST")]) # 2

df_chp_final['chp_wr_Sum'] <- df_chp[,c("위례열병합GT")] # 1

df_chp_final['chp_los_Sum'] <- rowSums(df_chp[,c("명품오산열병합 GT#1","명품오산열병합 ST#1")]) # 2

df_chp_final['chp_direct_Sum'] <- rep(0)
df_chp_final['chp_kogas_Sum'] <- rep(0)


# Check column names
colnames(df_chp_final) # 28
colnames(chp) # 26

# Change old chp date column with ymd form 
chp$date <- seq(lubridate::ymd("2001-01-01"), lubridate::ymd("2018-02-28"), by = "days")

chp_original <- chp %>% filter(date < "2016-01-01")

# LH set to 0 for old chp data
chp_original$chp_lh_Sum <- rep(0) # 아산은 0으로 설정한다

# Combine chp old and chp new

chp_final <- plyr::rbind.fill(chp_original, df_chp_final) %>% replace(is.na(.), 0)
chp_final %>% colnames

chp_final <- chp_final %>% relocate(chp_ay21_Sum, .after = chp_ay_Sum) %>% 
  relocate(chp_ay22_Sum, .after = chp_ay21_Sum) 

colnames(chp_final)

chp_final %>% dim() # 7882 28


write.csv(chp_final, "/Users/user/Dropbox/KOGAS/Final CSV File/열병합 (CHP) 호기 통합 일별 발전량.csv", 
          row.names = FALSE)





# ============================================================
# CHP 호기별 시간당 용량 16년 이후 계산 ======================
# ============================================================


pp_info <- read_xlsx("/Users/user/Dropbox/KOGAS/PP_Info_20220119.xlsx") %>%
  select(발전기명,Cat,Cap) %>% 
  filter(Cat == 'LNG_CHP') %>% select(-Cat) %>% 
  spread(key = 발전기명, value = Cap)

sum(is.na(pp_info)) # 0

pp_info %>% dim() # 77

df_chp_change <- df_chp %>% select(-일자)

names <- df_chp_change %>% colnames()

pp_info <- pp_info[,names]

pp_info %>% colnames == names # ALL TRUE 순서 알맞는다

df_chp_final <- data.frame(df_chp['일자'], df_chp_change)

dim(df_chp_final) # 2373 78


DF <- rep(0)
SQ <- rep(0)

for (i in 2:78) {
  DF[i-1] <- df_chp_final[which(df_chp_final[,i]!= 0)[1],1]
  SQ[i-1] <- which(df_chp_final[,i]!= 0)[1]
}


SQ <- SQ %>% replace(is.na(.), obs)
SQ

names %>% length()
DF %>% length()

data.frame(names,DF,SQ) # 63번째 관측치 위례열병합부터 들어옴


# ======================================
# 용량 기초 구조 파일 ==================
# ======================================

chp_capa <- df_chp_change %>% replace(!0, 0) # 용량 기초 구조 모든 관측치 0
capa_seq <- pp_info[1,] %>% as.numeric()
my_seq <- SQ

data <- data.frame(1:77, my_seq, capa_seq)
data %>% head()

# =======================================================================
# =======================================================================

for (i in 63:77) {
  
  chp_capa[as.numeric(data[i,][2]), as.numeric(data[i,][1])] <- 
    df_chp_change[as.numeric(data[i,][2]), as.numeric(data[i,][1])]/24
  
  for (j in 0:180){
    
    chp_capa[as.numeric(data[i,][2])+j+1, as.numeric(data[i,][1])] <- min(max(
      chp_capa[as.numeric(data[i,][2])+j, as.numeric(data[i,][1])], 
      df_chp_change[as.numeric(data[i,][2])+j+1, as.numeric(data[i,][1])]/24), as.numeric(data[i,][3]))
    
  }
  
  for (k in 181:obs){ # 2022-05-31 까지
    
    chp_capa[as.numeric(data[i,][2])+k+1, as.numeric(data[i,][1])] <-
      as.numeric(data[i,][3])
    
  }
}

# ======================================

for (i in 1:62){ # 나머지 시작일부터 채우기
  
  chp_capa[,i] <- data[i,][3]
  
}

df_chp_change %>% dim() # 2404 77
chp_capa %>% dim() # 4562 77

# ====================================
# Reverse 180일 동안 없으면 0 

chp_reverse <- df_chp_final[nrow(df_chp_final):1,]
chp_reverse %>% dim()
DF_reverse <- rep(0)
SQ_reverse <- rep(0)

for (i in 2:78) { 
  DF_reverse[i-1] <- chp_reverse[which(chp_reverse[,i]!= 0)[1],1] 
  SQ_reverse[i-1] <- which(chp_reverse[,i]!= 0)[1]
}

SQ_reverse <- SQ_reverse %>% replace(is.na(.), obs)

df_reverse <- data.frame(names,DF_reverse,SQ_reverse)
df_reverse %>% arrange(-SQ_reverse)



#######################################
# 수동으로 확인 필요 ##################
#######################################

# 180 이상 표시된 날짜 이후 0으로 용량 조정 ( 발전소 닫은것으로 판단 )
# 2022 07 31 기준 서울#4 & 서울5

df_reverse %>% dim() # 77 3  

for (i in 1:77) {
  
  if (df_reverse[i,3] > 240) { # 안양 발전소 문 닫은 사실 X
    
    chp_capa[as.numeric(obs-df_reverse[i,3]+2):obs,i] <- 0 
    
  }
}

chp_capa_final <- data.frame(df_chp['일자'],chp_capa[1:obs,])

colnames(chp_capa_final) <- gsub("\\.","#",colnames(chp_capa_final))

# 서울#4 발전소 이미 닫음 (첫 관측치 0인거 제거)

chp_capa_final["서울#4"] <- rep(0)

# 합계 칼럼 생성

chp_capa_final["합계"] <- rep(0)

dim(chp_capa_final)

for (i in 1:obs) {
  
  chp_capa_final[i,79] <- sum(chp_capa_final[i,2:78])
  
}

chp_capa_final %>% tail(3)
chp_capa_final %>% head(3)

# ============================================================

write.csv(chp_capa_final,
          "/Users/user/Dropbox/KOGAS/Final CSV File/열병합 (CHP) 호기별 시간당 용량 16년 이후.csv", 
          row.names = FALSE)



# ============================================================
# CHP 호기 통합 일별 용량 ====================================
# ============================================================

colnames(chp_capa_final)
chp_capa_final <- chp_capa_final %>% 
  rename("대구그린파워 GT"="대구그린파워#GT") %>%
  rename("명품오산열병합 GT#1"="명품오산열병합#GT#1") %>% 
  rename("명품오산열병합 ST#1"="명품오산열병합#ST#1") %>% 
  rename("안양열병합2-1CC GT"="안양열병합2#1CC#GT") %>% 
  rename("안양열병합2-1CC ST"="안양열병합2#1CC#ST") %>% 
  rename("안양열병합2-2CC GT"="안양열병합2#2CC#GT") %>% 
  rename("안양열병합2-2CC ST"="안양열병합2#2CC#ST") 


df_capa_final <- as.data.frame(date) # 새로운 Data frame 생성

df_capa_final['chp_su_Capa'] <- rowSums(chp_capa_final[,c("서울#4","서울#5")]) # 2

df_capa_final['chp_bd_Capa'] <- rowSums(chp_capa_final[,c("분당GT#1","분당GT#2","분당GT#3","분당GT#4","분당GT#5","분당GT#6","분당GT#7","분당GT#8","분당ST#1","분당ST#2")]) # 10

df_capa_final['chp_is_Capa'] <- rowSums(chp_capa_final[,c("일산GT#1","일산GT#2","일산GT#3","일산GT#4","일산GT#5","일산GT#6","일산ST#1","일산ST#2")]) # 8

df_capa_final['chp_ay_Capa'] <- rowSums(chp_capa_final[,c("안양GT#1","안양GT#2","안양GT#3","안양GT#4","안양ST#1")]) # 5

df_capa_final['chp_ay21_Capa'] <- rowSums(chp_capa_final[,c("안양열병합2-1CC GT","안양열병합2-1CC ST")]) # 2

df_capa_final['chp_ay22_Capa'] <- rowSums(chp_capa_final[,c("안양열병합2-2CC GT","안양열병합2-2CC ST")]) # 2


df_capa_final['chp_bc_Capa'] <- rowSums(chp_capa_final[,c("부천GT#1","부천GT#2","부천GT#3","부천ST#1")]) # 4

df_capa_final['chp_pk_Capa'] <- rowSums(chp_capa_final[,c("판교GT","판교ST")]) # 2

df_capa_final['chp_pj_Capa'] <- rowSums(chp_capa_final[,c("파주GT#1","파주GT#2","파주ST#1")]) # 3

df_capa_final['chp_kk_Capa'] <- rowSums(chp_capa_final[,c("광교GT","광교ST")]) # 2

df_capa_final['chp_hws_Capa'] <- rowSums(chp_capa_final[,c("화성GT#1","화성GT#2","화성ST#1")]) # 3

df_capa_final['chp_sd_Capa'] <- rowSums(chp_capa_final[,c("송도GT#1","송도GT#2","송도ST#1")]) # 3

df_capa_final['chp_yj_Capa'] <- rowSums(chp_capa_final[,c("양주GT#1","양주GT#2","양주ST#1")]) # 3

df_capa_final['chp_dgh_Capa'] <- chp_capa_final[,c("대구그린파워 GT")] # 1

df_capa_final['chp_sj_Capa'] <- rowSums(chp_capa_final[,c("세종GT#1","세종GT#2","세종ST#1")]) # 3

df_capa_final['chp_bn_Capa'] <- rowSums(chp_capa_final[,c("별내GT#1","별내GT#2","별내ST#1")]) # 3

df_capa_final['chp_lh_Capa'] <- rowSums(chp_capa_final[,c("아산GT#1","아산GT#2","아산ST#1")]) # 3

df_capa_final['chp_sw_Capa'] <- rowSums(chp_capa_final[,c("수완GT#1","수완GT#2","수완ST#1")]) # 3

df_capa_final['chp_icair_Capa'] <- rowSums(chp_capa_final[,c("인천공항GT#1","인천공항GT#2","인천공항ST#1")]) # 3

df_capa_final['chp_hn_Capa'] <- rowSums(chp_capa_final[,c("하남열병합GT#1","하남열병합ST#1")]) # 2

df_capa_final['chp_cc_Capa'] <- chp_capa_final[,c("춘천열병합GT")] # 1

df_capa_final['chp_dt_Capa'] <- rowSums(chp_capa_final[,c("동탄열병합1GT","동탄열병합1ST","동탄열병합2GT","동탄열병합2ST")]) # 4

df_capa_final['chp_jk_Capa'] <- rowSums(chp_capa_final[,c("부산정관1GT","부산정관1ST")]) # 2

df_capa_final['chp_wr_Capa'] <- chp_capa_final[,c("위례열병합GT")] # 1

df_capa_final['chp_los_Capa'] <- rowSums(chp_capa_final[,c("명품오산열병합 GT#1","명품오산열병합 ST#1")]) # 2

df_capa_final['chp_direct_Capa'] <- rep(0)
df_capa_final['chp_kogas_Capa'] <- rep(0)

colnames(df_capa_final) # 28

df_capa_final %>% head()
df_capa_final %>% tail()
df_capa_final %>% colnames()
df_capa_final_day <- data.frame(date,df_capa_final[,-1]*24)

# =========================================================
# Load 2001-01-01 ~ 2015-12-31 capacity data for chp

load("/Users/user/Dropbox/KOGAS/R Data/df_chp_capa_old.RData") 

# =========================================================


# Combine df_capa_old & df_capa_final_day

df_capa_real_final <- rbind(df_capa_old, df_capa_final_day)


# 발전량과 칼럼 순서 맞추기 

df_capa_real_final %>% colnames()
chp_final %>% colnames()

colnames(chp_final) %>% substr(1,7) == colnames(df_capa_real_final) %>% substr(1,7) # TRUE

df_capa_real_final %>% colnames()

# 호기 통합 일별 용량 2001-01-01 ~ Today 

write.csv(df_capa_real_final, 
          paste("/Users/user/Dropbox/KOGAS/Final CSV File/열병합 (CHP) 호기 통합 일별 용량.csv"), 
          row.names=FALSE)




#####################################################################################


