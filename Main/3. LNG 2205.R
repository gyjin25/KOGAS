
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
         filter(발전연료2=='6.LNG(기타)') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0)) # 162

# This Month

assign(paste("df22",this_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               this_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 발전연료2, 계) %>% 
         arrange(일자) %>% 
         filter(발전연료2=='6.LNG(기타)') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0)) # 162


# Load past lng generation data

load(paste("/Users/user/Dropbox/KOGAS/R Data/df_lng_22",
           last_month,".RData",sep=""))

# combine old & new lng gen data

df_lng <- plyr::rbind.fill(df_lng, get(paste("df22",this_month,sep=""))) %>% replace(is.na(.), 0)


save(df_lng, file=paste("/Users/user/Dropbox/KOGAS/R Data/df_lng_22",
                         this_month,".RData",sep=""))

df_lng %>% dim() # 174 columns
df_lng %>% colnames()


# ============================================================
# KP 광양 

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
         select("광양GT#1","광양GT#2","광양GT#3","광양GT#4","광양ST#1","광양ST#2")) 


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
         select("광양GT#1","광양GT#2","광양GT#3","광양GT#4","광양ST#1","광양ST#2")) 


load(paste("/Users/user/Dropbox/KOGAS/R Data/df_KP_22",
           last_month,".RData",sep=""))

df_KP %>% colnames()

df_KP <- plyr::rbind.fill(df_KP, get(paste("df22",this_month,sep=""))) %>% replace(is.na(.), 0)

save(df_KP, file=paste("/Users/user/Dropbox/KOGAS/R Data/df_KP_22",
                         this_month,".RData",sep=""))


# ============================================================
# LNG 발전량

df_lng["광양GT#1"] <- df_KP$`광양GT#1`
df_lng["광양GT#2"] <- df_KP$`광양GT#2`
df_lng["광양GT#3"] <- df_KP$`광양GT#3`
df_lng["광양GT#4"] <- df_KP$`광양GT#4`
df_lng["광양ST#1"] <- df_KP$`광양ST#1`
df_lng["광양ST#2"] <- df_KP$`광양ST#2`

df_lng %>% colnames

lng <- read.table("/Users/user/Dropbox/KOGAS/Raw Data/DPG_lng.txt", sep="\t", header=T)

df_lng_new <- df_lng %>% select(-"장문복합1GT#1",-"장문복합1GT#2",-"장문복합1ST")

df_lng_new %>% colnames() # 177 (일자 포함)

date <- seq(ymd("2016-01-01"), # 2016-01-01 이후 날짜
            ymd(paste("2022-",this_month,"-",days,sep="")), by = "days")

df_lng_final <- as.data.frame(date) # 새로운 Data Frame 생성
df_lng_final %>% head()



# 호기 통합 lng dataframe 제작

df_lng_final['lng_ict_Sum'] <- rep(0)
df_lng_final['lng_dicc_Sum'] <- rowSums(df_lng_new[,c("인천GT#5","인천GT#6","인천ST#3")]) # 3

df_lng_final['lng_icc_Sum'] <- rowSums(df_lng_new[,c("인천GT#1","인천GT#2","인천GT#3","인천GT#4","인천ST#1","인천ST#2")]) # 6

df_lng_final['lng_pt_Sum'] <- rowSums(df_lng_new[,c("평택GT#1","평택GT#2","평택GT#3","평택GT#4","평택GT#5","평택GT#6","평택ST#1","평택ST#2")]) # 8

df_lng_final['lng_br_Sum'] <- rowSums(df_lng_new[,c("보령GT#1","보령GT#2","보령GT#3","보령GT#4","보령GT#5","보령GT#6","보령ST#1","보령ST#2","보령ST#3")]) # 9

df_lng_final['lng_seoic_Sum'] <- rowSums(df_lng_new[,c( "서인천GT#1","서인천GT#2","서인천GT#3","서인천GT#4","서인천GT#5","서인천GT#6","서인천GT#7","서인천GT#8","서인천ST#1","서인천ST#2","서인천ST#3","서인천ST#4","서인천ST#5","서인천ST#6","서인천ST#7","서인천ST#8")]) # 16

df_lng_final['lng_shinic_Sum'] <- rowSums(df_lng_new[,c("신인천GT#1","신인천GT#2","신인천GT#3","신인천GT#4","신인천GT#5","신인천GT#6","신인천GT#7","신인천GT#8","신인천ST#1","신인천ST#2","신인천ST#3","신인천ST#4")]) # 12

df_lng_final['lng_us_Sum'] <- rowSums(df_lng_new[,c( "울산GT#1","울산GT#2","울산GT#3","울산GT#4","울산GT#5","울산GT#6","울산GT#7","울산GT#8","울산ST#1","울산ST#2","울산ST#3","울산ST#4")]) # 12

df_lng_final['lng_bs_Sum'] <- rowSums(df_lng_new[,c("부산GT#1","부산GT#2","부산GT#3","부산GT#4","부산GT#5","부산GT#6","부산GT#7","부산GT#8","부산ST#1","부산ST#2","부산ST#3","부산ST#4")]) # 12

df_lng_final['lng_may_Sum'] <- rowSums(df_lng_new[,c( "율촌GT#1","율촌GT#2","율촌GT#3","율촌GT#4","율촌ST#1","율촌ST#2")]) # 6

df_lng_final['lng_kp_Sum'] <- rowSums(df_lng_new[,c("광양GT#1","광양GT#2","광양GT#3","광양GT#4","광양ST#1","광양ST#2")]) # 6

df_lng_final['lng_gs_Sum'] <- rowSums(df_lng_new[,c("군산GT#1","군산GT#2","군산ST#1")]) # 3

df_lng_final['lng_yw_Sum'] <- rowSums(df_lng_new[,c("영월GT#1","영월GT#2","영월GT#3","영월ST#1")]) # 4

df_lng_final['lng_os_Sum'] <- rowSums(df_lng_new[,c("오성GT#1","오성GT#2","오성GT#3","오성ST#1")]) # 4

df_lng_final['lng_ad_Sum'] <- df_lng_new[,c("안동GT#1")] # 1

df_lng_final['lng_as_Sum'] <- rowSums(df_lng_new[,c("안산GT#1","안산GT#2","안산ST#1")]) # 3

df_lng_final['lng_pch_Sum'] <- rowSums(df_lng_new[,c("포천GT#1","포천GT#2","포천GT#3","포천GT#4","포천ST#1","포천ST#2")]) # 6

df_lng_final['lng_ddc_Sum'] <- rowSums(df_lng_new[,c("동두천GT#1","동두천GT#2","동두천GT#3","동두천GT#4","동두천ST#1","동두천ST#2")]) # 6

df_lng_final['lng_hh3_Sum'] <- rowSums(df_lng_new[,c("포스코GT#7","포스코GT#8","포스코GT#9","포스코ST#3")]) # 4
df_lng_final['lng_hh4_Sum'] <- rowSums(df_lng_new[,c("포스코GT#10","포스코GT#11","포스코GT#12","포스코ST#4")]) # 4
df_lng_final['lng_hh59_Sum'] <- rowSums(df_lng_new[,c("포스코GT#13","포스코GT#14","포스코ST#5","포스코GT#15","포스코GT#16","포스코ST#6","포스코GT#17","포스코GT#18","포스코GT#19","포스코GT#4","포스코GT#5","포스코GT#6","포스코ST#2")]) # 13

df_lng_final['lng_hh_Sum'] <- rowSums(df_lng_new[,c("포스코GT#7","포스코GT#8","포스코GT#9","포스코ST#3","포스코GT#10","포스코GT#11","포스코GT#12","포스코ST#4","포스코GT#13","포스코GT#14","포스코ST#5","포스코GT#15","포스코GT#16","포스코ST#6","포스코GT#17","포스코GT#18","포스코GT#19","포스코GT#4","포스코GT#5","포스코GT#6","포스코ST#2")]) # 이건 그냥 모든 포스코 합

df_lng_final['lng_lg_Sum'] <- rowSums(df_lng_new[,c("GS당진GT#1","GS당진GT#2","GS당진GT#3","GS당진GT#4","GS당진GT#5","GS당진ST#1","GS당진ST#2")]) # 7

df_lng_final['lng_pcp_Sum'] <- rowSums(df_lng_new[,c("포천천연복합1ST","포천천연복합GT#1","포천천연복합GT#2")]) # 3

df_lng_final['lng_dj_Sum'] <- rowSums(df_lng_new[,c("GS당진복합 4GT#2","GS당진복합4GT#1","GS당진복합4ST")]) # 3

df_lng_final['lng_ynpw_Sum'] <- df_lng_new[,c("영남파워1GT")] # 1

df_lng_final['lng_pjms_Sum'] <- rowSums(df_lng_new[,c("파주문산복합1GT#1","파주문산복합1GT#2","파주문산복합1ST","파주문산복합2GT#1","파주문산복합2GT#2","파주문산복합2ST")]) # 6

df_lng_final['lng_su_Sum'] <- rowSums(df_lng_new[,c("서울복합GT#1","서울복합GT#2","서울복합ST#1","서울복합ST#2")]) # 4

df_lng_final['lng_npt_Sum'] <- rowSums(df_lng_new[,c("신평택복합GT#1","신평택복합GT#2","신평택복합ST")]) # 3

df_lng_final['lng_jeju_Sum'] <- rowSums(df_lng_new[,c("제주LNG1GT","제주LNG1ST","제주LNG2GT","제주LNG2ST" )]) # 4

df_lng_final['lng_hl_Sum'] <- rowSums(df_lng_new[,c("한림GT#1","한림GT#2","한림ST#1")]) # 3

df_lng_final['lng_hpt_Sum'] <- rowSums(df_lng_new[,c("평택#1","평택#2","평택#3","평택#4")]) # 4 (중유 -> LNG)

# ---

df_lng_final['lng_direct_Sum'] <- rep(0) 
df_lng_final['lng_kogas_Sum'] <- rep(0) 


# Check column names 
colnames(df_lng_final) # 35
df_lng_final %>% head()
colnames(lng) # 27

lng %>% tail()
# Change old lng date column with ymd form
lng$date <- seq(lubridate::ymd("2001-01-01"), lubridate::ymd("2018-02-28"), by = "days")

lng_original <- lng %>% filter(date < "2016-01-01")

# Combine lng_old & lng_new

lng_final <- plyr::rbind.fill(lng_original, df_lng_final) %>% replace(is.na(.), 0)
lng_final %>% colnames

lng_final <- lng_final %>% relocate(lng_direct_Sum, .after = lng_hl_Sum) %>% 
  relocate(lng_kogas_Sum, .after = lng_direct_Sum) %>% 
  relocate(lng_hh_Sum, .after = lng_hh59_Sum) %>% 
  relocate(lng_hpt_Sum, .after = lng_hl_Sum)

colnames(lng_final)

lng_final %>% dim() # 7882 35
lng_final %>% head()


write.csv(lng_final, "/Users/user/Dropbox/KOGAS/Final CSV File/복합 (LNG) 호기 통합 일별 발전량.csv",
          row.names = FALSE)



# ============================================================
# LNG 호기별 시간당 용량 16년 이후 계산 ======================
# ============================================================


pp_info <- read_xlsx("/Users/user/Dropbox/KOGAS/PP_Info_20220119.xlsx") %>% 
  select(발전기명,Cat,Cap) %>% 
  filter(Cat == 'LNG_CC') %>% select(-Cat) %>% 
  spread(key = 발전기명, value = Cap)

sum(is.na(pp_info)) # 0

pp_info %>% dim() # 176

df_lng_change <- df_lng %>% select(-일자,-"장문복합1GT#1",-"장문복합1GT#2",-"장문복합1ST")

names <- df_lng_change %>% colnames()

pp_info <- pp_info[,names]

pp_info %>% colnames == names # ALL TRUE 순서 알맞는다

df_lng_final <- data.frame(df_lng['일자'], df_lng_change)

dim(df_lng_final) # 2404 177


DF <- rep(0)
SQ <- rep(0)

for (i in 2:177) { 
  
  DF[i-1] <- df_lng_final[which(df_lng_final[,i]!= 0)[1],1]
  SQ[i-1] <- which(df_lng_final[,i]!= 0)[1]
  
}

SQ

SQ <- SQ %>% replace(is.na(.), obs) # 22년 7월까지

data.frame(names,DF,SQ) # 140번째 관측치 포천천연복합부터 새로 들어옴


# ======================================
# 용량 기초 구조 파일 ==================
# ======================================

lng_capa <- df_lng_change %>% replace(!0, 0) # 용량 기초 구조 모든 관측치 0
capa_seq <- pp_info[1,] %>% as.numeric()
my_seq <- SQ

data <- data.frame(1:176, my_seq, capa_seq)
data %>% head()
data
# =======================================================================
# =======================================================================

for (i in 140:170) {
  
  lng_capa[as.numeric(data[i,][2]), as.numeric(data[i,][1])] <- 
    df_lng_change[as.numeric(data[i,][2]), as.numeric(data[i,][1])]/24
  
  for (j in 0:180){
    
    
    lng_capa[as.numeric(data[i,][2])+j+1, as.numeric(data[i,][1])] <- min(max(
      lng_capa[as.numeric(data[i,][2])+j, as.numeric(data[i,][1])], 
      df_lng_change[as.numeric(data[i,][2])+j+1, as.numeric(data[i,][1])]/24), as.numeric(data[i,][3]))
    
  }
  
  for (k in 181:obs){ # 2022-05-31 까지
    
    lng_capa[as.numeric(data[i,][2])+k+1, as.numeric(data[i,][1])] <-
      as.numeric(data[i,][3])
    
  }
}

# ======================================

for (i in 1:139){ # 나머지 시작일부터 채우기
  
  lng_capa[,i] <- data[i,][3]

}

for (i in 171:176) { # 광양 시작일부터 채우기
  
  lng_capa[,i] <- data[i,][3]

}

df_lng_change %>% dim() # 2404 176
lng_capa %>% dim() # 4096 176

# ====================================
# Reverse 180일 동안 없으면 0 

lng_reverse <- df_lng_final[nrow(df_lng_final):1,]
lng_reverse %>% dim()
DF_reverse <- rep(0)
SQ_reverse <- rep(0)

for (i in 2:177) { 
  DF_reverse[i-1] <- lng_reverse[which(lng_reverse[,i]!= 0)[1],1] 
  SQ_reverse[i-1] <- which(lng_reverse[,i]!= 0)[1]
}

SQ_reverse <- SQ_reverse %>% replace(is.na(.), obs) # NA 값 obs 값으로 변경

df_reverse <- data.frame(names,DF_reverse,SQ_reverse)
df_reverse %>% arrange(-SQ_reverse)


#######################################
# 수동으로 확인 필요 ##################
#######################################

# 180 이상 표시된 날짜 이후 0으로 용량 조정 ( 발전소 닫은것으로 판단 )

df_reverse %>% dim() # 176 3 

for (i in 1:176) {
  
  if (df_reverse[i,3] > 180) {
    
    lng_capa[as.numeric(obs-df_reverse[i,3]+2):obs,i] <- 0 
    
  }
}

capa_final <- data.frame(df_lng['일자'],lng_capa[1:obs,])

capa_final %>% colnames()

colnames(capa_final) <- gsub("\\.","#",colnames(capa_final))

# 포스코 4개 발전소 이미 닫음 (첫 관측치 0인거 제거)

capa_final["포스코GT#4"] <- rep(0)
capa_final["포스코GT#5"] <- rep(0)
capa_final["포스코GT#6"] <- rep(0)
capa_final["포스코ST#2"] <- rep(0)

# 합계 칼럼 생성

capa_final["합계"] <- rep(0)

dim(capa_final)

for (i in 1:obs) {
  
  capa_final[i,178] <- sum(capa_final[i,2:177])
  
}

capa_final %>% tail(3)
capa_final %>% head(3)
capa_final$합계 %>% plot()
# ============================================================

write.csv(capa_final,
          "/Users/user/Dropbox/KOGAS/Final CSV File/복합 (LNG) 호기별 시간당 용량 16년 이후.csv", 
          row.names = FALSE)




# ============================================================
# LNG 호기 통합 일별 용량 ====================================
# ============================================================

df_capa_final <- as.data.frame(date) # 새로운 Data frame 생성

df_capa_final['lng_ict_Capa'] <- rep(0)
df_capa_final['lng_dicc_Capa'] <- rowSums(capa_final[,c("인천GT#5","인천GT#6","인천ST#3")]) # 3

df_capa_final['lng_icc_Capa'] <- rowSums(capa_final[,c("인천GT#1","인천GT#2","인천GT#3","인천GT#4","인천ST#1","인천ST#2")]) # 6

df_capa_final['lng_pt_Capa'] <- rowSums(capa_final[,c("평택GT#1","평택GT#2","평택GT#3","평택GT#4","평택GT#5","평택GT#6","평택ST#1","평택ST#2")]) # 8

df_capa_final['lng_br_Capa'] <- rowSums(capa_final[,c("보령GT#1","보령GT#2","보령GT#3","보령GT#4","보령GT#5","보령GT#6","보령ST#1","보령ST#2","보령ST#3")]) # 9

df_capa_final['lng_seoic_Capa'] <- rowSums(capa_final[,c( "서인천GT#1","서인천GT#2","서인천GT#3","서인천GT#4","서인천GT#5","서인천GT#6","서인천GT#7","서인천GT#8","서인천ST#1","서인천ST#2","서인천ST#3","서인천ST#4","서인천ST#5","서인천ST#6","서인천ST#7","서인천ST#8")]) # 16

df_capa_final['lng_shinic_Capa'] <- rowSums(capa_final[,c("신인천GT#1","신인천GT#2","신인천GT#3","신인천GT#4","신인천GT#5","신인천GT#6","신인천GT#7","신인천GT#8","신인천ST#1","신인천ST#2","신인천ST#3","신인천ST#4")]) # 12

df_capa_final['lng_us_Capa'] <- rowSums(capa_final[,c( "울산GT#1","울산GT#2","울산GT#3","울산GT#4","울산GT#5","울산GT#6","울산GT#7","울산GT#8","울산ST#1","울산ST#2","울산ST#3","울산ST#4")]) # 12

df_capa_final['lng_bs_Capa'] <- rowSums(capa_final[,c("부산GT#1","부산GT#2","부산GT#3","부산GT#4","부산GT#5","부산GT#6","부산GT#7","부산GT#8","부산ST#1","부산ST#2","부산ST#3","부산ST#4")]) # 12

df_capa_final['lng_may_Capa'] <- rowSums(capa_final[,c("율촌GT#1","율촌GT#2","율촌GT#3","율촌GT#4","율촌ST#1","율촌ST#2")]) # 6

df_capa_final['lng_kp_Capa'] <- rowSums(capa_final[,c("광양GT#1","광양GT#2","광양GT#3","광양GT#4","광양ST#1","광양ST#2")]) # 6

df_capa_final['lng_gs_Capa'] <- rowSums(capa_final[,c("군산GT#1","군산GT#2","군산ST#1")]) # 3

df_capa_final['lng_yw_Capa'] <- rowSums(capa_final[,c("영월GT#1","영월GT#2","영월GT#3","영월ST#1")]) # 4

df_capa_final['lng_os_Capa'] <- rowSums(capa_final[,c("오성GT#1","오성GT#2","오성GT#3","오성ST#1")]) # 4

df_capa_final['lng_ad_Capa'] <- capa_final[,c("안동GT#1")] # 1

df_capa_final['lng_as_Capa'] <- rowSums(capa_final[,c("안산GT#1","안산GT#2","안산ST#1")]) # 3

df_capa_final['lng_pch_Capa'] <- rowSums(capa_final[,c("포천GT#1","포천GT#2","포천GT#3","포천GT#4","포천ST#1","포천ST#2")]) # 6

df_capa_final['lng_ddc_Capa'] <- rowSums(capa_final[,c("동두천GT#1","동두천GT#2","동두천GT#3","동두천GT#4","동두천ST#1","동두천ST#2")]) # 6

df_capa_final['lng_hh3_Capa'] <- rowSums(capa_final[,c("포스코GT#7","포스코GT#8","포스코GT#9","포스코ST#3")]) # 4
df_capa_final['lng_hh4_Capa'] <- rowSums(capa_final[,c("포스코GT#10","포스코GT#11","포스코GT#12","포스코ST#4")]) # 4
df_capa_final['lng_hh59_Capa'] <- rowSums(capa_final[,c("포스코GT#13","포스코GT#14","포스코ST#5","포스코GT#15","포스코GT#16","포스코ST#6","포스코GT#17","포스코GT#18","포스코GT#19")]) # 9

df_capa_final['lng_hh_Capa'] <- rowSums(capa_final[,c("포스코GT#7","포스코GT#8","포스코GT#9","포스코ST#3","포스코GT#10","포스코GT#11","포스코GT#12","포스코ST#4","포스코GT#13","포스코GT#14","포스코ST#5","포스코GT#15","포스코GT#16","포스코ST#6","포스코GT#17","포스코GT#18","포스코GT#19","포스코GT#4","포스코GT#5","포스코GT#6","포스코ST#2")]) # 이건 그냥 모든 포스코 합 21

df_capa_final['lng_lg_Capa'] <- rowSums(capa_final[,c("GS당진GT#1","GS당진GT#2","GS당진GT#3","GS당진GT#4","GS당진GT#5","GS당진ST#1","GS당진ST#2")]) # 7

df_capa_final['lng_pcp_Capa'] <- rowSums(capa_final[,c("포천천연복합1ST","포천천연복합GT#1","포천천연복합GT#2")]) # 3

df_capa_final['lng_dj_Capa'] <- rowSums(capa_final[,c("GS당진복합#4GT#2","GS당진복합4GT#1","GS당진복합4ST")]) # 3

df_capa_final['lng_ynpw_Capa'] <- capa_final[,c("영남파워1GT")] # 1

df_capa_final['lng_pjms_Capa'] <- rowSums(capa_final[,c("파주문산복합1GT#1","파주문산복합1GT#2","파주문산복합1ST","파주문산복합2GT#1","파주문산복합2GT#2","파주문산복합2ST")]) # 6

df_capa_final['lng_su_Capa'] <- rowSums(capa_final[,c("서울복합GT#1","서울복합GT#2","서울복합ST#1","서울복합ST#2")]) # 4

df_capa_final['lng_npt_Capa'] <- rowSums(capa_final[,c("신평택복합GT#1","신평택복합GT#2","신평택복합ST")]) # 3

df_capa_final['lng_jeju_Capa'] <- rowSums(capa_final[,c("제주LNG1GT","제주LNG1ST","제주LNG2GT","제주LNG2ST" )]) # 4

df_capa_final['lng_hl_Capa'] <- rowSums(capa_final[,c("한림GT#1","한림GT#2","한림ST#1")]) # 3

df_capa_final['lng_hpt_Capa'] <- rowSums(capa_final[,c("평택#1","평택#2","평택#3","평택#4")]) # 4 (증유 -> LNG)

df_capa_final['lng_direct_Capa'] <- rep(0) 
df_capa_final['lng_kogas_Capa'] <- rep(0) 

colnames(df_capa_final) # 35

df_capa_final %>% head()
df_capa_final %>% tail()
df_capa_final %>% colnames()
df_capa_final_day <- data.frame(date,df_capa_final[,-1]*24)
df_capa_final_day


# =========================================================
# Load 2001-01-01 ~ 2015-12-31 capacity data for lng

load("/Users/user/Dropbox/KOGAS/R Data/df_lng_capa_old.RData") 

# =========================================================


# Combine df_capa_old & df_capa_final_day

df_capa_real_final <- rbind(df_capa_old, df_capa_final_day)


# 발전량과 칼럼 순서 맞추기 

lng_final %>% colnames()
df_capa_real_final %>% colnames()

df_capa_real_final <- df_capa_real_final %>% 
  relocate(lng_hh3_Capa, .after = lng_pjms_Capa) %>% 
  relocate(lng_hh4_Capa, .after = lng_hh3_Capa) %>% 
  relocate(lng_hh59_Capa, .after = lng_hh4_Capa) %>% 
  relocate(lng_hh_Capa, .after = lng_hh59_Capa)


colnames(lng_final) %>% substr(1,7) == colnames(df_capa_real_final) %>% substr(1,7) # TRUE

df_capa_real_final %>% colnames()

# 호기 통합 일별 용량 2001-01-01 ~ Today 

write.csv(df_capa_real_final, 
          paste("/Users/user/Dropbox/KOGAS/Final CSV File/복합 (LNG) 호기 통합 일별 용량.csv"), 
          row.names=FALSE)




