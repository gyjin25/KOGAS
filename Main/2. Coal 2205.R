
## 당진#4 & 보령#4 수동으로 확인 필요 ##

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
# 2022 - 05 ==================================================

this_month <- "07"
last_month <- "06"
days <- "31"

total_obs <- seq(ymd("2001-01-01"), 
                 ymd(paste("2022-",this_month,"-",days,sep="")), by = "days") %>% length()

# Last Month (태안 IGCC 제외 확인)

assign(paste("df22",last_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               last_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 발전연료2, 계) %>% 
         arrange(일자) %>% 
         filter(발전연료2=='3.석탄'|발전연료2=='2.국내탄') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0) %>% 
         select(-c("태안 IGCC GT","태안 IGCC ST"))) # 59

# This Month

assign(paste("df22",this_month,sep=""), 
       read_excel_allsheets(
         paste("/Users/user/Dropbox/KOGAS/Raw Data/2022",
               this_month,".xlsx", sep=""))$Sheet1 %>% 
         janitor::row_to_names(1) %>% 
         select(일자, 발전기명, 발전연료2, 계) %>% 
         arrange(일자) %>% 
         filter(발전연료2=='3.석탄'|발전연료2=='2.국내탄') %>% 
         select(-발전연료2) %>% 
         spread(key = 발전기명, value = 계) %>%
         mutate_all(function(x) as.numeric(as.character(x))) %>% 
         replace(is.na(.), 0) %>% 
         select(-c("태안 IGCC GT","태안 IGCC ST"))) # 59

df2206 %>% colnames()
df2207 %>% colnames()

load(paste("/Users/user/Dropbox/KOGAS/R Data/df_coal_22",
           last_month,".RData",sep=""))

df_coal %>% colnames()
df2206 %>% colnames()

df_coal <- plyr::rbind.fill(df_coal, get(paste("df22",this_month,sep=""))) %>% replace(is.na(.), 0)

save(df_coal, file=paste("/Users/user/Dropbox/KOGAS/R Data/df_coal_22",
                       this_month,".RData",sep=""))

# ============================================================
# Matching colnames

df_coal %>% colnames()

colnames(df_coal) <- c("date","bc_dj1_Sum","bc_dj2_Sum","bc_dj3_Sum","bc_dj4_Sum","bc_dj5_Sum","bc_dj6_Sum","bc_dj7_Sum","bc_dj8_Sum","bc_dj9_Sum","ac_dh1_Sum","ac_dh2_Sum","bc_br1_Sum","bc_br2_Sum","bc_br3_Sum","bc_br4_Sum","bc_br5_Sum","bc_br6_Sum","bc_br7_Sum","bc_br8_Sum","bc_scp1_Sum","bc_scp2_Sum","bc_scp3_Sum","bc_scp4_Sum","bc_scp5_Sum","bc_scp6_Sum","ac_sc1_Sum","ac_sc2_Sum","bc_ys2_Sum","ac_yd1_Sum","ac_yd2_Sum","bc_yh1_Sum","bc_yh2_Sum","bc_yh3_Sum","bc_yh4_Sum","bc_yh5_Sum","bc_yh6_Sum","bc_tn1_Sum","bc_tn2_Sum","bc_tn3_Sum","bc_tn4_Sum","bc_tn5_Sum","bc_tn6_Sum","bc_tn7_Sum","bc_tn8_Sum","bc_hd1_Sum","bc_hd2_Sum","bc_hd3_Sum","bc_hd4_Sum","bc_hd5_Sum","bc_hd6_Sum","bc_hd7_Sum","bc_hd8_Sum","bc_hn1_Sum","bc_hn2_Sum","bc_bp1_Sum","bc_nbr1_Sum","bc_ys1_Sum","bc_dj10_Sum","bc_tn9_Sum","bc_sc1_Sum","bc_sc2_Sum","bc_nbr2_Sum","bc_tn10_Sum","bc_bp2_Sum","bc_gsh1_Sum","bc_nsc1_Sum","bc_gsh2_Sum","bc_ka1_Sum")

df_coal %>% dim() # 2404 69
df_coal %>% colnames()

# Loading Previous Data

ac <- read.table("/Users/user/Dropbox/KOGAS/Raw Data/DPG_ac.txt", sep="\t", header=T)
bc <- read.table("/Users/user/Dropbox/KOGAS/Raw Data/DPG_bc.txt", sep="\t", header=T)

coal <- data.frame(ac,bc)[1:5478,] # 2001-01-01 ~ 2015-12-31

Date <- seq(lubridate::ymd("2001-01-01"), 
            lubridate::ymd(paste("2022-",this_month,"-",days,sep="")), by = "days") %>% 
  as.character() %>% str_replace_all("-","") %>%  as.data.frame() %>% set_names("date") 

coal_final <- data.frame(Date, 
                         plyr::rbind.fill(coal %>% select(-date,-date.1), 
                                          df_coal %>% select(-date))) %>% 
  replace(is.na(.),0)


coal_final %>% dim() # 7882 / 72

# 일별 발전량 (Daily Generation of COAL)

write.csv(coal_final, 
          paste("/Users/user/Dropbox/KOGAS/Final CSV File/석탄 (CL) 호기별 일별 발전량.csv"), 
          row.names=FALSE)

# ============================================================
# AC =========================================================

df_ac <- coal_final %>% select("date", contains("ac"))

ac_final <- df_ac %>% select("date")
ac_final['ac_gs_Sum'] <- df_ac["ac_gs1_Sum"]
ac_final['ac_dh_Sum'] <- rowSums(df_ac[,c("ac_dh1_Sum","ac_dh2_Sum")])
ac_final['ac_sc_Sum'] <- rowSums(df_ac[,c("ac_sc1_Sum","ac_sc2_Sum")])
ac_final['ac_yd_Sum'] <- rowSums(df_ac[,c("ac_yd1_Sum","ac_yd2_Sum")])
ac_final['ac_yw_Sum'] <- rowSums(df_ac[,c("ac_yw1_Sum","ac_yw2_Sum")])

write.csv(ac_final, 
          paste("/Users/user/Dropbox/KOGAS/Final CSV File/국내탄 (AC) 호기 통합 일별 발전량.csv"), 
          row.names=FALSE)


# ============================================================
# BC =========================================================

df_bc <- coal_final %>% select("date", contains("bc"))

df_bc %>% colnames() # 63

bc_final <- df_bc %>% select("date")
bc_final['bc_dj_Sum'] <- rowSums(df_bc[,c("bc_dj1_Sum","bc_dj2_Sum","bc_dj3_Sum",
                                           "bc_dj4_Sum","bc_dj5_Sum","bc_dj6_Sum",
                                           "bc_dj7_Sum","bc_dj8_Sum","bc_dj9_Sum",
                                           "bc_dj10_Sum")]) #당진 10
bc_final['bc_br_Sum'] <- rowSums(df_bc[,c("bc_br1_Sum","bc_br2_Sum","bc_br3_Sum",
                                           "bc_br4_Sum", "bc_br5_Sum","bc_br6_Sum",
                                           "bc_br7_Sum","bc_br8_Sum")]) #보령 8
bc_final['bc_bp_Sum'] <- rowSums(df_bc[,c("bc_bp1_Sum","bc_bp2_Sum")]) #북평 2
bc_final['bc_sc_Sum'] <- rowSums(df_bc[,c("bc_sc1_Sum","bc_sc2_Sum")]) #삼척그린 2
bc_final['bc_scp_Sum'] <- rowSums(df_bc[,c("bc_scp1_Sum","bc_scp2_Sum","bc_scp3_Sum",
                                            "bc_scp4_Sum","bc_scp5_Sum","bc_scp6_Sum")]) #삼천포 6
bc_final['bc_nbr_Sum'] <- rowSums(df_bc[,c("bc_nbr1_Sum","bc_nbr2_Sum")]) #신보령 2
bc_final['bc_ys_Sum'] <- rowSums(df_bc[,c("bc_ys1_Sum","bc_ys2_Sum")]) #여수 2
bc_final['bc_yh_Sum'] <- rowSums(df_bc[,c("bc_yh1_Sum","bc_yh2_Sum","bc_yh3_Sum",
                                           "bc_yh4_Sum","bc_yh5_Sum","bc_yh6_Sum")]) #영흥6
bc_final['bc_tn_Sum'] <- rowSums(df_bc[,c("bc_tn1_Sum","bc_tn2_Sum","bc_tn3_Sum",
                                           "bc_tn4_Sum","bc_tn5_Sum","bc_tn6_Sum",
                                           "bc_tn7_Sum","bc_tn8_Sum","bc_tn9_Sum",
                                           "bc_tn10_Sum")]) #태안 10
bc_final['bc_hd_Sum'] <- rowSums(df_bc[,c("bc_hd1_Sum","bc_hd2_Sum","bc_hd3_Sum",
                                           "bc_hd4_Sum","bc_hd5_Sum","bc_hd6_Sum",
                                           "bc_hd7_Sum","bc_hd8_Sum")]) #하동 8
bc_final['bc_hn_Sum'] <- rowSums(df_bc[,c("bc_hn1_Sum","bc_hn2_Sum")]) #호남 2
bc_final['bc_gsh_Sum'] <- rowSums(df_bc[,c("bc_gsh1_Sum","bc_gsh2_Sum")]) #고성하이 2
bc_final['bc_nsc_Sum'] <- df_bc[,c("bc_nsc1_Sum")] #신서천화력 1
bc_final['bc_ka_Sum'] <- df_bc[,c("bc_ka1_Sum")] # 강릉인안 1



write.csv(bc_final, 
          paste("/Users/user/Dropbox/KOGAS/Final CSV File/유연탄 (BC) 호기 통합 일별 발전량.csv"), 
          row.names=FALSE)




# ======================================
# 용량 자료 구축 =======================
# ====================================== 


DF <- rep(0) # Date of Start
SQ <- rep(0) # Days from 2001-01-01 to Start Date 


for (i in 2:72) { 
  # which(nc[,i]!= 0) %>% head(1) %>% print() # 0이 아닌 발전량이 나오는 날짜
  DF[i-1] <- coal_final[which(coal_final[,i]!= 0)[1],1] 
  SQ[i-1] <- which(coal_final[,i]!= 0) %>% head(1)
}

names <- coal_final %>% select(-'date') %>% colnames()
names

data.frame(names,DF,SQ) # 70


# ======================================
# 용량 기초 구조 파일 ==================
# ====================================== 

pp_info_ac <- read_xlsx("/Users/user/Dropbox/KOGAS/PP_Info_20220119.xlsx") %>%  
  select(Code,Sub_Cat,Cap) %>% 
  filter(Sub_Cat == 'AC') %>% select(-Sub_Cat) %>% 
  spread(key = Code, value = Cap) %>% 
  as.data.frame()

pp_info_bc

pp_info_bc <- read_xlsx("/Users/user/Dropbox/KOGAS/PP_Info_20220119.xlsx") %>%  
  select(Code,Sub_Cat,Cap) %>% 
  filter(Sub_Cat == 'BC') %>% select(-Sub_Cat) %>% 
  spread(key = Code, value = Cap) %>% 
  as.data.frame()


colnames(pp_info_bc) <- paste("bc_", colnames(pp_info_bc), "_Sum", sep = "")
colnames(pp_info_ac) <- paste("ac_", colnames(pp_info_ac), "_Sum", sep = "")

pp_info <- data.frame(pp_info_ac, pp_info_bc)
pp_info <- pp_info[,names] # Re-ordering colnames
pp_info 

# ===========================================================

coal_capa <- coal_final %>% select(-"date") %>% replace(!0, 0)
coal_capa %>% colnames() == pp_info %>% colnames() # ALL TRUE = ordering correct
capa_seq <- pp_info[1,] %>% as.numeric()

my_seq <- SQ
my_seq

data <- data.frame(1:71, my_seq, capa_seq)
data # 38 ~ 71

df_coal_change <- coal_final %>% select(-"date")
df_coal_change %>% colnames() # 71
df_coal_change

# =======================================================================
# =======================================================================


for (i in 38:71) {
  
  coal_capa[as.numeric(data[i,][2]), as.numeric(data[i,][1])] <- 
    df_coal_change[as.numeric(data[i,][2]), as.numeric(data[i,][1])]/24
  
  for (j in 0:180){
    
    coal_capa[as.numeric(data[i,][2])+j+1, as.numeric(data[i,][1])] <- min(max(
      coal_capa[as.numeric(data[i,][2])+j, as.numeric(data[i,][1])], 
      df_coal_change[as.numeric(data[i,][2])+j+1, as.numeric(data[i,][1])]/24), 
      as.numeric(data[i,][3]))
    
  }
  
  for (k in 181:total_obs) {
    
    coal_capa[as.numeric(data[i,][2])+k+1, as.numeric(data[i,][1])] <-
      as.numeric(data[i,][3])
    
  }
}

for (i in 1:37) {
  
  coal_capa[,i] <- data[i,][3]
  
}

coal_capa %>% dim() # 15705 71
df_coal_change %>% dim() # 7882 71


# ====================================
# Reverse 180일 동안 없으면 0 

coal_reverse <- coal_final[nrow(coal_final):1,]
coal_reverse %>% head()

DF_reverse <- rep(0)
SQ_reverse <- rep(0)

for (i in 2:72) { 
  DF_reverse[i-1] <- coal_reverse[which(coal_reverse[,i]!= 0)[1],1] 
  SQ_reverse[i-1] <- which(coal_reverse[,i]!= 0) %>% head(1)
}

df_reverse <- data.frame(names,DF_reverse,SQ_reverse)
df_reverse %>% arrange(-SQ_reverse) # 71


#######################################
# 수동으로 확인 필요 ##################
#######################################

for (i in 1:71) {
  
  if (df_reverse[i,3] > 320) { # 당진#1 & 보령#4 아직 폐쇄 X 
    
    coal_capa[as.numeric(total_obs-df_reverse[i,3]+2):total_obs,i] <- 0 
    
  }
}

capa_final <- data.frame(coal_final['date'], coal_capa[1:total_obs,]*24)

# ================================================================================
# 호남#1 & 호남#2 2022-01-01 기준으로 폐쇄 

which(capa_final$date == '20220101') # 7671 번째 열이 2022-01-01

capa_final[7671,] 

capa_final$bc_hn1_Sum[7671:total_obs] <- 0 # 22-05-31 
capa_final$bc_hn2_Sum[7671:total_obs] <- 0 # 22-05-31 


write.csv(capa_final,
          "/Users/user/Dropbox/KOGAS/Final CSV File/석탄 (CL) 호기별 일별 용량.csv", 
          row.names = FALSE)





