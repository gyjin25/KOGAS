
setwd("/Users/user/Dropbox/KOGAS/Final CSV File")
getwd()
dir()

# 원자력 용량 비교 

nc_capa_original <- read.csv("./22년 7월 가스공사/원자력 (NC) 호기별 일별 용량.csv")
nc_capa_new <- read.csv("./22년 7월 가스공사/원자력 (NC) 호기별 일별 용량.csv")

sum(nc_capa_original != nc_capa_new[1:7851,1:28]) # 0 all equal

nc_capa_new[,28] %>% tail(30) # ok

# 석탄 용량 비교 

coal_capa_original <- read.csv("./22년 6월 가스공사/석탄 (CL) 호기별 일별 용량.csv")
coal_capa_new <- read.csv("./22년 7월 가스공사/석탄 (CL) 호기별 일별 용량.csv")

sum(coal_capa_original != coal_capa_new[1:7851,1:72]) # 0 all equal

coal_capa_new[,72] %>% tail(30) # ok

# for (i in 1:71){
#   if ((sum(coal_capa_original[,i] != coal_capa_new[1:7790,i]))!=0) print(i) # dj4 / hn1 / hn2
# }
# 
# colnames(coal_capa_original) # dj4 / hn1 / hn2

# 복합 용량 비교

lng_capa_original <- read.csv("./22년 6월 가스공사/복합 (LNG) 호기 통합 일별 용량.csv")
lng_capa_new <- read.csv("./22년 7월 가스공사/복합 (LNG) 호기 통합 일별 용량.csv")

sum(lng_capa_original != lng_capa_new[1:7851,]) # 0 all equal


# 열병합 용량 비교

chp_capa_original <- read.csv("./22년 6월 가스공사/열병합 (CHP) 호기 통합 일별 용량.csv")
chp_capa_new <- read.csv("./22년 7월 가스공사/열병합 (CHP) 호기 통합 일별 용량.csv")

sum(chp_capa_original != chp_capa_new[1:7851,]) # 0


# ==============================================================================================

# 원자력 발전량 비교

nc_generation_original <- read.csv("./22년 6월 가스공사/원자력 (NC) 호기별 일별 발전량.csv")
nc_generation_new <- read.csv("./22년 7월 가스공사/원자력 (NC) 호기별 일별 발전량.csv")

sum(nc_generation_original != nc_generation_new[1:7851,1:28]) # 0

# If Wrong

# for (i in 1:27){
#   if ((sum(nc_generation_original[,i] != nc_generation_new[1:7729,i]))!=0) print(i) 
# }


# 석탄 발전량 비교

coal_generation_original <- read.csv("./22년 6월 가스공사/석탄 (CL) 호기별 일별 발전량.csv")
coal_generation_new <- read.csv("./22년 7월 가스공사/석탄 (CL) 호기별 일별 발전량.csv")
# coal_generation_sujung <- read.csv("./22년 2월 가스공사 수정본/석탄 (CL) 호기별 일별 발전량.csv")

sum(coal_generation_original != coal_generation_new[1:7851,1:72]) # 0
# sum(coal_generation_sujung != coal_generation_new[1:7729,]) # 0

# for (i in 1:71){
#   if ((sum(coal_generation_original[,i] != coal_generation_new[1:7729,i]))!=0) print(i) 
# }


ac_generation_original <- read.csv("./22년 6월 가스공사/국내탄 (AC) 호기 통합 일별 발전량.csv")
ac_generation_new <- read.csv("./22년 7월 가스공사/국내탄 (AC) 호기 통합 일별 발전량.csv")
sum(ac_generation_original != ac_generation_new[1:7851,]) # 0


bc_generation_original <- read.csv("./22년 6월 가스공사/유연탄 (BC) 호기 통합 일별 발전량.csv")
bc_generation_new <- read.csv("./22년 7월 가스공사/유연탄 (BC) 호기 통합 일별 발전량.csv")
sum(bc_generation_original != bc_generation_new[1:7851,1:15]) # 0

# 복합 발전량 비교

lng_generation_original <- read.csv("./22년 6월 가스공사/복합 (LNG) 호기 통합 일별 발전량.csv")
lng_generation_new <- read.csv("./22년 7월 가스공사/복합 (LNG) 호기 통합 일별 발전량.csv")
# lng_generation_sujung <- read.csv("./22년 2월 가스공사 수정본/복합 (LNG) 호기 통합 일별 발전량.csv")

sum(lng_generation_original != lng_generation_new[1:7851,]) # 0
# sum(lng_generation_sujung != lng_generation_new[1:7729,]) # 

# for (i in 1:35){
#   if ((sum(lng_generation_sujung[,i] != lng_generation_new[1:7729,i]))!=0) print(i) # 12 KP
# }

# colnames(lng_generation_new) # 광양이 안들어갔음 수정본에서는

# 열병합 발전량 비교

chp_generation_original <- read.csv("./22년 6월 가스공사/열병합 (CHP) 호기 통합 일별 발전량.csv")
chp_generation_new <- read.csv("./22년 7월 가스공사/열병합 (CHP) 호기 통합 일별 발전량.csv")
# chp_generation_sujung <- read.csv("./22년 2월 가스공사 수정본/열병합 (CHP) 호기 통합 일별 발전량.csv")

sum(chp_generation_original != chp_generation_new[1:7851,]) # 0
# sum(chp_generation_sujung != chp_generation_new[1:7729,]) # 0

# for (i in 1:28){
#   if ((sum(chp_generation_sujung[,i] != chp_generation_new[1:7729,i]))!=0) print(i) # 14
# }

# for (i in 1:7729){
#   for (j in 1:28){
#     if ((sum(chp_generation_sujung[i,j] != chp_generation_new[i,j]))!=0){
#       print(i) # 14
#       print(j) # 14
#     }
  #   }
  # }  
  
  # chp_generation_new %>% colnames() # 18 아산임 obs 7306 ~ 7729 
  
  # 2월 수정본에서 아산 포함 X -> 3월 new가 맞다
  
  
  # ==============================================================================================
  
  # 총발전량 비교
  
  total_generation_original <- read.csv("./22년 6월 가스공사/총발전량.csv")
  total_generation_new <- read.csv("./22년 7월 가스공사/총발전량.csv")
  # total_generation_sujung <- read.csv("/22년 2월 가스공사 수정본/총발전량.csv")
  
  x <- rep(0) %>% as.data.frame()
  
  sum(total_generation_original != total_generation_new[1:7851,]) # 0
  
  
  
  # ==============================================================================================
  
  # LAST 확인 (plot)
  
  n_obs <- dim(total_generation_new)[1]
  
  rowSums(nc_generation_new[(n_obs-365):n_obs,2:28]) %>% ts.plot()
rowSums(coal_generation_new[(n_obs-365):n_obs,2:72]) %>% ts.plot()
rowSums(lng_generation_new[(n_obs-365):n_obs,2:35]) %>% ts.plot()
rowSums(chp_generation_new[(n_obs-365):n_obs,2:28]) %>% ts.plot()

total_generation_new$total %>% ts.plot()

# for (i in 1:7790) {
#   if ((sum(total_generation_original[i,2] != total_generation_new[i,2]))!=0) {
#     print(i)
#   }
# }
# 
# x %>% drop_na()
# 
# total_generation_new %>% ts.plot()
# total_generation_original %>% ts.plot() # original 문제 

