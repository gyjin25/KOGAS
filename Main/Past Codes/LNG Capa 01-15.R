
setwd("/Users/user/Dropbox/KOGAS/Main/Past Codes")
getwd()
dir()

library(readxl)
library(tidyverse)
library(lubridate)

# ======================================
# 2001년 1월 1일 ~ 2015년 12월 31일 용량 
# ======================================

old_capa <- read_xlsx("/Users/user/Dropbox/KOGAS/PP_Info_20220119.xlsx") %>% 
  select(발전기명,Cat,S_date,E_date,Cap) %>% 
  filter(Cat == 'LNG_CC') %>% mutate(Cap = Cap*24) %>% arrange(S_date) %>% arrange(E_date)

old_capa$S_date <- as.Date(old_capa$S_date ,format = "%Y-%m-%d")
old_capa$E_date <- as.Date(old_capa$E_date ,format = "%Y-%m-%d")
old_capa['Int'] <- interval(ymd(old_capa$S_date), ymd(old_capa$E_date))

old_capa_int <- aggregate(old_capa$Cap, list(old_capa$Int), sum) %>% 
  set_names(c("INT","CAP")) %>% arrange(INT) 

# Date Sequence Generation
old_date <- seq(lubridate::ymd("2001-01-01"), lubridate::ymd("2015-12-31"), by = "days")

# Making Data Frame of old dates (2001 ~ 2016)
df_capa_old <- as.data.frame(old_date)
df_capa_old %>% head()
dim(df_capa_old) # 5478

# (1) ict ======================================================================
# ==============================================================================

df_capa_old['lng_ict_Capa'] <- rep(0)

# (2) dicc =====================================================================
# "인천GT#5","인천GT#6","인천ST#3" =============================================

df_capa_old['lng_dicc_Capa'] <- rep(0)

Dicc_Capa <- old_capa %>% filter(grepl('인천',발전기명)) %>% filter(!grepl('신',발전기명)) %>% 
  filter(!grepl('서',발전기명)) %>% filter(S_date == "2013-01-01")
Dicc_Capa

Dicc_Capa_int <- aggregate(Dicc_Capa$Cap, list(Dicc_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
Dicc_Capa_int # 1

df_capa_old$lng_dicc_Capa[which(grepl(Dicc_Capa_int[1,3], df_capa_old$old_date)):5478] <- Dicc_Capa_int[1,4]


# (3) icc ======================================================================
# ==============================================================================

df_capa_old['lng_icc_Capa'] <- rep(0)

Icc_Capa <- old_capa %>% filter(grepl('인천',발전기명)) %>% filter(!grepl('신',발전기명)) %>% 
  filter(!grepl('서',발전기명)) %>% filter(S_date != "2013-01-01")
Icc_Capa

Icc_Capa_int <- aggregate(Icc_Capa$Cap, list(Icc_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
Icc_Capa_int  # 2

# 각 날짜별 용량 설정
df_capa_old$lng_icc_Capa[which(grepl(Icc_Capa_int[1,3], df_capa_old$old_date)):
                           (which(grepl(Icc_Capa_int[2,3], df_capa_old$old_date))-1)] <- Icc_Capa_int[1,4]

df_capa_old$lng_icc_Capa[which(grepl(Icc_Capa_int[2,3], df_capa_old$old_date)):5478] <- Icc_Capa_int[2,4]


# (4) pt ======================================================================
# 평택#1 ~ 평택#4 2020년 2월 기준 변경 (포함 X)

df_capa_old['lng_pt_Capa'] <- rep(0)

pt_Capa <- old_capa %>% filter(grepl('평택',발전기명)) %>% filter(!grepl('신',발전기명)) %>% 
  filter(S_date != "2020-02-01")
pt_Capa 

pt_Capa_int <- aggregate(pt_Capa$Cap, list(pt_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
pt_Capa_int # 2

# 각 날짜별 용량 설정
df_capa_old$lng_pt_Capa[which(grepl(pt_Capa_int[1,3], df_capa_old$old_date)):
                          (which(grepl(pt_Capa_int[2,3], df_capa_old$old_date))-1)] <- pt_Capa_int[1,4]

df_capa_old$lng_pt_Capa[which(grepl(pt_Capa_int[2,3], df_capa_old$old_date)):5478] <- pt_Capa_int[2,4]


# (5) br ======================================================================
# =============================================================================

df_capa_old['lng_br_Capa'] <- rep(0)

br_Capa <- old_capa %>% filter(grepl('보령',발전기명)) # 9
br_Capa 

br_Capa_int <- aggregate(br_Capa$Cap, list(br_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
br_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_br_Capa[which(grepl(br_Capa_int[1,3], df_capa_old$old_date)):5478] <- br_Capa_int[1,4]


# (6) seoic ===================================================================
# =============================================================================

df_capa_old['lng_seoic_Capa'] <- rep(0)

seoic_Capa <- old_capa %>% filter(grepl('서인천',발전기명)) # 16
seoic_Capa 

seoic_Capa_int <- aggregate(seoic_Capa$Cap, list(seoic_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
seoic_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_seoic_Capa[which(grepl(seoic_Capa_int[1,3], df_capa_old$old_date)):5478] <- seoic_Capa_int[1,4]


# (7) shinic ===================================================================
# =============================================================================

df_capa_old['lng_shinic_Capa'] <- rep(0)

shinic_Capa <- old_capa %>% filter(grepl('신인천',발전기명)) # 12
shinic_Capa 

shinic_Capa_int <- aggregate(shinic_Capa$Cap, list(shinic_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
shinic_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_shinic_Capa[which(grepl(shinic_Capa_int[1,3], df_capa_old$old_date)):5478] <- shinic_Capa_int[1,4]


# (8) us ===================================================================
# =============================================================================

df_capa_old['lng_us_Capa'] <- rep(0)

us_Capa <- old_capa %>% filter(grepl('울산',발전기명)) # 12
us_Capa 

us_Capa_int <- aggregate(us_Capa$Cap, list(us_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
us_Capa_int # 2

# 각 날짜별 용량 설정
df_capa_old$lng_us_Capa[which(grepl(us_Capa_int[1,3], df_capa_old$old_date)):
                          (which(grepl(us_Capa_int[2,3], df_capa_old$old_date))-1)] <- us_Capa_int[1,4]

df_capa_old$lng_us_Capa[which(grepl(us_Capa_int[2,3], df_capa_old$old_date)):5478] <- us_Capa_int[2,4]


# (9) bs ===================================================================
# =============================================================================

df_capa_old['lng_bs_Capa'] <- rep(0)

bs_Capa <- old_capa %>% filter(grepl('부산',발전기명)) # 12
bs_Capa 

bs_Capa_int <- aggregate(bs_Capa$Cap, list(bs_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
bs_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_bs_Capa[which(grepl(bs_Capa_int[1,3], df_capa_old$old_date)):5478] <- bs_Capa_int[1,4]


# (10) may ===================================================================
# =============================================================================

df_capa_old['lng_may_Capa'] <- rep(0)

may_Capa <- old_capa %>% filter(grepl('율촌',발전기명)) # 6
may_Capa 

may_Capa_int <- aggregate(may_Capa$Cap, list(may_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
may_Capa_int # 2

# 각 날짜별 용량 설정
df_capa_old$lng_may_Capa[which(grepl(may_Capa_int[1,3], df_capa_old$old_date)):
                           (which(grepl(may_Capa_int[2,3], df_capa_old$old_date))-1)] <- may_Capa_int[1,4]

df_capa_old$lng_may_Capa[which(grepl(may_Capa_int[2,3], df_capa_old$old_date)):5478] <- may_Capa_int[2,4]


# (11) kp ===================================================================
# =============================================================================

df_capa_old['lng_kp_Capa'] <- rep(0)

kp_Capa <- old_capa %>% filter(grepl('광양',발전기명)) # 6
kp_Capa 

kp_Capa_int <- aggregate(kp_Capa$Cap, list(kp_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
kp_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_kp_Capa[which(grepl(kp_Capa_int[1,3], df_capa_old$old_date)):5478] <- kp_Capa_int[1,4]

# (12) gs ===================================================================
# =============================================================================

df_capa_old['lng_gs_Capa'] <- rep(0)

gs_Capa <- old_capa %>% filter(grepl('군산',발전기명)) # 3
gs_Capa 

gs_Capa_int <- aggregate(gs_Capa$Cap, list(gs_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
gs_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_gs_Capa[which(grepl(gs_Capa_int[1,3], df_capa_old$old_date)):5478] <- gs_Capa_int[1,4]


# (13) yw ===================================================================
# =============================================================================

df_capa_old['lng_yw_Capa'] <- rep(0)

yw_Capa <- old_capa %>% filter(grepl('영월',발전기명)) # 4
yw_Capa 

yw_Capa_int <- aggregate(yw_Capa$Cap, list(yw_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
yw_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_yw_Capa[which(grepl(yw_Capa_int[1,3], df_capa_old$old_date)):5478] <- yw_Capa_int[1,4]


# (14) os ===================================================================
# =============================================================================

df_capa_old['lng_os_Capa'] <- rep(0)

os_Capa <- old_capa %>% filter(grepl('오성',발전기명)) # 4
os_Capa 

os_Capa_int <- aggregate(os_Capa$Cap, list(os_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
os_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_os_Capa[which(grepl(os_Capa_int[1,3], df_capa_old$old_date)):5478] <- os_Capa_int[1,4]



# (15) ad ===================================================================
# =============================================================================

df_capa_old['lng_ad_Capa'] <- rep(0)

ad_Capa <- old_capa %>% filter(grepl('안동',발전기명)) # 1
ad_Capa 

ad_Capa_int <- aggregate(ad_Capa$Cap, list(ad_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
ad_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_ad_Capa[which(grepl(ad_Capa_int[1,3], df_capa_old$old_date)):5478] <- ad_Capa_int[1,4]



# (16) as ===================================================================
# =============================================================================

df_capa_old['lng_as_Capa'] <- rep(0)

as_Capa <- old_capa %>% filter(grepl('안산',발전기명)) # 3
as_Capa 

as_Capa_int <- aggregate(as_Capa$Cap, list(as_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
as_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_as_Capa[which(grepl(as_Capa_int[1,3], df_capa_old$old_date)):5478] <- as_Capa_int[1,4]


# (17) pch ===================================================================
# =============================================================================

df_capa_old['lng_pch_Capa'] <- rep(0)

pch_Capa <- old_capa %>% filter(grepl('포천',발전기명)) %>% filter(!grepl('천연',발전기명))
pch_Capa # 6

pch_Capa_int <- aggregate(pch_Capa$Cap, list(pch_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
pch_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_pch_Capa[which(grepl(pch_Capa_int[1,3], df_capa_old$old_date)):5478] <- pch_Capa_int[1,4]



# (18) ddc ===================================================================
# =============================================================================

df_capa_old['lng_ddc_Capa'] <- rep(0)

ddc_Capa <- old_capa %>% filter(grepl('동두천',발전기명)) # 6
ddc_Capa 

ddc_Capa_int <- aggregate(ddc_Capa$Cap, list(ddc_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
ddc_Capa_int # 2

# 각 날짜별 용량 설정
df_capa_old$lng_ddc_Capa[which(grepl(ddc_Capa_int[1,3], df_capa_old$old_date)):
                           (which(grepl(ddc_Capa_int[2,3], df_capa_old$old_date))-1)] <- ddc_Capa_int[1,4]

df_capa_old$lng_ddc_Capa[which(grepl(ddc_Capa_int[2,3], df_capa_old$old_date)):5478] <- ddc_Capa_int[2,4]


# (19) hh3 ===================================================================
# =============================================================================

df_capa_old['lng_hh3_Capa'] <- rep(0)

hh3_Capa <- old_capa %>% filter(발전기명 == "포스코GT#7"|
                                      발전기명 == "포스코GT#8"|
                                      발전기명 == "포스코GT#9"|
                                      발전기명 == "포스코ST#3")
hh3_Capa # 4

hh3_Capa_int <- aggregate(hh3_Capa$Cap, list(hh3_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
hh3_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_hh3_Capa[which(grepl(hh3_Capa_int[1,3], df_capa_old$old_date)):5478] <- hh3_Capa_int[1,4]


# (20) hh4 ===================================================================
# =============================================================================

df_capa_old['lng_hh4_Capa'] <- rep(0)

hh4_Capa <- old_capa %>% filter(발전기명 == "포스코GT#10"|
                                      발전기명 == "포스코GT#11"|
                                      발전기명 == "포스코GT#12"|
                                      발전기명 == "포스코ST#4")
hh4_Capa # 4

hh4_Capa_int <- aggregate(hh4_Capa$Cap, list(hh4_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
hh4_Capa_int # 1

# 각 날짜별 용량 설정
df_capa_old$lng_hh4_Capa[which(grepl(hh4_Capa_int[1,3], df_capa_old$old_date)):5478] <- hh4_Capa_int[1,4]


# (21) hh59 ===================================================================
# =============================================================================

df_capa_old['lng_hh59_Capa'] <- rep(0)

hh59_Capa <- old_capa %>% filter(발전기명 == "포스코GT#13"|발전기명 == "포스코GT#14"|
                                       발전기명 == "포스코GT#15"|발전기명 == "포스코GT#16"|
                                       발전기명 == "포스코GT#17"|발전기명 == "포스코GT#18"|
                                       발전기명 == "포스코GT#19"|발전기명 == "포스코ST#5"|
                                       발전기명 == "포스코ST#6")
hh59_Capa # 9

hh59_Capa_int <- aggregate(hh59_Capa$Cap, list(hh59_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
hh59_Capa_int # 5

# 각 날짜별 용량 설정
df_capa_old$lng_hh59_Capa[which(grepl(hh59_Capa_int[1,3], df_capa_old$old_date)):
                            (which(grepl(hh59_Capa_int[2,3], df_capa_old$old_date))-1)] <- hh59_Capa_int[1,4]

df_capa_old$lng_hh59_Capa[which(grepl(hh59_Capa_int[2,3], df_capa_old$old_date)):
                            (which(grepl(hh59_Capa_int[3,3], df_capa_old$old_date))-1)] <- hh59_Capa_int[2,4]

df_capa_old$lng_hh59_Capa[which(grepl(hh59_Capa_int[3,3], df_capa_old$old_date)):
                            (which(grepl(hh59_Capa_int[4,3], df_capa_old$old_date))-1)] <- hh59_Capa_int[3,4]

df_capa_old$lng_hh59_Capa[which(grepl(hh59_Capa_int[4,3], df_capa_old$old_date)):
                            (which(grepl(hh59_Capa_int[5,3], df_capa_old$old_date))-1)] <- hh59_Capa_int[4,4]

df_capa_old$lng_hh59_Capa[which(grepl(hh59_Capa_int[5,3], df_capa_old$old_date)):5478] <- hh59_Capa_int[5,4]



# (22) hh ===================================================================
# =============================================================================

df_capa_old['lng_hh_Capa'] <- rep(0)

hh_Capa <- old_capa %>% filter(grepl('포스코',발전기명))

hh_Capa # 21

hh_Capa_int <- aggregate(hh_Capa$Cap, list(hh_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
hh_Capa_int # 7

# 각 날짜별 용량 설정
df_capa_old$lng_hh_Capa[which(grepl("2001-01-01", df_capa_old$old_date)):
                          (which(grepl("2011-02-25", df_capa_old$old_date))-1)] <- 31200

df_capa_old$lng_hh_Capa[which(grepl("2011-02-25", df_capa_old$old_date)):
                          (which(grepl("2011-06-17", df_capa_old$old_date))-1)] <- 44990.4

df_capa_old$lng_hh_Capa[which(grepl("2011-06-17", df_capa_old$old_date)):
                          (which(grepl("2014-07-30", df_capa_old$old_date))-1)] <- 58780.8

df_capa_old$lng_hh_Capa[which(grepl("2014-07-30", df_capa_old$old_date)):
                          (which(grepl("2014-10-21", df_capa_old$old_date))-1)] <- 67795.2

df_capa_old$lng_hh_Capa[which(grepl("2014-10-21", df_capa_old$old_date)):
                          (which(grepl("2015-01-01", df_capa_old$old_date))-1)] <- 76809.6

df_capa_old$lng_hh_Capa[which(grepl("2015-01-01", df_capa_old$old_date)):
                          (which(grepl("2015-01-17", df_capa_old$old_date))-1)] <- 67209.6

df_capa_old$lng_hh_Capa[which(grepl("2015-01-17", df_capa_old$old_date)):5478] <- 76224


# (23) lg ===================================================================
# =============================================================================

df_capa_old['lng_lg_Capa'] <- rep(0)

lg_Capa <- old_capa %>% filter(grepl('당진',발전기명)) %>% filter(!grepl('복합',발전기명))
lg_Capa # 7

lg_Capa_int <- aggregate(lg_Capa$Cap, list(lg_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
lg_Capa_int # 2

# 각 날짜별 용량 설정
df_capa_old$lng_lg_Capa[which(grepl(lg_Capa_int[1,3], df_capa_old$old_date)):
                          (which(grepl(lg_Capa_int[2,3], df_capa_old$old_date))-1)] <- lg_Capa_int[1,4]

df_capa_old$lng_lg_Capa[which(grepl(lg_Capa_int[2,3], df_capa_old$old_date)):5478] <- lg_Capa_int[2,4]

# (24) pcp ===================================================================
# 2016년 이후 시작 0 =========================================================

df_capa_old['lng_pcp_Capa'] <- rep(0)

pcp_Capa <- old_capa %>% filter(grepl('포천천연',발전기명)) 
pcp_Capa # 3


# (25) dj ===================================================================
# 2016년 이후 시작 0  =======================================================

df_capa_old['lng_dj_Capa'] <- rep(0)

dj_Capa <- old_capa %>% filter(grepl('당진복합',발전기명)) 
dj_Capa # 3

# (26) ynpw ===================================================================
# 2016년 이후 시작 0 ==========================================================

df_capa_old['lng_ynpw_Capa'] <- rep(0)

ynpw_Capa <- old_capa %>% filter(grepl('영남',발전기명)) 
ynpw_Capa # 1

# (27) pjms ===================================================================
# 2016년 이후 시작 0 ===========================================================

df_capa_old['lng_pjms_Capa'] <- rep(0)

pjms_Capa <- old_capa %>% filter(grepl('파주',발전기명)) 
pjms_Capa 

# (28) su ===================================================================
# 2016년 이후 시작 0 ========================================================

df_capa_old['lng_su_Capa'] <- rep(0)

su_Capa <- old_capa %>% filter(grepl('서울',발전기명)) 
su_Capa 

# (29) npt ===================================================================
# 2016년 이후 시작 0 ==========================================================

df_capa_old['lng_npt_Capa'] <- rep(0)

npt_Capa <- old_capa %>% filter(grepl('신평택',발전기명)) 
npt_Capa 

# (30) jeju ===================================================================
# 2016년 이후 시작 0 ==========================================================

df_capa_old['lng_jeju_Capa'] <- rep(0)

jeju_Capa <- old_capa %>% filter(grepl('제주',발전기명)) 
jeju_Capa # 4

# (31) hl ===================================================================
# 2016년 이후 시작 0 ========================================================

df_capa_old['lng_hl_Capa'] <- rep(0)

hl_Capa <- old_capa %>% filter(grepl('한림',발전기명)) 
hl_Capa # 3

# (32) hpt =================================================================
# 2016년 이후 시작 0 =======================================================

df_capa_old['lng_hpt_Capa'] <- rep(0)

hpt_capa <- old_capa %>% filter(grepl('평택#',발전기명))
hpt_capa

# (33) direct =================================================================
# =============================================================================

df_capa_old['lng_direct_Capa'] <- rep(0)

# (34) kogas ==================================================================
# =============================================================================

df_capa_old['lng_kogas_Capa'] <- rep(0)


# =============================================================================
# =============================================================================

df_capa_old %>% head()
df_capa_old %>% tail()

df_capa_old <- df_capa_old %>% rename(date = old_date)
df_capa_old %>% colnames
df_capa_old %>% dim() # 5478 35

save(df_capa_old, file="/Users/user/Dropbox/KOGAS/R Data/df_lng_capa_old.Rdata")

