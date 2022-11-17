
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
  filter(Cat == 'LNG_CHP') %>% mutate(Cap = Cap*24) %>% arrange(S_date) %>% arrange(E_date)

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

# (1) su ======================================================================
# ==============================================================================

df_capa_old['chp_su_Capa'] <- rep(0)

su_Capa <- old_capa %>% filter(grepl('서울',발전기명))
su_Capa

su_Capa_int <- aggregate(su_Capa$Cap, list(su_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_Sum = cumsum(Cap))
su_Capa_int # 2 인데 어차피 끝나는 일 2016년 이후

df_capa_old$chp_su_Capa[which(grepl(su_Capa_int[1,3], df_capa_old$old_date)):5478] <- su_Capa_int[2,4]

# (2) bd ======================================================================
# ==============================================================================

df_capa_old['chp_bd_Capa'] <- rep(0)

bd_Capa <- old_capa %>% filter(grepl('분당',발전기명))
bd_Capa

bd_Capa_int <- aggregate(bd_Capa$Cap, list(bd_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_bdm = cumsum(Cap))
bd_Capa_int # 1

df_capa_old$chp_bd_Capa[which(grepl(bd_Capa_int[1,3], df_capa_old$old_date)):5478] <- bd_Capa_int[1,4]

# (3) is ======================================================================
# ==============================================================================

df_capa_old['chp_is_Capa'] <- rep(0)

is_Capa <- old_capa %>% filter(grepl('일산',발전기명))
is_Capa

is_Capa_int <- aggregate(is_Capa$Cap, list(is_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_ism = cumsum(Cap))
is_Capa_int # 1

df_capa_old$chp_is_Capa[which(grepl(is_Capa_int[1,3], df_capa_old$old_date)):5478] <- is_Capa_int[1,4]

# (4) ay ======================================================================
# ==============================================================================

df_capa_old['chp_ay_Capa'] <- rep(0)

ay_Capa <- old_capa %>% filter(grepl('안양',발전기명)) %>% filter(!grepl('열병합',발전기명))
ay_Capa

ay_Capa_int <- aggregate(ay_Capa$Cap, list(ay_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_aym = cumsum(Cap))
ay_Capa_int # 1

df_capa_old$chp_ay_Capa[which(grepl(ay_Capa_int[1,3], df_capa_old$old_date)):5478] <- ay_Capa_int[1,4]

# (5) ay21 ======================================================================
# 16년 이후 =====================================================================

df_capa_old['chp_ay21_Capa'] <- rep(0)

ay21_Capa <- old_capa %>% filter(grepl('안양열병합2-1',발전기명))
ay21_Capa

# (6) ay22 =====================================================================
# ==============================================================================

df_capa_old['chp_ay22_Capa'] <- rep(0)

ay_Capa <- old_capa %>% filter(grepl('안양열병합2-2',발전기명))
ay_Capa

# (7) bc ======================================================================
# ==============================================================================

df_capa_old['chp_bc_Capa'] <- rep(0)

bc_Capa <- old_capa %>% filter(grepl('부천',발전기명))
bc_Capa

bc_Capa_int <- aggregate(bc_Capa$Cap, list(bc_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_bcm = cumsum(Cap))
bc_Capa_int # 1

df_capa_old$chp_bc_Capa[which(grepl(bc_Capa_int[1,3], df_capa_old$old_date)):5478] <- bc_Capa_int[1,4]

# (8) pk =======================================================================
# ==============================================================================

df_capa_old['chp_pk_Capa'] <- rep(0)

pk_Capa <- old_capa %>% filter(grepl('판교',발전기명))
pk_Capa

pk_Capa_int <- aggregate(pk_Capa$Cap, list(pk_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_pkm = cumsum(Cap))
pk_Capa_int # 1

df_capa_old$chp_pk_Capa[which(grepl(pk_Capa_int[1,3], df_capa_old$old_date)):5478] <- pk_Capa_int[1,4]

# (9) pj =======================================================================
# ==============================================================================

df_capa_old['chp_pj_Capa'] <- rep(0)

pj_Capa <- old_capa %>% filter(grepl('파주',발전기명))
pj_Capa

pj_Capa_int <- aggregate(pj_Capa$Cap, list(pj_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_pjm = cumsum(Cap))
pj_Capa_int # 1

df_capa_old$chp_pj_Capa[which(grepl(pj_Capa_int[1,3], df_capa_old$old_date)):5478] <- pj_Capa_int[1,4]

# (10) kk =======================================================================
# ==============================================================================

df_capa_old['chp_kk_Capa'] <- rep(0)

kk_Capa <- old_capa %>% filter(grepl('광교',발전기명))
kk_Capa

kk_Capa_int <- aggregate(kk_Capa$Cap, list(kk_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_kkm = cumsum(Cap))
kk_Capa_int # 1

df_capa_old$chp_kk_Capa[which(grepl(kk_Capa_int[1,3], df_capa_old$old_date)):5478] <- kk_Capa_int[1,4]

# (11) hws =======================================================================
# ==============================================================================

df_capa_old['chp_hws_Capa'] <- rep(0)

hws_Capa <- old_capa %>% filter(grepl('화성',발전기명))
hws_Capa

hws_Capa_int <- aggregate(hws_Capa$Cap, list(hws_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_hwsm = cumsum(Cap))
hws_Capa_int # 1

df_capa_old$chp_hws_Capa[which(grepl(hws_Capa_int[1,3], df_capa_old$old_date)):5478] <- hws_Capa_int[1,4]


# (12) sd =======================================================================
# ==============================================================================

df_capa_old['chp_sd_Capa'] <- rep(0)

sd_Capa <- old_capa %>% filter(grepl('송도',발전기명))
sd_Capa

sd_Capa_int <- aggregate(sd_Capa$Cap, list(sd_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_sdm = cumsum(Cap))
sd_Capa_int # 1

df_capa_old$chp_sd_Capa[which(grepl(sd_Capa_int[1,3], df_capa_old$old_date)):5478] <- sd_Capa_int[1,4]


# (13) yj =======================================================================
# ==============================================================================

df_capa_old['chp_yj_Capa'] <- rep(0)

yj_Capa <- old_capa %>% filter(grepl('양주',발전기명))
yj_Capa

yj_Capa_int <- aggregate(yj_Capa$Cap, list(yj_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_yjm = cumsum(Cap))
yj_Capa_int # 1

df_capa_old$chp_yj_Capa[which(grepl(yj_Capa_int[1,3], df_capa_old$old_date)):5478] <- yj_Capa_int[1,4]


# (14) dgh =======================================================================
# ==============================================================================

df_capa_old['chp_dgh_Capa'] <- rep(0)

dgh_Capa <- old_capa %>% filter(grepl('대구',발전기명))
dgh_Capa

dgh_Capa_int <- aggregate(dgh_Capa$Cap, list(dgh_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_dghm = cumsum(Cap))
dgh_Capa_int # 1

df_capa_old$chp_dgh_Capa[which(grepl(dgh_Capa_int[1,3], df_capa_old$old_date)):5478] <- dgh_Capa_int[1,4]


# (15) sj =======================================================================
# ==============================================================================

df_capa_old['chp_sj_Capa'] <- rep(0)

sj_Capa <- old_capa %>% filter(grepl('세종',발전기명))
sj_Capa

sj_Capa_int <- aggregate(sj_Capa$Cap, list(sj_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_sjm = cumsum(Cap))
sj_Capa_int # 1

df_capa_old$chp_sj_Capa[which(grepl(sj_Capa_int[1,3], df_capa_old$old_date)):5478] <- sj_Capa_int[1,4]


# (16) bn =======================================================================
# ==============================================================================

df_capa_old['chp_bn_Capa'] <- rep(0)

bn_Capa <- old_capa %>% filter(grepl('별내',발전기명))
bn_Capa

bn_Capa_int <- aggregate(bn_Capa$Cap, list(bn_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_bnm = cumsum(Cap))
bn_Capa_int # 1

df_capa_old$chp_bn_Capa[which(grepl(bn_Capa_int[1,3], df_capa_old$old_date)):5478] <- bn_Capa_int[1,4]


# (17) lh =======================================================================
# ==============================================================================

df_capa_old['chp_lh_Capa'] <- rep(0)

lh_Capa <- old_capa %>% filter(grepl('아산',발전기명))
lh_Capa


# (18) sw =======================================================================
# ==============================================================================

df_capa_old['chp_sw_Capa'] <- rep(0)

sw_Capa <- old_capa %>% filter(grepl('수완',발전기명))
sw_Capa

sw_Capa_int <- aggregate(sw_Capa$Cap, list(sw_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_swm = cumsum(Cap))
sw_Capa_int # 1

df_capa_old$chp_sw_Capa[which(grepl(sw_Capa_int[1,3], df_capa_old$old_date)):5478] <- sw_Capa_int[1,4]


# (19) icair =======================================================================
# ==============================================================================

df_capa_old['chp_icair_Capa'] <- rep(0)

icair_Capa <- old_capa %>% filter(grepl('인천',발전기명))
icair_Capa

icair_Capa_int <- aggregate(icair_Capa$Cap, list(icair_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_icairm = cumsum(Cap))
icair_Capa_int # 1

df_capa_old$chp_icair_Capa[which(grepl(icair_Capa_int[1,3], df_capa_old$old_date)):5478] <- icair_Capa_int[1,4]


# (20) hn =======================================================================
# ==============================================================================

df_capa_old['chp_hn_Capa'] <- rep(0)

hn_Capa <- old_capa %>% filter(grepl('하남',발전기명))
hn_Capa

hn_Capa_int <- aggregate(hn_Capa$Cap, list(hn_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_hnm = cumsum(Cap))
hn_Capa_int # 1

df_capa_old$chp_hn_Capa[which(grepl(hn_Capa_int[1,3], df_capa_old$old_date)):5478] <- hn_Capa_int[1,4]


# (21) cc =======================================================================
# ==============================================================================

df_capa_old['chp_cc_Capa'] <- rep(0)

cc_Capa <- old_capa %>% filter(grepl('춘천',발전기명))
cc_Capa

# (22) dt =======================================================================
# ==============================================================================

df_capa_old['chp_dt_Capa'] <- rep(0)

dt_Capa <- old_capa %>% filter(grepl('동탄',발전기명))
dt_Capa

# (23) jk =======================================================================
# ==============================================================================

df_capa_old['chp_jk_Capa'] <- rep(0)

jk_Capa <- old_capa %>% filter(grepl('부산',발전기명))
jk_Capa

# (24) wr =======================================================================
# ==============================================================================

df_capa_old['chp_wr_Capa'] <- rep(0)

wr_Capa <- old_capa %>% filter(grepl('위례',발전기명))
wr_Capa

# (25) los =====================================================================
# 실제 발전량 자료는 존재하지 X 그러나 CHP_DPG 자료 보면 12월 10일부터 발전량 존재

df_capa_old['chp_los_Capa'] <- rep(0)

los_Capa <- old_capa %>% filter(grepl('오산',발전기명))
los_Capa

los_Capa_int <- aggregate(los_Capa$Cap, list(los_Capa$Int), sum) %>%
  set_names(c("Int","Cap")) %>%
  arrange(Int) %>% 
  mutate(S_date = Int@start) %>% 
  mutate(Cap_losm = cumsum(Cap))
los_Capa_int # 1

df_capa_old %>% dim()
df_capa_old$chp_los_Capa[which(grepl("2015-12-10", df_capa_old$old_date)):5478] <- los_Capa_int[1,4]

# (26) direct =================================================================
# =============================================================================

df_capa_old['chp_direct_Capa'] <- rep(0)

# (27) kogas ==================================================================
# =============================================================================

df_capa_old['chp_kogas_Capa'] <- rep(0)


# =============================================================================
# =============================================================================

df_capa_old %>% head()
df_capa_old %>% tail()

df_capa_old <- df_capa_old %>% rename(date = old_date)
df_capa_old %>% colnames
df_capa_old %>% dim() # 5478 28 

save(df_capa_old, file="/Users/user/Dropbox/KOGAS/R Data/df_chp_capa_old.Rdata")
