
setwd("/Users/user/Dropbox/KOGAS/Main")
getwd()

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

# ===========================================================


# ============================================================
#  2016 ======================================================
# ============================================================

df_1601 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201601.xlsx")
df_1602 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201602.xlsx")
df_1603 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201603.xlsx")
df_1604 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201604.xlsx")
df_1605 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201605.xlsx")
df_1606 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201606.xlsx")
df_1607 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201607.xlsx")
df_1608 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201608.xlsx")
df_1609 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201609.xlsx")
df_1610 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201610.xlsx")
df_1611 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201611.xlsx")
df_1612 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201612.xlsx")

df1601 <- df_1601$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 63

df1602 <- df_1602$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1603 <- df_1603$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1604 <- df_1604$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 

df1605 <- df_1605$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1606 <- df_1606$Sheet1 %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1607 <- df_1607$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1608 <- df_1608$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1609 <- df_1609$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1610 <- df_1610$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1611 <- df_1611$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1612 <- df_1612$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 위례열병합GT 추가 (+1) 64

df16 <- plyr::rbind.fill(df1601,df1602,df1603,df1604,df1605,df1606,
                         df1607,df1608,df1609,df1610,df1611,df1612) %>% 
  replace(is.na(.), 0)

# ============================================================
#  2017 ======================================================
# ============================================================

df_1701 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201701.xlsx")
df_1702 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201702.xlsx")
df_1703 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201703.xlsx")
df_1704 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201704.xlsx")
df_1705 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201705.xlsx")
df_1706 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201706.xlsx")
df_1707 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201707.xlsx")
df_1708 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201708.xlsx")
df_1709 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201709.xlsx")
df_1710 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201710.xlsx")
df_1711 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201711.xlsx")
df_1712 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201712.xlsx")

df1701 <- df_1701$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 64

df1702 <- df_1702$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 춘천열병합GT 추가 (+1) 65

df1703 <- df_1703$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 65

df1704 <- df_1704$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1705 <- df_1705$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1706 <- df_1706$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 동탄열병합1GT 추가 (+1) 66

df1707 <- df_1707$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 동탄열병합2GT & 부산정관1GT & 부산정관1ST 추가 (+3) 69

df1708 <- df_1708$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1709 <- df_1709$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1710 <- df_1710$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 동탄열병합1ST & 동탄열병합2ST 추가 (+2) 71

df1711 <- df_1711$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1712 <- df_1712$`발전실적(1712)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df17 <- plyr::rbind.fill(df1701,df1702,df1703,df1704,df1705,df1706,
                         df1707,df1708,df1709,df1710,df1711,df1712) %>% 
  replace(is.na(.), 0)

# ============================================================
#  2018 ======================================================
# ============================================================

df_1801 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201801.xlsx")
df_1802 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201802.xlsx")
df_1803 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201803.xlsx")
df_1804 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201804.xlsx")
df_1805 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201805.xlsx")
df_1806 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201806.xlsx")
df_1807 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201807.xlsx")
df_1808 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201808.xlsx")
df_1809 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201809.xlsx")
df_1810 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201810.xlsx")
df_1811 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201811.xlsx")
df_1812 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201812.xlsx")

df1801 <- df_1801$`발전실적(1801)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 71

df1802 <- df_1802$`발전실적(1802)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 안양열병합2-1CC GT 추가 (+1) 72

df1803 <- df_1803$`발전실적(1803)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 안양열병합2-1CC ST 추가 (+1) 73

df1804 <- df_1804$`발전실적(1804)` %>% janitor::row_to_names(1) %>%
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% select(-"제주열병합GT") # 제주열병합GT 추가 포함 X -> 73

df1805 <- df_1805$`발전실적(1805)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% select(-"제주열병합GT") # 73

df1806 <- df_1806$`발전실적(1806)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주열병합GT",-"제주LNG1GT",-"제주LNG1ST",
         -"제주LNG복합#2",-"제주LNG복합2ST",-"제주열병합ST") # 73 (제주 모두 제외)

df1807 <- df_1807$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") # 73

df1808 <- df_1808$`발전실적(1808)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 73

df1809 <- df_1809$`발전실적(1809)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") # 73

df1810 <- df_1810$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") # 73

df1811 <- df_1811$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") # 73

df1812 <- df_1812$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") # 73


df18 <- plyr::rbind.fill(df1801,df1802,df1803,df1804,df1805,df1806,
                         df1807,df1808,df1809,df1810,df1811,df1812) %>% 
  replace(is.na(.), 0)

# ============================================================
#  2019 ======================================================
# ============================================================

df_1901 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201901.xlsx")
df_1902 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201902.xlsx")
df_1903 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201903.xlsx")
df_1904 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201904.xlsx")
df_1905 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201905.xlsx")
df_1906 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201906.xlsx")
df_1907 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201907.xlsx")
df_1908 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201908.xlsx")
df_1909 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201909.xlsx")
df_1910 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201910.xlsx")
df_1911 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201911.xlsx")
df_1912 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/201912.xlsx")

df1901 <- df_1901$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 합계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") # 73

df1902 <- df_1902$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 합계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") # 73

df1903 <- df_1903$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 합계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") # 73

df1904 <- df_1904$Sheet1 %>% janitor::row_to_names(1) %>%
  select(일자, 발전기명, 발전연료, 합계) %>% 
  arrange(일자) %>% filter(발전연료=='LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) %>% 
  select(-"제주LNG1GT",-"제주LNG1ST",-"제주LNG2GT",-"제주LNG2ST") 
# 서울#4 & 서울#5 제거 (-2) 71



# ======================== LNG(D) 형태로 변화 ======================================


df1905 <- df_1905$`발전실적(1905)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 71

df1906 <- df_1906$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1907 <- df_1907$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) 

df1908 <- df_1908$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) 

df1909 <- df_1909$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1910 <- df_1910$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1911 <- df_1911$Sheet1 %>% janitor::row_to_names(1) %>%
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df1912 <- df_1912$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df19 <- plyr::rbind.fill(df1901,df1902,df1903,df1904,df1905,df1906,
                         df1907,df1908,df1909,df1910,df1911,df1912) %>% 
  replace(is.na(.), 0)

# ============================================================
#  2020 ======================================================
# ============================================================

df_2001 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202001.xlsx")
df_2002 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202002.xlsx")
df_2003 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202003.xlsx")
df_2004 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202004.xlsx")
df_2005 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202005.xlsx")
df_2006 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202006.xlsx")
df_2007 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202007.xlsx")
df_2008 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202008.xlsx")
df_2009 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202009.xlsx")
df_2010 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202010.xlsx")
df_2011 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202011.xlsx")
df_2012 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202012.xlsx")

df2001 <- df_2001$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 71

df2002 <- df_2002$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2003 <- df_2003$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2004 <- df_2004$Sheet1 %>% janitor::row_to_names(1) %>%
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2005 <- df_2005$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2006 <- df_2006$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2007 <- df_2007$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) 

df2008 <- df_2008$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2009 <- df_2009$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2010 <- df_2010$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2011 <- df_2011$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2012 <- df_2012$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)


df20 <- plyr::rbind.fill(df2001,df2002,df2003,df2004,df2005,df2006,
                         df2007,df2008,df2009,df2010,df2011,df2012) %>% 
  replace(is.na(.), 0)

# ============================================================
#  2021 ======================================================
# ============================================================

df_2101 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202101.xlsx")
df_2102 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202102.xlsx")
df_2103 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202103.xlsx")
df_2104 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202104.xlsx")
df_2105 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202105.xlsx")
df_2106 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202106.xlsx")
df_2107 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202107.xlsx")
df_2108 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202108.xlsx")
df_2109 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202109.xlsx")
df_2110 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202110.xlsx")
df_2111 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202111.xlsx")
df_2112 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202112.xlsx")

df2101 <- df_2101$`2021년 1월 발전량` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 71

df2102 <- df_2102$`2021년 2월 발전량 (2)` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료, 합계) %>% 
  arrange(일자) %>% filter(발전연료=='5.LNG(난방)') %>% select(-발전연료) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2103 <- df_2103$`2021년 3월 발전량` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2104 <- df_2104$`2021년 4월 발전량` %>% janitor::row_to_names(1) %>%
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2105 <- df_2105$`2021년 5월 발전량` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2106 <- df_2106$`2021년 6월 발전량` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2107 <- df_2107$`2021년 7월 발전량` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2108 <- df_2108$`2021년 8월 발전량` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0)

df2109 <- df_2109$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) 

df2110 <- df_2110$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 안양열병합2-2CC GT 추가 (+1) 72

df2111 <- df_2111$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 안양열병합2-2CC ST 추가 (+1) 73

df2112 <- df_2112$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) 


df21 <- plyr::rbind.fill(df2101,df2102,df2103,df2104,df2105,df2106,
                         df2107,df2108,df2109,df2110,df2111,df2112) %>% 
  replace(is.na(.), 0)

# ============================================================
#  2021 ======================================================
# ============================================================

df_2201 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202201.xlsx")
df_2202 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202202.xlsx")
df_2203 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202203.xlsx")
df_2204 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202204.xlsx")

df2201 <- df_2201$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 73

df2202 <- df_2202$`2022년 2월 발전량` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 73

df2203 <- df_2203$`2022년 3월 발전량` %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 합계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 73

df2204 <- df_2204$Sheet1 %>% janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 발전연료2, 계) %>% 
  arrange(일자) %>% filter(발전연료2=='5.LNG(난방)') %>% select(-발전연료2) %>% 
  spread(key = 발전기명, value = 계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 73


df22 <- plyr::rbind.fill(df2201,df2202,df2203,df2204) %>% 
  replace(is.na(.), 0)

# ============================================================


df_chp <- plyr::rbind.fill(df16,df17,df18,df19,df20,df21,df22) %>%  
  replace(is.na(.), 0)

save(df_chp, file="/Users/user/Dropbox/KOGAS/R Data/df_chp_2204.RData")

