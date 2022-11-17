
setwd("/Users/user/Dropbox/KOGAS/Main/Past Codes")
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
# 18 ===========================================================

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


df1803 <- df_1803$`201803`%>% janitor::row_to_names(1) %>% select('총합계') %>% slice(-1) %>% slice(-32)
df1804 <- df_1804$`201804`%>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df1805 <- df_1805$`201805`%>% janitor::row_to_names(1) %>% select('총합계') %>% slice(-1) %>% slice(-32)
df1806 <- df_1806$`201806`%>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df1807 <- df_1807$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df1808 <- df_1808$`210808`%>% janitor::row_to_names(1) %>% select('총합계') %>% slice(-1) %>% slice(-32)
df1809 <- df_1809$`201809` %>% janitor::row_to_names(1) %>% select('총합계') %>% slice(-1) %>% slice(-31)
df1810 <- df_1810$Sheet2 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df1811 <- df_1811$Sheet2 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df1812 <- df_1812$Sheet2 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)

df18 <- cbind(seq(as.Date("2018-03-01"), as.Date("2018-12-31"), by=1), 
              rbind(df1803,df1804,df1805,df1806,df1807,
                    df1808,df1809,df1810,df1811,df1812)) %>% 
  set_names(c('date','total'))

head(df18)
tail(df18)
ts.plot(df18 %>% select(total))

# 19 ===========================================================

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

df1901 <- df_1901$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df1902 <- df_1902$Sheet2 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-29)
df1903 <- df_1903$Sheet2 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df1904 <- df_1904$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df1905 <- df_1905$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df1906 <- df_1906$Sheet4 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df1907 <- df_1907$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df1908 <- df_1908$Sheet4 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df1909 <- df_1909$Sheet5 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df1910 <- df_1910$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df1911 <- df_1911$Sheet4 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df1912 <- df_1912$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)


df19 <- cbind(seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by=1), 
              rbind(df1901,df1902,df1903,df1904,df1905,df1906,
                    df1907,df1908,df1909,df1910,df1911,df1912)) %>% 
  set_names(c('date','total'))

head(df19)
tail(df19)
ts.plot(df19 %>% select(total))

# 20 ===========================================================

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

df2001 <- df_2001$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df2002 <- df_2002$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-30)
df2003 <- df_2003$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df2004 <- df_2004$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df2005 <- df_2005$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df2006 <- df_2006$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df2007 <- df_2007$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df2008 <- df_2008$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df2009 <- df_2009$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df2010 <- df_2010$Sheet4 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df2011 <- df_2011$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-31)
df2012 <- df_2012$Sheet6 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)

df20 <- cbind(seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by=1), 
              rbind(df2001,df2002,df2003,df2004,df2005,df2006,df2007,
                    df2008,df2009,df2010,df2011,df2012)) %>% 
  set_names(c('date','total'))

head(df20)
tail(df20)
ts.plot(df20 %>% select(total))

# 21 ===========================================================

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

df2101 <- df_2101$Sheet3 %>% janitor::row_to_names(3) %>% select('총합계') %>% slice(-32)
df2102 <- df_2102$`202102(가동률 엑셀)` %>% janitor::row_to_names(1) %>%
  select('총합계') %>% slice(-1) %>% slice(-29)
df2103 <- df_2103$`202103(가동률 엑셀)` %>% janitor::row_to_names(1) %>% 
  select('총합계') %>% slice(-1) %>% slice(-32)
df2104 <- df_2104$`202104(가동률 엑셀)` %>% janitor::row_to_names(1) %>%
  select('총합계') %>% slice(-1) %>% slice(-31)
df2105 <- df_2105$`202105(가동률 엑셀)` %>% janitor::row_to_names(1) %>% 
  select('총합계') %>% slice(-1) %>% slice(-32)
df2106 <- df_2106$`202106(가동률 엑셀)` %>% janitor::row_to_names(1) %>% 
  select('총합계') %>% slice(-1) %>% slice(-31)
df2107 <- df_2107$`202107(가동률 엑셀)` %>% janitor::row_to_names(1) %>% 
  select('총합계') %>% slice(-1) %>% slice(-32)
df2108 <- df_2108$`202108(가동률 엑셀)` %>% janitor::row_to_names(1) %>% 
  select('총합계') %>% slice(-1) %>% slice(-32)

# ======== Raw Data 총합계 Not Available ======== 

df09 <- df_2109$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 합계) %>% arrange(일자) %>% spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) # 446
df2109 <- data.frame(rowSums(df09[,2:446])) %>% set_names("총합계")
df2109

df10 <- df_2110$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 합계) %>% arrange(일자) %>% spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) # 447
df2110 <- data.frame(rowSums(df10[,2:447])) %>% set_names("총합계")
df2110

df11 <- df_2111$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 합계) %>% arrange(일자) %>% spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 448
df2111 <- data.frame(rowSums(df11[,2:448])) %>% set_names("총합계")
df2111

df12 <- df_2112$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 합계) %>% arrange(일자) %>% spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 443
df2112 <- data.frame(rowSums(df12[,2:443])) %>% set_names("총합계")
df2112

df21 <- cbind(seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by=1), 
              rbind(df2101,df2102,df2103,df2104,df2105,df2106,
                    df2107,df2108,df2109,df2110,df2111,df2112)) %>% 
  set_names(c('date','total'))

head(df21)
tail(df21)
ts.plot(df21 %>% select(total))

# 22 ===========================================================

df_2201 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202201.xlsx")
df_2202 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202202.xlsx")
df_2203 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202203.xlsx")
df_2204 <- read_excel_allsheets("/Users/user/Dropbox/KOGAS/Raw Data/202204.xlsx")

df01 <- df_2201$Sheet1 %>% janitor::row_to_names(1) %>% select(일자, 발전기명, 합계) %>% arrange(일자) %>% spread(key = 발전기명, value = 합계) %>% mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 441

df02 <- df_2202$`2022년 2월 발전량` %>% 
  janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 합계) %>% arrange(일자) %>% 
  spread(key = 발전기명, value = 합계) %>% 
  mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 438

df03 <- df_2203$`2022년 3월 발전량` %>% 
  janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 합계) %>% arrange(일자) %>% 
  spread(key = 발전기명, value = 합계) %>% 
  mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 438

df04 <- df_2204$Sheet1 %>% 
  janitor::row_to_names(1) %>% 
  select(일자, 발전기명, 계) %>% arrange(일자) %>% 
  spread(key = 발전기명, value = 계) %>% 
  mutate_all(function(x) as.numeric(as.character(x))) %>% 
  replace(is.na(.), 0) # 438

df2201 <- data.frame(rowSums(df01[,2:441])) %>% set_names("총합계")
df2202 <- data.frame(rowSums(df02[,2:438])) %>% set_names("총합계")
df2203 <- data.frame(rowSums(df03[,2:438])) %>% set_names("총합계")
df2204 <- data.frame(rowSums(df04[,2:438])) %>% set_names("총합계")

df22 <- cbind(seq(as.Date("2022-01-01"), as.Date("2022-04-30"), by=1), 
              rbind(df2201,df2202,df2203,df2204)) %>% 
  set_names(c('date','total'))

# ================================================================================

df_final <- rbind(df18,df19,df20,df21,df22)
df_final %>% tail()
df_final %>% select(total) %>% ts.plot()

save(df_final, file="/Users/user/Dropbox/KOGAS/R Data/df_tot_2204.RData")
