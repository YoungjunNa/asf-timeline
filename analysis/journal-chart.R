library(tidyverse)
library(highcharter)
library(broom)
library(highcharter)

df <- readxl::read_excel("asf1.xlsx")
df$date <- lubridate::ymd(df$date)

df1 <- df %>% filter(journal != 0)

lm.model <- augment(lm(journal ~ date, data = df1))

lm.model %>%
  hchart(type = "line", hcaes(x = date, y = journal), name = "ASF 기사수") %>%
  hc_title(text = "일자별 언론관심도(네이버 기사수)") %>%
  hc_add_series(lm.model, "line", hcaes(x = date, y = .fitted)) %>%
  hc_add_series(df1, "line", hcaes(x = date, y = joguk), name = "조국 기사수") %>%
  hc_add_theme(hc_theme_darkunica())

## 빅카인드 데이터 ----

big <- readxl::read_excel("NewsResult_20190916-20191010.xlsx")

big$일자 <- lubridate::ymd(big$일자)

big %>%
  group_by(언론사) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  hchart("column", hcaes(언론사, n)) %>%
  hc_title(text = "언론사별 기사수(190917-1010)")
