library(tidyverse)
library(highcharter)
library(broom)

df <- readxl::read_excel("asf1.xlsx")
df$date <- lubridate::ymd(df$date)

df1 <- df %>% filter(journal != 0)

lm.model <- augment(lm(journal ~ date, data = df1))

lm.model %>%
  hchart(type = "line", hcaes(x = date, y = journal)) %>%
  hc_title(text = "일자별 언론관심도(네이버 기사수)") %>%
  hc_add_series(lm.model, "line", hcaes(x = date, y = .fitted)) %>%
  hc_add_theme(hc_theme_darkunica())
