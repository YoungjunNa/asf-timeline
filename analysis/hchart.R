library(tidyverse)
library(highcharter)
library(htmltools)


df <- readxl::read_excel("asf.xlsx")
df$date <- lubridate::ymd(df$date)

# line ----
df %>%
  hchart(type = "line", hcaes(x = date, y = head_accumulated)) %>%
  hc_title(text = "일자별 살처분 대상두수(누적)") %>%
  hc_add_theme(hc_theme_darkunica())

df %>%
  hchart(type = "line", hcaes(x = date, y = farm_accumulated), color = "red") %>%
  hc_title(text = "일자별 살처분 대상농가수(누적)") %>%
  hc_add_theme(hc_theme_darkunica()) 

# column ----
df %>%
  hchart(type = "column", hcaes(x = date, y = head)) %>%
  hc_title(text = "일자별 살처분 대상두수") %>%
  hc_add_theme(hc_theme_darkunica()) 

df %>%
  hchart(type = "column", hcaes(x = date, y = farm)) %>%
  hc_title(text = "일자별 살처분 대상농가수") %>%
  hc_add_theme(hc_theme_darkunica()) 



# map ----
cities <- readxl::read_excel("map-data.xlsx")

hcmap("countries/kr/kr-all", showInLegend = FALSE) %>% 
  hc_add_series(data = cities, type = "mapbubble", name = "ASF 발생지역", maxSize = '5%', hcaes(color = "name")) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(hc_theme_538()) 


