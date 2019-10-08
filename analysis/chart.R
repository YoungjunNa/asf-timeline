library(tidyverse)
library(highcharter)

df <- readxl::read_excel("asf1.xlsx")
df$date <- lubridate::ymd(df$date)

df1 <- df %>% 
  group_by(date) %>%
  summarise(head = sum(head), area = sum(area), farm = sum(farm)) %>%
  mutate(sum = head + area, head_acc = cumsum(head), area_acc = cumsum(area), sum_acc = cumsum(sum), farm_acc = cumsum(farm))

df3 <- df %>%
  group_by(city) %>%
  summarise(sum_head = sum(sum), sum_farm = sum(farm))

library(treemap)
tm <- treemap(df3,
              index = c("city"),
              vSize = "sum_head", vColor = "sum_farm",
              type = "value", palette = c("#b18ea6", "#b2e4d5", "#f2a6a6", "#e7f3ee"),
              mapping = c(min(df3$sum_farm), max(df3$sum_farm))
)

hctreemap(tm)

df3 %>%
  filter(sum_head != 0) %>%
  hchart("treemap", hcaes(x = city, value = sum_head), colors = c("#b2e4d5", "#f2a6a6", "#b18ea6", "#e7f3ee"))

# line ----
df1 %>%
  select("date", "head_acc", "area_acc", "sum_acc") %>%
  rename("발생농가"= "head_acc", "예방적안락사" = "area_acc", "합계"= "sum_acc") %>%
  gather("발생농가", "예방적안락사", "합계", key = "type", value = "head") %>%
  hchart(type = "line", hcaes(x = date, y = head, group = type)) %>%
  hc_title(text = "일자별 안락사 대상두수(누적)") %>%
  hc_add_theme(hc_theme_darkunica())

df1 %>%
  hchart(type = "line", hcaes(x = date, y = farm_acc), color = "red") %>%
  hc_title(text = "일자별 안락사 대상농가수(누적)") %>%
  hc_add_theme(hc_theme_darkunica()) 

# column ----
df1 %>%
  rename("발생농가" = "head", "예방적안락사" = area) %>%
  gather("발생농가", "예방적안락사", key = "type", value = "head") %>%
  hchart(type = "column", hcaes(x = date, y = head, group = type)) %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_title(text = "일자별 안락사 대상두수") %>%
  hc_add_theme(hc_theme_darkunica()) 

df1 %>%
  hchart(type = "column", hcaes(x = date, y = farm)) %>%
  hc_title(text = "일자별 안락사 대상농가수") %>%
  hc_add_theme(hc_theme_darkunica()) 

# map ----
hcmap("countries/kr/kr-all", showInLegend = FALSE) %>% 
  hc_add_series(data = filter(df, head != 0), type = "mapbubble", name = "ASF 발생지역", maxSize = '5%', hcaes(color = "address")) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(hc_theme_538()) 



# df2 ----
df2 <- df %>%
  filter(head != 0) %>%
  select(date, address, head, area, sum, sum_acc, farm, farm_acc, x)

colnames(df2) <- c("일자", "위치", "해당농장두수", "예방적두수", "안락사합계", "누적안락사두수", "농가수", "누적농가수", "비고")

df2$No <- 1:nrow(df2)
df2 <- select(df2, No, everything())

## timevis ----

library(timevis)

tv <- df %>%
  filter(head != 0) %>%
  select(date, address, city) %>%
  rename(start = "date", content = "address", id = "city") %>%
  mutate(group = id) %>%
  mutate(type = "point") %>%
  mutate(id = 1:13)

tv_groups <- data.frame(
  id = c("파주시", "인천광역시", "연천군", "김포시"),
  content = c("파주시", "인천광역시", "연천군", "김포시")
)

timevis(data = tv, groups = tv_groups, height = "400px", width = "100%")

## table ----

library(formattable)
formattable(
  df2,
  list(
    area(col = c(해당농장두수)) ~ normalize_bar("#eb7070", 0.2),
    area(col = c(예방적두수)) ~ normalize_bar("#fec771", 0.2),
    area(col = c(안락사합계)) ~ normalize_bar("#e6e56c", 0.2),
    area(col = c(누적안락사두수)) ~ normalize_bar("#7f78d2", 0.2),
    area(col = c(농가수)) ~ normalize_bar("#64e291", 0.2),
    area(col = c(누적농가수)) ~ normalize_bar("pink", 0.2)
  )
)

# library(DT)
# datatable(
#   df2,
#   rownames = FALSE,
#   extensions = c("Scroller", "Buttons"),
#   options = list(
#     autoWidth = TRUE,
#     columnDefs = list(
#       list(width = "170px", targets = "_all", className = "dt-center") 
#     ),
#     # pageLength = 100,
#     lengthMenu = c(15, 25, 50, 100, 500),
#     deferRender = TRUE,
#     # scrollX = TRUE,
#     # scrollY = 800,
#     dom = "Blfrtip",
#     buttons = c("copy", "excel", "print")
#   )
# ) %>%
#   formatStyle("안락사두수",
#               background = styleColorBar(c(0, max(df2$안락사두수, na.rm = TRUE)), "#eb7070"),
#               backgroundSize = "98% 88%",
#               backgroundRepeat = "no-repeat",
#               backgroundPosition = "center"
#   ) %>%
#   formatStyle("예방적안락사두수",
#               background = styleColorBar(c(0, max(df2$예방적안락사두수, na.rm = TRUE)), "#fec771"),
#               backgroundSize = "98% 88%",
#               backgroundRepeat = "no-repeat",
#               backgroundPosition = "center"
#   ) %>%
#   formatStyle("안락사두수합계",
#               background = styleColorBar(c(0, max(df2$안락사두수합계, na.rm = TRUE)), "#e6e56c"),
#               backgroundSize = "98% 88%",
#               backgroundRepeat = "no-repeat",
#               backgroundPosition = "center"
#   ) %>%
#   formatStyle("누적안락사두수",
#               background = styleColorBar(c(0, max(df2$누적안락사두수, na.rm = TRUE)), "#7f78d2"),
#               backgroundSize = "98% 88%",
#               backgroundRepeat = "no-repeat",
#               backgroundPosition = "center"
#   ) %>%
#   formatStyle("안락사농가수",
#               background = styleColorBar(c(0, max(df2$안락사농가수, na.rm = TRUE)), "#64e291"),
#               backgroundSize = "98% 88%",
#               backgroundRepeat = "no-repeat",
#               backgroundPosition = "center"
#   ) %>%
#   formatStyle("누적안락사농가수",
#                  background = styleColorBar(c(0, max(df2$누적안락사농가수, na.rm = TRUE)), "pink"),
#                  backgroundSize = "98% 88%",
#                  backgroundRepeat = "no-repeat",
#                  backgroundPosition = "center"
#   )
# 
