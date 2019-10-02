library(tidyverse)
library(highcharter)

df <- readxl::read_excel("asf1.xlsx")
df$date <- lubridate::ymd(df$date)

df1 <- df %>% 
  group_by(date) %>%
  summarise(head = sum(head), area = sum(area), farm = sum(farm)) %>%
  mutate(sum = head + area, head_acc = cumsum(head), area_acc = cumsum(area), sum_acc = cumsum(sum), farm_acc = cumsum(farm))

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



# table ----
df2 <- df %>%
  filter(head != 0) %>%
  select(date, address, head, area, sum, sum_acc, farm, farm_acc, x)

colnames(df2) <- c("일자", "위치", "안락사두수", "예방적안락사두수", "안락사두수합계", "누적안락사두수", "안락사농가수", "누적안락사농가수", "비고")


library(DT)
datatable(
  df2,
  rownames = FALSE,
  extensions = c("Scroller", "Buttons"),
  options = list(
    autoWidth = TRUE,
    columnDefs = list(
      list(width = "170px", targets = "_all", className = "dt-center") 
    ),
    # pageLength = 100,
    lengthMenu = c(15, 25, 50, 100, 500),
    deferRender = TRUE,
    # scrollX = TRUE,
    # scrollY = 800,
    dom = "Blfrtip",
    buttons = c("copy", "excel", "print")
  )
) %>%
  formatStyle("안락사두수",
              background = styleColorBar(c(0, max(df2$안락사두수, na.rm = TRUE)), "#eb7070"),
              backgroundSize = "98% 88%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center"
  ) %>%
  formatStyle("예방적안락사두수",
              background = styleColorBar(c(0, max(df2$예방적안락사두수, na.rm = TRUE)), "#fec771"),
              backgroundSize = "98% 88%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center"
  ) %>%
  formatStyle("안락사두수합계",
              background = styleColorBar(c(0, max(df2$안락사두수합계, na.rm = TRUE)), "#e6e56c"),
              backgroundSize = "98% 88%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center"
  ) %>%
  formatStyle("누적안락사두수",
              background = styleColorBar(c(0, max(df2$누적안락사두수, na.rm = TRUE)), "#7f78d2"),
              backgroundSize = "98% 88%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center"
  ) %>%
  formatStyle("안락사농가수",
              background = styleColorBar(c(0, max(df2$안락사농가수, na.rm = TRUE)), "#64e291"),
              backgroundSize = "98% 88%",
              backgroundRepeat = "no-repeat",
              backgroundPosition = "center"
  ) %>%
  formatStyle("누적안락사농가수",
                 background = styleColorBar(c(0, max(df2$누적안락사농가수, na.rm = TRUE)), "pink"),
                 backgroundSize = "98% 88%",
                 backgroundRepeat = "no-repeat",
                 backgroundPosition = "center"
  )

