library(DT)
library(dplyr)

df <- readxl::read_excel("asf.xlsx")
df$date <- lubridate::ymd(df$date)
df <- filter(df, farm != 0)


colnames(df) <- c("확정일자", "살처분농가수", "누적농가수", "살처분두수", "누적살처분두수", "발생농가수")

datatable(
  df,
  rownames = FALSE,
  extensions = c("Scroller", "Buttons"),
  options = list(
    autoWidth = TRUE,
    # columnDefs = list(list(targets = "_all", className = "dt-center")),
    # pageLength = 100,
    lengthMenu = c(25, 50, 100, 500),
    # deferRender = TRUE,
    # scrollX = TRUE,
    # scrollY = 800,
    dom = "Blfrtip",
    buttons = c("copy", "excel", "print")
  )
) %>%
  formatStyle("살처분농가수",
    background = styleColorBar(c(0, max(df$살처분농가수, na.rm = TRUE)), "lightblue"),
    backgroundSize = "98% 88%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center"
  ) %>%
  formatStyle("살처분두수",
    background = styleColorBar(c(0, max(df$살처분두수, na.rm = TRUE)), "pink"),
    backgroundSize = "98% 88%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center"
  )
