library("rvest")
library("dplyr")

# harvesting Indonesia's holiday
page_event <- "https://www.liburnasional.com/kalender-lengkap-2021/" %>% 
  read_html()

event <- page_event %>% 
  html_nodes(".libnas-calendar-full-detail") %>% 
  html_table() %>% 
  bind_rows() %>% 
  select(X1, X3) %>% 
  as_tibble()

# mapping the holidays' day
event <- rbind(event, event[rep(8,1),])
event[8,1] <- "15 Mei"
event[16,1] <- "16 Mei"

event$X1 <- paste0(event$X1, " 21") %>% 
  as.Date("%d %b %y")

event <- event %>% 
  rename("tanggal" = "X1", "peringatan" = "X3") %>% 
  mutate(hari = julian.Date(.$tanggal, origin = as.Date("2020-12-31")))

tanggal <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "1 day")
holiday <- ifelse(format(tanggal, "%w") %in% c(6, 0), "Weekend", NA)
holiday[event$hari] <- "Libur Nasional" 

# create calendar
library("calendR")
calendR(
  year = 2021, 
  start = "M", 
  special.days = holiday, 
  special.col = c("lightblue", "pink"),  # holidays col
  days.col = 1,                          # days text col
  months.col = "white",                  # months text col
  mbg.col = 4,                           # months bg col
  bg.col = "#f4f4f4",                    # bg col
  lty = 0,
  title = "KALENDER 2021",
  orientation = "l", 
  papersize = "A4",
  # pdf = TRUE
)
