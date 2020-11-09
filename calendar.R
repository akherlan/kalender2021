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

event <- tidyr::separate(event, X1, c("tanggal", "bulan"), " ")

event$tanggal <- as.integer(event$tanggal)
event$bulan <- substr(event$bulan, 1, 3) %>% 
  ifelse(. == "Mei", "May", .) %>% 
  ifelse(. == "Agu", "Aug", .) %>%
  ifelse(. == "Okt", "Oct", .) %>% 
  ifelse(. == "Des", "Dec", .)

dcount <- c(rep(c(31,30), 3), 31, rep(c(31, 30), 3))[1:12] %>% 
  replace(2, 28)
event_num <- tidyr::tibble(
  num = 1L:12L, 
  bulan = month.abb, 
  dcount = dcount,
  kumulatif = cumsum(dcount)
)

event <- inner_join(event, event_num, by = "bulan")

event <- event %>% 
  select(tanggal, num, dcount, kumulatif, X3) %>% 
  rename(bulan = num, perayaan = X3) %>% 
  mutate(hari = kumulatif-dcount+tanggal) %>% 
  select(hari, tanggal, perayaan) %>% 
  arrange(hari)

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
  orientation = "lanscape",
  papersize = "A4",
  pdf = TRUE
)
