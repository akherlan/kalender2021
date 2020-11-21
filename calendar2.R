library("rvest")
library("dplyr")

# harvesting Indonesia's holiday data
page_event <- "https://www.liburnasional.com/kalender-lengkap-2021/" %>% 
  read_html()

table <- page_event %>% 
  html_nodes(".libnas-calendar-full-detail") %>% 
  html_table() %>% 
  bind_rows() %>% 
  select(X1, X3) %>% 
  as_tibble()

# mapping the holidays' day
event <- rbind(table, table[rep(8,1),])
event[8,1] <- "15 Mei"
event[16,1] <- "16 Mei"

event$X1 <- paste0(event$X1, " 21") %>% 
  as.Date("%d %b %y")

event <- event %>% 
  rename("tanggal" = "X1", "peringatan" = "X3") %>% 
  mutate(hari = julian.Date(.$tanggal, origin = as.Date("2020-12-31"))) %>% 
  arrange(by = tanggal)

tanggal <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "1 day")
holiday <- ifelse(format(tanggal, "%w") %in% c(6, 0), "Weekend", NA)
holiday[event$hari] <- "Libur Nasional" 

# create calendar
library("calendR")
library("gridExtra")

kalender <- calendR(
  year = 2021, 
  start = "M", 
  special.days = holiday, 
  special.col = c("#37b24d", "#b2f2bb"), # holidays col
  days.col = 1,                          # days text col
  months.col = "white",                  # months text col
  mbg.col = "#2e3440",                   # months bg col
  bg.col = "#f4f4f4",                    # bg col
  lty = 0,
  title = "KALENDER 2021",
  orientation = "l"
)

tblplot <- bind_cols(table[1:3,],table[4:6,],table[7:9,],table[10:12,],table[13:15,]) %>% 
  `names<-`(paste0("x", 1:10))

keterangan <- tableGrob(
  d = tblplot, rows = NULL, cols = NULL,
  theme = ttheme_minimal(
    base_size = 8, 
    base_colour = "grey30"
  )
)

doc <- grid.arrange(kalender, keterangan, nrow = 2, heights = c(6,1))

ggplot2::ggsave(
  file = "Calendar_2021.pdf", plot = doc,
  width = 11.69, height = 8.27, # A4 landscape
  units = "in", dpi = 300
)
