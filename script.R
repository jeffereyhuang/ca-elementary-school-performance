library(tidyverse)
library(readxl)
library(janitor)

sat18 <- read_xls("sat18.xls") %>% 
  clean_names() %>% 
  select(-year)

sat17 <- read_xls("sat17.xls") %>% 
  clean_names()

sat16 <- read_xls("sat16.xls") %>% 
  clean_names() %>% 
  select(-year)

sat15 <- read_xls("sat15.xls") %>% 
  clean_names()

full_sat <- bind_rows(sat15, sat16, sat17, sat18, .id="year") %>% 
  drop_na(sname)

act18 <- read_xls("act18.xls") %>% 
  clean_names() %>% 
  mutate_at(9:14, as.integer) %>% 
  select(-year)

act17 <- read_xls("act17.xls") %>% 
  clean_names() %>% 
  mutate_at(9:14, as.integer)

act16 <- read_xls("act16.xls") %>% 
  clean_names() %>% 
  mutate_at(6:11, as.integer)

act15 <- read_xls("act15.xls") %>% 
  clean_names() %>% 
  mutate_at(6:11, as.integer) %>% 
  select(-year)

full_act <- bind_rows(act15, act16, act17, act18, .id="year") %>% 
  drop_na(sname)

ap18 <- read_xls("ap18.xls") %>% 
  clean_names() %>% 
  select(-year) %>% 
  mutate_at(9:16, as.integer)

ap17 <- read_xls("ap17.xls") %>% 
  clean_names() %>% 
  mutate_at(9:16, as.integer)

ap16 <- read_xls("ap16.xls") %>% 
  clean_names() %>% 
  select(-year) %>% 
  mutate_at(6:13, as.integer)
ap15 <- read_xls("ap15.xls") %>% 
  clean_names() %>% 
  select(-year) %>% 
  mutate_at(6:13, as.integer)

full_ap <- bind_rows(ap15, ap16, ap17, ap18, .id="year") %>% 
  drop_na(sname)

expense18 <- read_xlsx("expense18.xlsx",
                       skip = 10) %>% 
  mutate(CDS = as.character(CDS))
expense17 <- read_xlsx("expense17.xlsx",
                       skip = 8)
expense16 <- read_xls("expense16.xls", 
                      skip = 7)
expense15 <- read_xls("expense15.xls",
                      skip = 7)

full_expense <- bind_rows(expense15, expense16, expense17, expense18, .id="year")


# dist_seg <- read_xlsx("district_seg.xlsx") %>% 
#   filter(str_detect(metroname, "CA")) %>% 
#   group_by(metroname) %>%
#   summarize() %>%
#   View()

# natl_seg <- read_xlsx("national-school-segregation-web.xlsx") %>% 
#   clean_names() %>% 
#   # filter(str_detect(metro_area, "CA")) %>% 
#   # group_by(metro_area) %>% 
#   # summarize() %>% 
#   # View()



