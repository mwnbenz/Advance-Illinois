# recreating the UIC Great Cities map 
# from this article:
#

library("ipumsr")
library("tidyverse")
library("sf")

setwd("/Users/mercedeswentworth-nice/Documents/Employment/Advance Illinois/CPS transportation project/")

ddi <- read_ipums_ddi("usa_00005.xml")
data <- read_ipums_micro(ddi)

pumas <- read_sf("ipums_puma_2010/ipums_puma_2010.shp") %>%
  mutate(PUMA = as.numeric(PUMA)) %>%
  filter(State == "Illinois")
communities <- read_sf("Boundaries - Community Areas (current)/geo_export_15ff3967-76e0-41d1-9dfc-0d3eb3c3d8a9.shp")
p2c <- read.csv("puma_to_community_area.csv") %>% 
  mutate(Community=strsplit(gsub(".*--", "", Community), ",")) %>% 
  unnest(Community) %>%
  mutate(Community=strsplit(Community, "&")) %>% 
  unnest(Community) %>%
  mutate(Community=strsplit(Community, "/")) %>% 
  unnest(Community) %>%
  mutate(Community = toupper(trimws(gsub("PUMA", "", Community))),
         PUMA = as.numeric(gsub(".* ", "", PUMA))) %>%
  mutate(Community = case_when(Community == "EAST" ~ "EAST GARFIELD PARK",
                               Community == "NORTH" ~ "NORTH LAWNDALE",
                               TRUE ~ Community)) %>%
  rbind(data.frame(PUMA=3532, Community="CALUMET HEIGHTS")) %>%
  rbind(data.frame(PUMA=3532, Community="RIVERDALE")) %>%
  rbind(data.frame(PUMA=3532, Community="HEGEWISCH")) %>%
  rbind(data.frame(PUMA=3530, Community="MOUNT GREENWOOD")) %>%
  rbind(data.frame(PUMA=3529, Community="WASHINGTON PARK")) %>%
  rbind(data.frame(PUMA=3529, Community="KENWOOD")) %>%
  rbind(data.frame(PUMA=3529, Community="OAKLAND")) %>%
  rbind(data.frame(PUMA=3529, Community="FULLER PARK")) %>%
  rbind(data.frame(PUMA=3525, Community="ARMOUR SQUARE")) %>%
  rbind(data.frame(PUMA=3526, Community="ARCHER HEIGHTS")) %>%
  rbind(data.frame(PUMA=3527, Community="WEST ELSDON")) %>%
  rbind(data.frame(PUMA=3527, Community="CLEARING")) %>%
  rbind(data.frame(PUMA=3420, Community="EDISON PARK")) %>%
  rbind(data.frame(PUMA=3422, Community="OHARE"))

df <- data %>%
  filter(STATEFIP == 17,
         CITY == 1190,
         GQ == 1,
         HHINCOME != 9999999,
         AGE <= 18,
         AGE >= 5) %>%
  group_by(SERIAL) %>%
  summarise(HHNCHILD = n(), HHINC = max(HHINCOME), POVERTY = max(POVERTY), PUMA = max(PUMA), HHWT = max(HHWT)) %>%
  #summarise(HHNCHILD = n(), HHINC = max(HHINCOME), POVERTY = max(POVERTY), PUMA = max(PUMA), HHWT = max(HHWT)) %>%
  #mutate(pct = (HHNCHILD * .75 * 2 * 180) / HHINC) %>%
  #filter(HHINC > 5) %>%
  #group_by(PUMA) %>%
  #summarise(pct = sum(pct * HHWT, na.rm = T) * 100 / sum(HHWT, na.rm = T), n = n()) %>%
  group_by(PUMA) %>%
  summarise(pct = sum(HHNCHILD * .75 * 2 * 180 * HHWT)/sum(HHINC * HHWT)) %>%
  left_join(p2c, by = "PUMA") %>%
  left_join(communities, by = c("Community" = "community"))

st_write(df, "pct_income_student_cta.shp", delete_dsn = TRUE)

ggplot(data = df) +
  geom_sf(aes(geometry = geometry, fill = pct)) 
