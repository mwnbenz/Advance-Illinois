# prepping data for EPP distances tableau

library("tidyverse")
library("readxl")
library("sf")
library("geosphere")

setwd("/Users/mercedeswentworth-nice/Documents/Employment/Advance Illinois/")

dist <- function(a, b, c, d) {
  return(as.numeric(distm(c(a, b), c(c, d), fun = distHaversine)) * 0.000621371)
}

districts<- read_sf("EDGE_SCHOOLDISTRICT_TL19_SY1819/schooldistrict_sy1819_tl19.shp") %>%
  filter(STATEFP == "17") %>%
  mutate(dummy = 1)
  
epps <- read_excel("Illinois.xls", sheet = 2)
ipeds_epp_crosswalk <- read_excel("IPEDS_Crosswalk.xls")
ipeds <- read.csv("hd2019.csv")

epps_by_subject <- read_excel("Illinois.xls", sheet = 6) %>%
  mutate(ProgramCode = as.numeric(ProgramCode)) %>%
  mutate(dummy = 1) %>%
  pivot_wider(id_cols = c("ProgramCode","ProgramType", "State"), 
              names_from = Category, values_from = dummy)

names(epps_by_subject) <- gsub("Teacher Education - ", "", names(epps_by_subject))

epps_by_subject <- epps_by_subject %>%
  mutate(science = coalesce(Physics, `Earth Science`, Psychology, Geography, 
                            `Science Teacher Education/General Science`, Biology),
         foreign_language = coalesce(Latin, German, French, Spanish),
         elementary = `Elementary Education`,
         middle = `Junior High/Intermediate/Middle School Education`,
         secondary = `Secondary Education`,
         arts = Art,
         lang_arts = `English/Language Arts`,
         reading = Reading,
         music = Music,
         social_studies = coalesce(`Social Science`, `Social Studies`, `History`),
         technology_cs = coalesce(`Technology Teacher Education/Industrial Arts`),
         family_consumer_science = `Family and Consumer Sciences/Home Economics`
         pe = `Physical Education and Coaching`
         )

df <- epps %>%
  mutate(ProgramCode = as.numeric(ProgramCode)) %>%
  left_join(ipeds_epp_crosswalk %>%
              mutate(InstCode = as.numeric(InstCode)), 
            by = c("ProgramCode" = "InstCode", "ProgramType", "State")) %>%
  left_join(ipeds, by = c("IPEDS" = "UNITID")) %>%
  mutate(dummy = 1) %>%
  left_join(districts, by = "dummy")

for (i in c(1:nrow(df))) {
  df$distCoords[i] = dist(as.numeric(df$INTPTLON[i]), 
                          as.numeric(df$INTPTLAT[i]), 
                          as.numeric(df$LONGITUD[i]), 
                          as.numeric(df$LATITUDE[i]))
}

df <- df %>%
  left_join(epps_by_subject, by = c("ProgramCode", "ProgramType", "State"))

st_write(df, "/Users/mercedeswentworth-nice/Documents/Employment/Advance Illinois/dist_epp_schools.shp", delete_dsn = TRUE)
