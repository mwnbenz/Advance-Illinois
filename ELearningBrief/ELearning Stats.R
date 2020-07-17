# E-Learning Stats for Advance Illinois

library("tidyverse")
library("ipumsr")
library("sf")

fp = "/Users/mercedeswentworth-nice/Documents/Employment/Advance Illinois/ELearningBrief/"

ddi  <- read_ipums_ddi(paste0(fp, "data/usa_00003.xml"))
ogdf <- read_ipums_micro(ddi)

# using PUMA since county level data is not available
p2c <- read_csv(paste0(fp, "data/puma2countyIL.csv")) %>%
  mutate(PUMA = as.character(puma12))

# For use in mapping the data
puma_geom <- read_sf(paste0(fp, "data/ipums_puma_2010/ipums_puma_2010.shp")) %>%
  mutate(PUMA = as.character(PUMA)) %>%
  filter(STATEFIP == "17")

# initial filters
df <- ogdf %>%
  filter(STATEFIP == 17) %>%
  # households under 1970 definition
  filter(GQ == 1) %>%
  # grades k-12
  filter(GRADEATT %in% c(2:5)) %>%
  filter(AGE < 21)

# dummies
# stats: internet, device, laptop/computer, high speed
# if data is given a missing/unknown code by IPUMS, marked 
# as -9999 for type consistency then converted to NA
df <- df %>%
  mutate(
    ddevice = case_when(CILAPTOP  == 1 | 
                          CISMRTPHN == 1 | 
                          CITABLET  == 1 |
                          CIOTHCOMP == 1 ~ PERWT,
                        CILAPTOP  == 2 & 
                          CISMRTPHN == 2 & 
                          CITABLET  == 2 &
                          CIOTHCOMP == 2 ~ 0,
                        TRUE ~ -9999),
    dcomputer = recode(CILAPTOP, 
                       '0' = -9999, 
                       '1' = PERWT, 
                       '2' = 0),
    dinternet = recode(CINETHH, 
                       '0' = -9999, 
                       '1' = PERWT, 
                       '2' = 0, 
                       '3' = 0),
    dhispeed = recode(CIHISPEED, 
                      '0'  = -9999, 
                      '10' = PERWT, 
                      '20' = 0)
  ) %>%
  mutate(ddevice   = na_if(ddevice, -9999),
         dcomputer = na_if(dcomputer, -9999),
         dinternet = na_if(dinternet, -9999),
         dhispeed  = na_if(dhispeed, -9999))

# all of IL -- used these lines to test different filters to see if I could replicate the
# edweek calculator
df %>%
  distinct(HHWT, .keep_all = TRUE) %>%
  summarise(internet = sum(dinternet, na.rm= TRUE)/sum(PERWT, na.rm= TRUE), 
            hispeed  = sum(dhispeed, na.rm= TRUE)/sum(PERWT, na.rm= TRUE),
            device   = sum(ddevice, na.rm= TRUE)/sum(PERWT, na.rm= TRUE),
            computer = sum(dcomputer, na.rm= TRUE)/sum(PERWT, na.rm= TRUE)
  )

# summarize by PUMA and add geometry
pumas <- df %>%
  group_by(PUMA) %>%
  summarise(internet = sum(dinternet, na.rm= TRUE)/sum(PERWT, na.rm= TRUE), 
            hispeed  = sum(dhispeed, na.rm= TRUE)/sum(PERWT, na.rm= TRUE),
            device   = sum(ddevice, na.rm= TRUE)/sum(PERWT, na.rm= TRUE),
            computer = sum(dcomputer, na.rm= TRUE)/sum(PERWT, na.rm= TRUE)
            ) %>%
  left_join(p2c, by="PUMA") %>%
  left_join(puma_geom, by = "PUMA")

readr::write_csv(pumas, paste0(fp, "out/digital_access_by_PUMA.csv"))
st_write(pumas, paste0(fp, "out/digital_access_by_PUMA.shp"), delete_dsn = TRUE)
st_write(pumas, "/Users/mercedeswentworth-nice/Documents/Employment/Advance Illinois/digital_access_by_PUMA.shp", delete_dsn = TRUE)
