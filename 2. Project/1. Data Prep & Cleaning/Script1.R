# TO-DO
"
1. DONE {xxx Implement 'type' for white wines xxx}
2. Implement 'type' for roses (don't know if there are any?)
3. DONE {xxx -''- 'type' for any other type of drink ('champagne', 'sherry', 'cava') xxx}
    a. DONE {xxx Automatic matching xxx}
    b. DONE Manual matching
    c. DONE fix port text matching (would match 'portuguese' rn)
4. !!TODO!!, CLEAN OUTLIERS
    a. by vintage (should be more under control)
    b. by price
    (c. by point)
5. DONE {xxx Sort out text fuck-ups (DECIDE IF WE WANT TO) xxx}
6. Look into the missing taster_name

"

### libraries ###

library(tidyverse)
library(forcats)
library(stringr)
library(stringi) #string functionality for wrapper in rvest
library(rvest) #for repair_encoding(), also a package for webscraping


### required data loading ###

wines0 <- read_csv("winemag-data-130k-v2.csv") %>% select(-X1) # source: https://www.kaggle.com/zynicide/wine-reviews

types <- read_csv("./Wine Varieties by Type/red_types.csv") # source: https://en.wikipedia.org/wiki/List_of_grape_varieties
white_types <- read_csv("./Wine Varieties by Type/white_types.csv") # source: https://en.wikipedia.org/wiki/List_of_grape_varieties
misc_types <- read_csv("./Wine Varieties by Type/misc_types.csv")
types <- bind_rows(types, white_types, misc_types)
rm(white_types, misc_types)

### data prepping ###
wines <- wines0 %>%
  mutate(vintage = as.numeric(str_match(title, "[2|1][0-9]{3}")))

varieties <- distinct(wines, variety)

varieties <- varieties %>%
  
  #detect if a blend
  mutate(blend = str_detect(variety, "-") | str_detect(variety, "(?i)blend")) %>%
  
  #search type explicitly 
  mutate(type = tolower(str_match(variety, "(?i)white"))) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)\\bred\\b")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)\\bchampagne\\b")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)\\bsherry\\b")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)\\bprosecco\\b")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)\\bsparkling\\b")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)\\bmadeira\\b")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)\\bport\\b")), type)) %>%
  
  #search by membership in type-vector (may be redundant)
  mutate(type = ifelse((is.na(type) & variety %in% types$variety[types$type == "red"]), "red", type)) %>%
  mutate(type = ifelse((is.na(type) & variety %in% types$variety[types$type == "white"]), "white", type)) %>% 
  
  #search by text to capture blends, e.g. "cabernet sauvignon-syrah"
  mutate(type = ifelse((is.na(type) & str_detect(variety, regex(paste(types$variety[types$type == "red"], collapse = "|"), ignore_case = T))), "red", type)) %>%
  mutate(type = ifelse((is.na(type) & str_detect(variety, regex(paste(types$variety[types$type == "white"], collapse = "|"), ignore_case = T))), "white", type))

varieties$type[455,] = "rosé"
varieties$type[492,] = "rosé"
varieties$type[varieties$variety == "Apple"] = "apple ice wine"
varieties$blend[str_detect(varieties$variety, "Thurgau")] = F

# create file for checking leftover types
#types_misc <- data.frame(varieties$variety[is.na(varieties$type)], repair_encoding(varieties$variety[is.na(varieties$type)]), varieties$type[is.na(varieties$type)])
#names(types_misc) = c("variety", "variety_fix", "type")
#write.csv(types_misc, "types_misc.csv")

#use the varieties df to incorporate type into 'wines'
wines <- wines %>%
  left_join(varieties)

### fix encoding errors ###

## taster_name ##

wines <- mutate(wines, taster_name = ifelse(str_detect(taster_name, "Keefe"), "Kerin O'Keefe", taster_name))

## variety ##
#variety_old = varieties$variety
#variety_fix = repair_encoding(variety_old)
#variety_fix_df <- tibble(old = variety_old, fix = variety_fix)
#manual_fix <- filter(df, old != fix)
#manual_fix$fix <- c(
#  "Feteasca Regala",
#  "Calkarasi", # need fixing
#  "Babic",
#  "Tamaioasa Romaneasca",
#  "Bogazkere"
#)

#variety_fix_df <- variety_fix_df %>%
#  anti_join(manual_fix, by = "old") %>%
#  bind_rows(manual_fix)

#wines <- wines %>%
#  mutate(variety = ifelse(variety == old, fix, variety))
  
## designation, title, province, region_1, region_2, winery ##

wines$designation = repair_encoding(wines$designation)
wines$title = repair_encoding(wines$title)
wines$province = repair_encoding(wines$province) 
wines$region_1 = repair_encoding(wines$region_1)
wines$region_2 = repair_encoding(wines$region_2)
wines$winery = repair_encoding(wines$winery)
wines$description = repair_encoding(wines$description)

# count NAs
sum(is.na(wines$type))

write.csv(wines, "wine-data-tidied.csv")
