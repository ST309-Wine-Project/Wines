# TO-DO
"
1. Implement 'type' for white wines
2. Implement 'type' for roses
3. -''- 'type' for any other type of drink ('sherry', 'cava')
4. CLEAN OUTLIERS
    a. by vintage (should be more under control)
    b. by price
    (c. by point)
5. DONE {xxx Sort out text fuck-ups (DECIDE IF WE WANT TO) xxx}
6. Look into the missing taster_name

"

### libraries ###

library(tidyverse)
library(stringr)
library(stringi) #string functionality for wrapper in rvest
library(rvest) #for repair_encoding(), also a package for webscraping


### required data loading ###

wines0 <- read.csv("winemag-data-130k-v2.csv") # source: https://www.kaggle.com/zynicide/wine-reviews

types <- read.csv("./Wine Varieties by Type/red_types.csv", header = T) # source: https://en.wikipedia.org/wiki/List_of_grape_varieties

### data prepping ###
wines <- wines0 %>%
  mutate(vintage = as.numeric(str_match(title, "[2|1][0-9]{3}")))

varieties <- data.frame(levels(wines$variety))
names(varieties) <- c("variety")

varieties <- varieties %>%
  
  #detect if a blend
  mutate(blend = str_detect(variety, "-") | str_detect(variety, "(?i)blend")) %>%
  
  #search type explicitly 
  mutate(type = tolower(str_match(variety, "(?i)white"))) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)red")), type)) %>%
  
  #search by membership in type-vector (may be redundant)
  mutate(type = ifelse((is.na(type) & variety %in% types$variety[types$type == "red"]), "red", type)) %>%
  mutate(type = ifelse((is.na(type) & variety %in% types$variety[types$type == "white"]), "white", type)) # to be implemented 
  
  #search by text to capture blends, e.g. "cabernet sauvignon-syrah"
  mutate(type = ifelse((is.na(type) & str_detect(variety, regex(paste(types$variety[types$type == "red"], collapse = "|"), ignore_case = T))), "red", type)) #%>%
  mutate(type = ifelse((is.na(type) & str_detect(variety, regex(paste(types$variety[types$type == "white"], collapse = "|"), ignore_case = T))), "white", type))

#use the varieties df to incorporate type into 'wines'
wines$blend <- ifelse(wines$variety %in% varieties$variety[varieties$blend == T], T, F)
wines$type <- ifelse(wines$variety %in% varieties$variety[varieties$type == "red"], "red", NA)
wines$type <- ifelse(wines$variety %in% varieties$variety[varieties$type == "white"], "white", NA)


### fix encoding errors ###

## taster_name ##

levels(wines$taster_name)[levels(wines$taster_name) == "Kerin O????TKeefe"] = "Kerin O'Keefe" #fix by: ????T

## variety ##
variety_old = varieties$variety
variety_fix = repair_encoding(variety_old)
index = variety_old != variety_fix
variety_fix = data.frame(variety_old[index], variety_fix[index], rep(F, sum(index)))
names(variety_fix) = c("original", "fixed", "manual")

#View(variety_fix) # identify manual errors
variety_fix[8,3] = T ; variety_fix[10,3] = T
variety_fix[24,3] = T ; variety_fix[63,3] = T
#~View(data.frame(variety_fix$original[variety_fix$manual], variety_fix$fix[variety_fix$manual]))

variety_fix$fixed = as.character(variety_fix$fixed)
variety_fix[8,2] = "Babic"
variety_fix[10,2] = "Bogazkere"
variety_fix[24,2] = "Feteasca Regala"
variety_fix[63,2] = "Tamaioasa Romaneasca"
variety_fix$fixed = as.factor(variety_fix$fixed)
#~View(data.frame(variety_fix$original[variety_fix$manual], variety_fix$fix[variety_fix$manual]))

varieties$variety = as.character(varieties$variety)
varieties$variety[index] = as.character(variety_fix$fixed)
varieties$variety = as.factor(varieties$variety)

levels(wines$variety) = varieties$variety

## designation, title, province, region_1, region_2, winery ##

wines$designation = repair_encoding(wines$designation)
wines$title = repair_encoding(wines$title)
wines$province = repair_encoding(wines$province) 
wines$region_1 = repair_encoding(wines$region_1)
wines$region_2 = repair_encoding(wines$region_2)
wines$winery = repair_encoding(wines$winery)


#write.csv(wines, "wine-data-tidied.csv")