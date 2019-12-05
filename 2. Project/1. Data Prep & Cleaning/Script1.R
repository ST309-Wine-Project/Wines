# TO-DO
"
1. DONE {xxx Implement 'type' for white wines xxx}
2. Implement 'type' for roses (don't know if there are any?)
3. DONE {xxx -''- 'type' for any other type of drink ('champagne', 'sherry', 'cava') xxx}
    a. DONE {xxx Automatic matching xxx}
    b. Manual matching
    c. fix port text matching (would match 'portuguese' rn)
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

#wines0 <- read.csv("winemag-data-130k-v2.csv") # source: https://www.kaggle.com/zynicide/wine-reviews

types <- read.csv("./Wine Varieties by Type/red_types.csv", header = T) # source: https://en.wikipedia.org/wiki/List_of_grape_varieties
white_types <- read.csv("./Wine Varieties by Type/white_types.csv", header = T) # source: https://en.wikipedia.org/wiki/List_of_grape_varieties
names(white_types) <- c("variety", "type")
misc_types <- read.csv("./Wine Varieties by Type/misc_types.csv", header = T)
names(misc_types) <- c("variety", "type")
levels(misc_types$type) # check for misspellings
types <- rbind(types, white_types, misc_types)

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
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)champagne")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)sherry")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)prosecco")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)sparkling")), type)) %>%
  mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)madeira")), type)) %>%
  #mutate(type = ifelse(is.na(type), tolower(str_match(variety, "(?i)port")), type)) %>% #will match with "portuguese"
  
  #search by membership in type-vector (may be redundant)
  mutate(type = ifelse((is.na(type) & variety %in% types$variety[types$type == "red"]), "red", type)) %>%
  mutate(type = ifelse((is.na(type) & variety %in% types$variety[types$type == "white"]), "white", type)) %>% 
  
  #search by text to capture blends, e.g. "cabernet sauvignon-syrah"
  mutate(type = ifelse((is.na(type) & str_detect(variety, regex(paste(types$variety[types$type == "red"], collapse = "|"), ignore_case = T))), "red", type)) %>%
  mutate(type = ifelse((is.na(type) & str_detect(variety, regex(paste(types$variety[types$type == "white"], collapse = "|"), ignore_case = T))), "white", type))

varieties$type[varieties$variety == "Apple"] = "apple ice wine"
varieties$blend[str_detect(varieties$variety, "Thurgau")] = F

# create file for checking leftover types
#types_misc <- data.frame(varieties$variety[is.na(varieties$type)], repair_encoding(varieties$variety[is.na(varieties$type)]), varieties$type[is.na(varieties$type)])
#names(types_misc) = c("variety", "variety_fix", "type")
#write.csv(types_misc, "types_misc.csv")

#use the varieties df to incorporate type into 'wines'
wines$blend <- ifelse(wines$variety %in% varieties$variety[varieties$blend == T], T, F)
wines$type <- rep(NA, nrow(wines))

for(type in levels(as.factor(varieties$type))) {
  index = is.na(wines$type) & (wines$variety %in% varieties$variety[varieties$type == type])
  wines$type[index] <- rep(type, sum(index, na.rm = T))
}

### fix encoding errors ###

## taster_name ##

levels(wines$taster_name)[levels(wines$taster_name) == "Kerin Oâ???TKeefe"] = "Kerin O'Keefe" #fix by: ????T

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

# count NAs
sum(is.na(wines$type))

#write.csv(wines, "wine-data-tidied.csv")