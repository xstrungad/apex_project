#turn off warnings
options(warn=-1)

# Instalation requirements from local deposit
# shiny_1.5.0
# shinyjs_0.1.0
# shiny.router_0.1.1

#import libraries
library(shiny)
library(shiny.router)
library(shinyjs)
library(DBI)
library(RMySQL)
library(data.table)
library(dplyr)
library(DT)
library(openxlsx)
library(leaflet)
library(stringr)
library(magrittr)
library(mongolite)
library(jsonlite)
library(tictoc)
#library(fastmap)

# Vytvorenie spojeni k databazam MONGO
conLacod <- mongo(url = "mongodb://localhost:27017/", collection = "dct_analysis", db = "lacod")
conSusdat <- mongo(url = "mongodb://localhost:27017/", collection = "susdat", db = "susdat")
conEcotox <- mongo(url = "mongodb://localhost:27017/", collection = "lowestpnec", db = "ecotox")
conFactsheets <- mongo(url = "mongodb://localhost:27017/", collection = "factsheets_coll", db = "factsheets")
# Vytvorenie spojeni k databazam 
conDct <- mongo(url = "mongodb://localhost:27017/", collection = "dct_analysis", db = "data")
dct_analysis <- conDct$find()
dct_analysis <- as.data.frame(dct_analysis)
dct_analysis <- relocate(dct_analysis, "basin_name", .after= "am_loq")
fwrite(dct_analysis, file= "dct_data.csv", sep=";")
conDct$disconnect()
# Vyber krajiny z udajov
countryArray <- (dct_analysis$country)

# Iba jedinecne krajiny
countryUnique <- unique(countryArray)
countryArray <- as.data.frame(countryArray)
countryUnique <- as.data.frame(countryUnique)
names(countryArray) <- "country"
names(countryUnique) <- "country"
# Vyber zluceniny z udajov
substancesArray <- (dct_analysis$sus_id)
# Iba jedinecne zluceniny
substancesUnique <- unique(substancesArray)
substancesArray <- as.data.frame(substancesArray)
substancesUnique <- as.data.frame(substancesUnique)
names(substancesArray) <- "sus_id"
names(substancesUnique) <- "sus_id"
# Generovanie udajov pre vyberovy prvok Marine biota PNECs
marine_biota_pnecs_id <- c(1, 2, 3) 
marine_biota_pnecs_title <- c("PNECbio_marine", "Same as PNECbio_fw", "PNECbio_marine/4 (molluscs) and PNECbio_marine (all other species)") 
marine_biota_pnecs <- data.frame(marine_biota_pnecs_id, marine_biota_pnecs_title)

# Vytvorenie asociativneho pola poloziek pre vyberovy formularovy prvok
choices_marine_biota_pnecs <- setNames(marine_biota_pnecs$marine_biota_pnecs_id, marine_biota_pnecs$marine_biota_pnecs_title)

# Vyber zoznamu uploadovanych suborov z tabulky empodat.dct_list
conLacod_list <- mongo(url = "mongodb://localhost:27017/", collection = "dct_list", db = "lacod")
# Vyber zoznamu uploadovanych suborov z tabulky empodat.dct_list
files <- conLacod_list$find(query = '{"list_deleted": {"$ne": true}}', fields = '{"_id": 0, "list_name": 1, "list_analysis_from": 1, "list_analysis_to": 1}', sort = '{"list_id": -1}')

# Vytvorenie asociativneho pola poloziek pre vyberovy formularovy prvok
choices_files <- setNames(paste(files$list_analysis_from, files$list_analysis_to, sep = ":"), files$list_name)
conLacod_list$disconnect()

# Vyber skratky a nazvu krajiny z tabulky lacod.data_country
conLacod_country <- mongo(url = "mongodb://localhost:27017/", collection = "data_country", db = "lacod")
countries <- conLacod_country$find(fields = '{"_id": 0, "country": 1, "country_name": 1}')
# Vytvorenie asociativneho pola poloziek pre vyberovy formularovy prvok
#countries <- inner_join(countries, countryUnique, by = c("country" = "country"))
countries <- merge(countries, countryUnique, by.c = "country", by.c = "country")
choices_countries <- setNames(countries$country, countries$country_name)
conLacod_country$disconnect()
## Vyber ID a nazvu matice 
# Zoznam matic pre vyberovy prvok matice
matrice_id = c(1,2,3,4,5) 
matrice_title = c("All", "Biota - Fresh water", "Biota - Marine water", "Biota - Terrestrial", "Biota - Other") 
matrices = data.frame(matrice_id, matrice_title) 
# Vytvorenie asociativneho pola poloziek pre vyberovy formularovy prvok
choices_matrices = setNames(matrices$matrice_id, matrices$matrice_title)
matricesBasin = matrices

# Zoznam zdrojov
data_source_id = c(1,2,3,4) 
data_source_title = c("Apex", "Connect", "UBA-HELCOM", "Pre-EMPT") 
data_sources = data.frame(data_source_id, data_source_title) 
# Vytvorenie asociativneho pola poloziek pre vyberovy formularovy prvok
choices_data_sources = setNames(data_sources$data_source_id, data_sources$data_source_title)

# Generovanie udajov pre vyberovy prvok Basin name
basins_id = c(1, 2, 3, 4, 5, 6, 7) 
basins_title = c("Danube", "Rhine", "Elbe", "Baltic Sea", "Black Sea", "Mediterranean Sea", "North Sea") 
basins = data.frame(basins_id, basins_title)
# Vytvorenie asociativneho pola poloziek pre vyberovy formularovy prvok
choices_basins = setNames(basins$basins_id, basins$basins_title)

# Vyber ID, nazvu a CAS zluceniny z tabulky susdat.susdat
substances <- conSusdat$find(query = '{"sus_id": {"$lt": 1000000}}', fields = '{"sus_id": 1, "sus_name": 1, "sus_cas": 1}')

# V niektorych pripadoch potrebujem iba nazov a casno
substances <- merge(substances, substancesUnique, by.c = "sus_id", by.c = "sus_id")
# V niektorych pripadoch potrebujem iba nazov a casno
substancesBase <- substances 
# Vytvorenie asociativneho pola poloziek pre vyberovy formularovy prvok
choices_substances <- setNames(substances$sus_id, substances$sus_name)

# Generovanie udajov pre vyberovy prvok species
conLacod_species <- mongo(url = "mongodb://localhost:27017/", collection = "data_species_group", db = "lacod")
species <- conLacod_species$find()
choices_species <- setNames(species$dsgr_id, species$dsgr_name)
conLacod_species$disconnect()
# Generovanie udajov pre vyberovy prvok tissue
conLacod_tiss <- mongo(url = "mongodb://localhost:27017/", collection = "data_tissue_element", db = "lacod")
tissue <- conLacod_tiss$find()
choices_tissue <- setNames(tissue$dtiel_id, tissue$dtiel_name)
conLacod_tiss$disconnect()

# Vyber ID, lowestPNEC zluceniny z tabulky ecotox.lowestpnec - Lowest PNECfw
lowest_pnec_value1 <- conEcotox$find(query = '{ "lowest_matrix": {"$eq": 1}, "lowest_active": {"$eq": 1}, "lowest_pnec_value": {"$gt": 0}}', fields = '{"sus_id": 1, "lowest_pnec_value": 1, "lowest_pnec_type": 1}')
colnames(lowest_pnec_value1) <- c("_id", "sus_id", "lowest_pnec_type1","lowest_pnec_value1")
lowest_pnec_value1 <- as.data.frame(lowest_pnec_value1)
lowest_pnec_value1 <- lowest_pnec_value1[,2:4]
# Vyber ID, lowestPNEC zluceniny z tabulky ecotox.lowestpnec - Lowest PNECmarine
lowest_pnec_value2 <- conEcotox$find(query = '{ "lowest_matrix": {"$eq": 2}, "lowest_active": {"$eq": 1}, "lowest_pnec_value": {"$gt": 0}}', fields = '{"sus_id": 1, "lowest_pnec_value": 1, "lowest_pnec_type": 1}')
colnames(lowest_pnec_value2) <- c("_id", "sus_id", "lowest_pnec_type2","lowest_pnec_value2")
lowest_pnec_value2 <- as.data.frame(lowest_pnec_value2)
lowest_pnec_value2 <- lowest_pnec_value2[,2:4]
# Vyber ID, lowestPNEC zluceniny z tabulky ecotox.lowestpnec - Lowest PNECbio_fw
lowest_pnec_value4 <- conEcotox$find(query = '{ "lowest_matrix": {"$eq": 4}, "lowest_active": {"$eq": 1}, "lowest_pnec_value": {"$gt": 0}}', fields = '{"sus_id": 1, "lowest_pnec_value": 1, "lowest_pnec_type": 1}')
colnames(lowest_pnec_value4) <- c("_id", "sus_id", "lowest_pnec_type4","lowest_pnec_value4")
lowest_pnec_value4 <- as.data.frame(lowest_pnec_value4)
lowest_pnec_value4 <- lowest_pnec_value4[,2:4]
# Vyber ID, lowestPNEC zluceniny z tabulky ecotox.lowestpnec - Lowest PNECbio_marine
lowest_pnec_value6 <- conEcotox$find(query = '{ "lowest_matrix": {"$eq": 6}, "lowest_active": {"$eq": 1}, "lowest_pnec_value": {"$gt": 0}}', fields = '{"sus_id": 1, "lowest_pnec_value": 1, "lowest_pnec_type": 1}')
colnames(lowest_pnec_value6) <- c("_id", "sus_id", "lowest_pnec_type6" ,"lowest_pnec_value6")
lowest_pnec_value6 <- as.data.frame(lowest_pnec_value6)
lowest_pnec_value6 <- lowest_pnec_value6[,2:4]
# Vyber BCF koeficientov z tabulky susdat.susdat_usepa
conSusdat_usepa <- mongo(url = "mongodb://localhost:27017/", collection = "susdat_usepa", db = "susdat")
pipeline <- '[{"$project": {"sus_id": 1, "BCF": {"$cond": {"if": { "$gt": ["$usepa_BCF_experimental", null] }, "then": "$usepa_BCF_experimental", "else": "$usepa_BCF_predicted"}}}}]'
bcf <- conSusdat_usepa$aggregate(pipeline)
bcf <- as.data.frame(bcf)
bcf <- bcf[,2:3]
## Lowest PNECs zalozene na PNECbiofw a ak nie je k dispozicii, potom na vypocitanom PNECbio_fw
# - PNECbiofw
lowest_pnec_value_bio <- lowest_pnec_value4
# - PNECbio_fw
lowest_pnec_value_fw_bio <- left_join(lowest_pnec_value1, bcf, by = c("sus_id" = "sus_id")) # pridanie BCF do PNECfw
# -- lowest PNECbio_fw = Lowest PNECfw * BCF
lowest_pnec_value_fw_bio[,3] <- lowest_pnec_value_fw_bio[,3] * lowest_pnec_value_fw_bio[,4] # vypocitanie PNECbio_fw
lowest_pnec_value_fw_bio[,4] <- NULL # odstranenie nepotrebneho BCF
# -- Spojenie PNECbiofw a PNECbio_fw do jedneho pre existujuce zluceniny
lowest_pnec_value_bio_fw <- substances[, c(1, 3:4)] # Vyber 2 stlpcov z matice substance
colnames(lowest_pnec_value_bio_fw)[3] <- "matrix" # premenovanie 2. stlpca na "matrix"
lowest_pnec_value_bio_fw[,3] <- 1 # na zaciatku bude matica 1 - Biota na zaklade freshwater
lowest_pnec_value_bio_fw <- full_join(lowest_pnec_value_bio_fw, lowest_pnec_value_fw_bio, by = c("sus_id" = "sus_id")) # pridanie PNECbio_fw
lowest_pnec_value_bio_fw <- full_join(lowest_pnec_value_bio_fw, lowest_pnec_value_bio, by = c("sus_id" = "sus_id")) # pridanie PNECbio
# --- Ak existuje PNECbiofw pouzi ju inak PNECbio_fw
lowest_pnec_value_bio_fw <- lowest_pnec_value_bio_fw %>% 
  mutate(lowest_pnec_value = ifelse(is.na(lowest_pnec_value4), lowest_pnec_value1, lowest_pnec_value4)) %>% 
  mutate(lowest_pnec_type = ifelse(is.na(lowest_pnec_value4), lowest_pnec_type1, lowest_pnec_type4))
lowest_pnec_value_bio_fw[,4:6] <- NULL # odstran lowest_pnec_value1, lowest_pnec_value4, lowest_pnec_type1, lowest_pnec_type4
###+++##
## Lowest PNECs pre maticu BIOTA
lowest_pnec_value <- lowest_pnec_value_bio_fw

## zlucenina aj s Lowest PNECs
lowest_pnec_value <- lowest_pnec_value[, c(1, 3:6)]
substances <- left_join(substances, lowest_pnec_value, by = c("sus_id" = "sus_id"))
substances <- substances[, c(1, 3:8)]
#names(sus_name) <- "sus_name"
substances[,4] <- 1 # na zaciatku bude matica - Biota - Fresh water
###+++
# Vyber "Marine biota PNECs" 
# 1 - PNECbio_marine (default) â€“ pocitany ako PNEC_marine*BCF (vid Ecotox module); je to vo vacsine pripadov PNECbio/10.
# - PNECmarine * BCF >> PNECmarine_bio
lowest_pnec_value_marine_bio <- left_join(lowest_pnec_value2, bcf, by = c("sus_id" = "sus_id")) # pridanie BCF do PNECmarine
lowest_pnec_value_marine_bio[,3] <- lowest_pnec_value_marine_bio[,3]*lowest_pnec_value_marine_bio[,4] # vypocitanie PNECbio_marine
lowest_pnec_value_marine_bio[,4] <- NULL # odstranenie BCF
# - PNECbio/10, PNECfw*BCF/10 = PNECbio_fw/10
lowest_pnec_value_marine_bio_fw <- lowest_pnec_value_bio_fw
lowest_pnec_value_marine_bio_fw <- lowest_pnec_value_marine_bio_fw [, c(1:3, 5:6)]
lowest_pnec_value_marine_bio_fw <- lowest_pnec_value_marine_bio_fw [, c(1, 3:5)]
lowest_pnec_value_marine_bio_fw[,3] <- lowest_pnec_value_marine_bio_fw[,3]/10
lowest_pnec_value_marine_bio_fw[,2] <- 2 # na zaciatku bude matica 2 - Biota na zaklade marine
# -- Spojenie PNECmarine_bio a PNECbio/10 a PNECfw*BCF/10 do jedneho pre existujuce zluceniny
lowest_pnec_value_bio_marine_middle <- full_join(lowest_pnec_value_marine_bio_fw, lowest_pnec_value_marine_bio, by = c("sus_id" = "sus_id"))
lowest_pnec_value_bio_marine_middle <- lowest_pnec_value_bio_marine_middle %>% 
  mutate(lowest_pnec_value = ifelse(is.na(lowest_pnec_value2), lowest_pnec_value, lowest_pnec_value2)) %>% 
  mutate(lowest_pnec_type = ifelse(is.na(lowest_pnec_value2), lowest_pnec_type, lowest_pnec_type2))
lowest_pnec_value_bio_marine_middle[,5:6] <- NULL

# -- Spojenie vypocitanych s "dodanymi" udajmi
lowest_pnec_value_bio_marine <- full_join(lowest_pnec_value_bio_marine_middle, lowest_pnec_value6, by = c("sus_id" = "sus_id"))
lowest_pnec_value_bio_marine <- lowest_pnec_value_bio_marine %>% 
  mutate(lowest_pnec_value = ifelse(is.na(lowest_pnec_value6), lowest_pnec_value, lowest_pnec_value6)) %>% 
  mutate(lowest_pnec_type = ifelse(is.na(lowest_pnec_value6), lowest_pnec_type, lowest_pnec_type6))
lowest_pnec_value_bio_marine[,5:6] <- NULL

# 2 - Same as PNECbio_fw (v Ecotox module zmenit PNECbio na PNECbio_fw; pridat stlpec PNECbio_marine)
lowest_pnec_value_bio_marine1 <- lowest_pnec_value_bio_fw
lowest_pnec_value_bio_marine1[,2] <- 2 # matica 2 - Biota na zaklade marinewater

# 3 - PNECbio_marine/4 (molluscs) and PNECbio_marine (all other species)
lowest_pnec_value_bio_marine2 <- lowest_pnec_value_bio_marine

###+++
# Vyber loq_biblio_min z tabulky susdat.susdat_biblio
substances <- substances[, c(1:4, 6:7)]
conSusdat_biblio <- mongo(url = "mongodb://localhost:27017/", collection = "susdat_biblio", db = "susdat")
loq_biblio_min <- conSusdat_biblio$find(fields = '{"_id": 0, "sus_id": 1, "loq_biblio_min": 1}')
loq_biblio_min <- as.data.frame(loq_biblio_min)
substances <- left_join(substances, loq_biblio_min, by = c("sus_id" = "sus_id"))
conSusdat_biblio$disconnect()


# Vyber EDscore a CMRscore z tabulky factsheets.pbmt_select
ED_CMR <- conFactsheets$find( fields = '{"_id": 0,"sus_id": 1, "EDscore": 1, "CMRscore": 1}')
ED_CMR <- as.data.frame(ED_CMR)
names(ED_CMR)[names(ED_CMR) == "EDscore"] <- "ED"
names(ED_CMR)[names(ED_CMR) == "CMRscore"] <- "CMR"
substances <- left_join(substances, ED_CMR, by = c("sus_id" = "sus_id"))
choices_substances = setNames(substances$sus_id, substances$sus_name)

# Vyber P / B/ M/ T/ PBT/ PB / PMT z tabulky factsheets.pbmt_select
PBMT <- conFactsheets$find(fields = '{"_id": 0, "sus_id": 1, "pScore": 1, "bScore": 1, "mScore": 1, "tScore": 1, "pbt": 1, "pb": 1}')
PBMT <- as.data.frame(PBMT)
PBMT <- left_join(substancesUnique, PBMT, by = c("sus_id" = "sus_id"))

# Vyber ExposureScore_Water_KEMI a HazScore_EcoChronic_KEMI z tabulky susdat.susdat
ExposureScoreWaterKEMI <- conSusdat$find(fields = '{"_id": 0, "sus_id": 1, "ExposureScore_Water_KEMI": 1, "HazScore_EcoChronic_KEMI": 1}')
ExposureScoreWaterKEMI <- as.data.frame(ExposureScoreWaterKEMI)

# Vyber hazardScore z tabulky factsheets.pbmt_select
hazardScore <- conFactsheets$find(fields = '{"_id": 0, "sus_id": 1, "hazardScore": 1}')
hazardScore <- as.data.frame(hazardScore)

#basic statistics, select all statistics from DB and create big mark f.e. (1 000 000)

#first stat
conLacod_st1 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics1", db = "lacod")
number_of_analysis <- conLacod_st1$find()
number_of_analysis <- number_of_analysis %>%
  mutate_if(is.numeric, format, big.mark = " ")
conLacod_st1$disconnect()
#second stat
conLacod_st2 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics2", db = "lacod")
number_of_substances <- conLacod_st2$find()
number_of_substances <- number_of_substances %>%
  mutate_if(is.numeric, format, big.mark = " ")
conLacod_st2$disconnect()
# Third stat
conLacod_st3 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics3", db = "lacod")
full_list_of_substances_query <- conLacod_st3$find()
full_list_of_substances <- left_join(full_list_of_substances_query, substances, by = c("sus_id" = "sus_id"))
full_list_of_substances <- full_list_of_substances %>%
  select(sus_name, sus_cas, number) %>%
  mutate_if(is.numeric, format, big.mark = " ") 
colnames(full_list_of_substances) <- c("Substance", "CAS No.", "No. of data") 
conLacod_st3$disconnect()
# Fourth stat
conLacod_st4 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics4", db = "lacod")
data_per_matrix <- conLacod_st4$find()
data_per_matrix <- data_per_matrix %>%
  mutate_if(is.numeric, format, big.mark = " ")
colnames(data_per_matrix) <- c("Ecosystem/Matrix", "No. of data") 
conLacod_st4$disconnect()

# Fifth stat
conLacod_st5 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics5", db = "lacod")
data_per_sub_matrix <- conLacod_st5$find()
data_per_sub_matrix <- data_per_sub_matrix %>%
  mutate_if(is.numeric, format, big.mark = " ")
colnames(data_per_sub_matrix) <- c("Ecosystem/Matrix - Sub-matrix", "No. of data")
conLacod_st5$disconnect()

# Sixth stat
conLacod_st6 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics6", db = "lacod")
data_per_category <- conLacod_st6$find()
data_per_category <- data_per_category %>%
  mutate_if(is.numeric, format, big.mark = " ")
colnames(data_per_category) <- c("Category of data", "No. of data")
conLacod_st6$disconnect()

# Seventh stat
conLacod_st7 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics7", db = "lacod")
data_per_collection <- conLacod_st7$find()
data_per_collection <- data_per_collection %>%
  mutate_if(is.numeric, format, big.mark = " ")
colnames(data_per_category) <- c("Data origin", "No. of data")
conLacod_st7$disconnect()

# Eighth stat
conLacod_st8 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics8", db = "lacod")
data_per_country <- conLacod_st8$find()
data_per_country <- data_per_country %>%
  mutate_if(is.numeric, format, big.mark = " ")
colnames(data_per_country) <- c("Country", "No. of data", "No. of Substances")
conLacod_st8$disconnect()

# Ninth stat
conLacod_st9 <- mongo(url = "mongodb://localhost:27017/", collection = "statistics9", db = "lacod")
data_per_year <- conLacod_st9$find()
data_per_year <- data_per_year %>%
  mutate_if(is.numeric, format, big.mark = " ")
colnames(data_per_year) <- c("Country", "Year", "No. of data")
conLacod_st9$disconnect()

# Close connections
conEcotox$disconnect()
conSusdat$disconnect()
conLacod$disconnect()
conFactsheets$disconnect()
conSusdat_usepa$disconnect()

#search - HTML parts-TEMPLATES
page <- function() {
  htmlTemplate("search.html")
}

#homepage
search <- function() {
  htmlTemplate("search.html")
}

#basic statistics
basic <- function() {
  htmlTemplate("basic.html")
}

#customized statistics
customized <- function() {
  htmlTemplate("customized.html")
}

#outlier
outlier <- function() {
  htmlTemplate("outliers.html")
}

#outlier
maps <- function() {
  htmlTemplate("maps.html")
}

#assign TEMPLATES to variables
root_page <- page()
search <- search()
basic <- basic()
customized <- customized()
outlier <- outlier()
maps <- maps()

#Callbacks on the server side for
basic_callback <- function(input, output, session) {
  
  #render first stat
  output$number_of_analysis <- renderText({
    number_of_analysis$number
  })
  
  #render second stat
  output$number_of_substances <- renderText({
    number_of_substances$number
  })
  
  #render third stat
  output$statistic1 <- DT::renderDataTable(
    full_list_of_substances, server = FALSE,
    class = 'cell-border stripe',
    extensions = c("Buttons"),
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      columnDefs = list(list(className = 'dt-center', targets = 0:3))
    )
  )
  
  #render fourth stat
  output$statistic2 <- DT::renderDataTable(
    data_per_matrix, server = FALSE,
    class = 'cell-border stripe',
    extensions = c("Buttons"),
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      columnDefs = list(list(className = 'dt-center', targets = 0:2))
    )
  )
  
  #render fifth stat
  output$statistic3 <- DT::renderDataTable(
    data_per_sub_matrix, server = FALSE,
    class = 'cell-border stripe',
    extensions = c("Buttons"),
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      columnDefs = list(list(className = 'dt-center', targets = 0:2))
    )
  )
  
  #render sixth stat
  output$statistic4 <- DT::renderDataTable(
    data_per_category, server = FALSE,
    class = 'cell-border stripe',
    extensions = c("Buttons"),
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      columnDefs = list(list(className = 'dt-center', targets = 0:2))
    )
  )
  
  #render seventh stat
  output$statistic5 <- DT::renderDataTable(
    data_per_collection, server = FALSE,
    class = 'cell-border stripe',
    extensions = c("Buttons"),
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      columnDefs = list(list(className = 'dt-center', targets = 0:2))
    )
  )
  
  #render eigth stat
  output$statistic6 <- DT::renderDataTable(
    data_per_country, server = FALSE,
    class = 'cell-border stripe',
    extensions = c("Buttons"),
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      columnDefs = list(list(className = 'dt-center', targets = 0:3))
    )
  )
  
  #render ninth stat
  output$statistic7 <- DT::renderDataTable(
    data_per_year, server = FALSE,
    class = 'cell-border stripe',
    extensions = c("Buttons"),
    options = list(
      dom = 'Bfrtip',
      buttons = c('csv', 'excel'),
      columnDefs = list(list(className = 'dt-center', targets = 0:3))
    )
  )
}
#result1 <- dct_analysis
# Function to establish MongoDB connection
establish_connection <- function() {
  conDct <- mongo(url = "mongodb://localhost:27017/", collection = "dct_analysis", db = "data")
  return(conDct)
}

# Function to retrieve data from MongoDB
result1 <- function(conDct) {
  tryCatch({
    # Attempt to fetch data from MongoDB
    result <- conDct$find()
    result <- as.data.frame(result)
    result <- relocate(result, "basin_name", .after= "am_loq")
    return(result)
  }, error = function(e) {
    # Print error message
    print(paste("Error:", e$message))
    # Wait for some time before attempting to reconnect
    Sys.sleep(5)
    # Attempt to reconnect and retrieve data recursively
    result1(establish_connection())
  })
}

# Main function to initiate data retrieval
main <- function() {
  conDct <- establish_connection()
  result1 <- result1(conDct)
  # Once data is retrieved, you can proceed with further processing
  print("Data retrieval successful.")
}

# Call the main function to start the process
main()

#customized callback
customized_callback <- function(input, output, session) {
  #update select from server side - LOAD option
  updateSelectizeInput(session, 'data_sources', choices = c("All", choices_data_sources), selected = "All", server = TRUE)
  updateSelectizeInput(session, 'files', choices = c("All", choices_files), selected = "All", server = TRUE)
  updateSelectizeInput(session, 'countries', choices = c("All", choices_countries), selected = "All", server = TRUE)
  updateSelectizeInput(session, 'matrices', choices = c(choices_matrices), server = TRUE)
  updateSelectizeInput(session, 'substances', choices = c("All", choices_substances), selected = "All", server = TRUE)
  updateSelectizeInput(session, 'species', choices = c("All", choices_species), selected = "All", server = TRUE)
  updateSelectizeInput(session, 'tissue', choices = c("All", choices_tissue), selected = "All", server = TRUE)		
  updateSelectizeInput(session, 'basins', choices = c("All", choices_basins), selected = "All", server = TRUE)
  updateSelectizeInput(session, 'year', choices = c("All", seq(from = 2003, to = as.integer(format(Sys.Date(), "%Y")), by = 1)), selected = "All", server = TRUE)
  updateSelectizeInput(session, 'marine_biota_pnecs', choices = c(choices_marine_biota_pnecs), selected = "1", server = TRUE)
  
  #after click save(FILTER button)-this action started
  observeEvent(input$save, {
    #JS UI
    shinyjs::disable("save")
    shinyjs::showElement("loading")
    
    result1 <- result1
    #filter dct
    #result1 <- dct_analysis	
    #result1 <- conDct$find()
    #result1 <- as.data.frame(result1)
    #result1 <- relocate(result1, "basin_name", .after= "am_loq")
    #fwrite(result1, file = "result1_mongo.csv", sep = ";")
    #result1 <- fread("dct_data.csv")	

    # Filtracia udajov
    if("All" %in% input$data_sources) {
      # All
    } 
    else {
      result1 <- result1[data_source %in% input$data_sources]
    }
    
    if("All" %in% input$files) {
      # All
    } 
    else {
      resultCCA <- NULL
      for (val in input$files) {
        d <- strsplit(val, ":")
        s0 <- as.numeric(d[[1]][1])
        s1 <- as.numeric(d[[1]][2])
        resultCCA <- rbind(resultCCA, result1[id %in% s0:s1])
      }
      result1 <- resultCCA
    }
    
    if("All" %in% input$substances) {
      # All
    } 
    else {
      result1 <- result1[sus_id %in% as.integer(input$substances)]
    }
    
    # Udaje z Filtra pre multivyber zluceniny
    if(isTruthy(input$multiSubstance)) {
      a <- isolate(input$multiSubstance)
      b <- str_replace_all(a, "NS", "")
      c <- strsplit(b,split=',', fixed=TRUE)
      d <- c[[1]]
      e <- unlist(d)
      result1 <- result1[sus_id %in% as.integer(e)]
      
      # Odstranenie pomocnych premennych
      rm(a)
      rm(b)
      rm(c)
      rm(d)
      rm(e)
    }
    
    if("All" %in% input$countries) {
      # All
    } 
    else {
      result1 <- result1[country %in% input$countries]
    }
    
    # vrat opat povodne, pretoze sa v tomto kroku mozu zmenit
    matrices = matricesBasin
    
    if("3" == input$matrices) {
      # Marinewater: 
      # (39) Biota - Sea water			
      # (42) Biota - Transitional water 
      # (43) Biota - Coastal water 
      # (44) Biota - Territorial (marine) water	
      result1 <- result1[matrix == 39 | matrix == 42 | matrix == 43 | matrix == 44]	
      result1[,2] <- 3
      
      # 1 - PNECbio_marine (default) - pocitany ako PNEC_marine*BCF (vid Ecotox module); je to vo vacsine pripadov PNECbio/10.
      # 2 - Same as PNECbio_fw
      # 3 - PNECbio_marine/4 (molluscs) and PNECbio_marine (all other species)
      if("1" %in% input$marine_biota_pnecs) {
        lowest_pnec_value <- lowest_pnec_value_bio_marine
      }
      else if("2" %in% input$marine_biota_pnecs) {
        lowest_pnec_value <- lowest_pnec_value_bio_marine1
      }
      else if("3" %in% input$marine_biota_pnecs) {
        lowest_pnec_value <- lowest_pnec_value_bio_marine2
        # pri molluscs by mal byt PNEC/4, ale namiesto toho je concentracia*4  
        #result1[dsgr_id == 2,3] <- result1[dsgr_id == 2,3] * 4 
      }
      lowest_pnec_value[,2] <- 2
    }
    else if("2" == input$matrices) {
      # Freshwater:
      # (40) Biota - River water 
      # (41) Biota - Lake water 
      # (45) Biota - Reservoirs 
      ## (46) Biota - Terrestrial 
      ## (47) Biota - Other
      ## result1 <- result1[matrix == 40 | matrix == 41 | matrix == 45 | matrix == 46 | matrix == 47]
      result1 <- result1[matrix == 40 | matrix == 41 | matrix == 45]
      result1[,2] <- 2
      
      lowest_pnec_value <- lowest_pnec_value_bio_fw
      lowest_pnec_value[,2] <- 1		
    }
    else if("4" == input$matrices) {
      # (46) Biota - Terrestrial 
      result1 <- result1[matrix == 46]
      matrice_id = c(46) 
      matrice_title = c("Biota - Terrestrial") 
      matrices = data.frame(matrice_id, matrice_title) 			
      
      lowest_pnec_value <- lowest_pnec_value_bio_fw
      lowest_pnec_value[,2] <- 1		
    }		
    else if("5" == input$matrices) {
      # (47) Biota - Other 
      result1 <- result1[matrix == 47]
      matrice_id = c(47) 
      matrice_title = c("Biota - Other") 
      matrices = data.frame(matrice_id, matrice_title) 				
      
      # 1 - PNECbio_marine (default) - pocitany ako PNEC_marine*BCF (vid Ecotox module); je to vo vacsine pripadov PNECbio/10.
      # 2 - Same as PNECbio_fw
      # 3 - PNECbio_marine/4 (molluscs) and PNECbio_marine (all other species)
      if("1" %in% input$marine_biota_pnecs) {
        lowest_pnec_value <- lowest_pnec_value_bio_marine
      }
      else if("2" %in% input$marine_biota_pnecs) {
        lowest_pnec_value <- lowest_pnec_value_bio_marine1
      }
      else if("3" %in% input$marine_biota_pnecs) {
        lowest_pnec_value <- lowest_pnec_value_bio_marine2
        # pri molluscs by mal byt PNEC/4, ale namiesto toho je concentracia*4  
        #result1[dsgr_id == 2,3] <- result1[dsgr_id == 2,3] * 4 
      }
      lowest_pnec_value[,2] <- 2	
    }			
    else {
      # All
      result1[,2] <- 48
      matrice_id = c(48) 
      matrice_title = c("Biota") 
      matrices = data.frame(matrice_id, matrice_title) 
      
      lowest_pnec_value <- lowest_pnec_value_bio_fw
      lowest_pnec_value[,2] <- 1		  
    }
    
    # Ak je zvolena species = molluscs tak vsetky pnecy su / 4 a 3. moznost v Marine biota PNECs
    if("2" %in% input$species & "3" %in% input$marine_biota_pnecs) {
      lowest_pnec_value[,3] <- lowest_pnec_value[,3] * 0.25
    } 	
    
    # Aktualizacia PNECs v substance
    substances[,4:6] <- lowest_pnec_value[,2:4]		
    
    if("All" %in% input$species) {
      # All
    } 
    else {
      result1 <- result1[dsgr_id %in% as.integer(input$species)]
    }		
    
    if("All" %in% input$tissue) {
      # All
    } 
    else {
      result1 <- result1[dtiel_id %in% as.integer(input$tissue)]
    }
    
    if("All" %in% input$basins) {
      # All
    } 
    else if("7" == input$basins) {
      result1 <- result1[tolower(basin_name) == "north sea"]
    }	
    else if("6" == input$basins) {
      result1 <- result1[tolower(basin_name) == "mediterranean sea"]
    }	
    else if("5" == input$basins) {
      result1 <- result1[tolower(basin_name) == "black sea"]
    }	
    else if("4" == input$basins) {
      result1 <- result1[tolower(basin_name) == "baltic sea"]
    }			
    else if("3" == input$basins) {
      result1 <- result1[tolower(basin_name) == "elbe"]
    }	
    else if("2" == input$basins) {
      result1 <- result1[tolower(basin_name) == "rhine"]
    }	
    else if("1" == input$basins) {
      result1 <- result1[tolower(basin_name) == "danube"]
    }
    
    if("All" %in% input$year) {
      # All
    } 
    else {
      result1 <- result1[sampling_date_y >= as.integer(input$year)]
    }
    
    ccaC1 <- as.integer(input$c1)
    ccaC2 <- as.integer(input$c2)
    ccaC3 <- as.integer(input$c3)
    ccaC4 <- as.integer(input$c4)
    
    #asign filter dataset to another variable- it use to complete second part final rendering dataset
    result2 <- result1
    
    # ------------------------------------------------------------------------
    # Tvorba statistik - result1 - zakladne statistiky	
    # ------------------------------------------------------------------------
    # Poznamka:
    # countXXXXXXConcentrationHigherThanLoq => concentration_value > 0 namiesto concentration_value > LoQ, pretoze vsetky zaznamy s dic_id = 1 by mali byt > LoQ 
    #fwrite(result1, file= "nasa2.csv", sep=";")
    #result1 <- fread("nasa2.csv")
    result1 <- result1[, .(
      countAnalyses = .N,
      countAnalysesConcHigherThanLoq = uniqueN(id[concentration_value > 0]),		
      countBasins = uniqueN(basin_name),
      countCountries = uniqueN(country),
      countStations = uniqueN(station_name),
      countYears = uniqueN(sampling_date_y),
      countBasinsConcHigherThanLoq = uniqueN(basin_name[concentration_value > 0]),
      countCountriesConcHigherThanLoq = uniqueN(country[concentration_value > 0]),
      countStationsConcHigherThanLoq = uniqueN(station_name[concentration_value > 0]),	
      minConc = min(concentration_value[concentration_value > 0], na.rm = TRUE),
      maxConc = max(concentration_value[concentration_value > 0], na.rm = TRUE),
      concPer10 = quantile(concentration_value[concentration_value > 0], .1, na.rm = TRUE),
      concPer90 = quantile(concentration_value[concentration_value > 0], .90, na.rm = TRUE),
      concPer95 = quantile(concentration_value[concentration_value > 0], .95, na.rm = TRUE),
      minLoq = min(am_loq[am_loq > 0], na.rm = TRUE),
      maxLoq = max(am_loq[am_loq > 0], na.rm = TRUE),
      concMedian = quantile(concentration_value[concentration_value > 0], .50, na.rm = TRUE),
      loqPer90 = quantile(am_loq, .90, na.rm = TRUE)
    ), by = list(sus_id, matrix)]
    #fwrite(result1, file= "nasa3.csv", sep=";")
    # Vsetky infinity nahradi za NA
    result1 <- result1 %>%
      mutate(minLoq = ifelse(is.infinite(minLoq), NA, minLoq)) %>%
      mutate(maxLoq = ifelse(is.infinite(maxLoq), NA, maxLoq)) %>%
      mutate(minConc = ifelse(is.infinite(minConc), NA, minConc)) %>%
      mutate(maxConc = ifelse(is.infinite(maxConc), NA, maxConc))
    
    # Pridanie dalsich stlpcov do result1
    # Vypocet na zaklade inych stlpcov v result1
    # Nie je osetrene delenie 0, pretoze pocet stanic, resp. analyz je minimalne 1 (aj ked je stanica prazdna)
    result1 <- data.table(result1)
    result1[,
            frequencyStations := (countStationsConcHigherThanLoq/countStations)*100]
    result1[,
            frequencyAnalysis := (countAnalysesConcHigherThanLoq/countAnalyses)*100]
    
    # ------------------------------------------------------------------------
    # Tvorba statistik - result2 - Dalsie statistiky zalozene na result1 
    # ------------------------------------------------------------------------
    # Pomocne vypocty
    # minLoq - mininalne LoQ v jednotlivych staniciach (podla matic) 
    # maxConc - maximalna koncentracia v jednotlivych staniciach (podla matic), ak nie je definovana potom je 0	
    #fwrite(result2, file= "nasa_r2.csv", sep=";")
    #result2 <- fread("nasa_r2.csv")
    result2 <- result2[, .(
      minLoq = min(am_loq[am_loq > 0], na.rm = TRUE),
      maxConc = max(concentration_value)
    ), by = list(sus_id, matrix, station_name)]
    
    # Odstranenie matrix zo substance, pretoze je ine (1) ako v result2 (39,40,41 ...) - je na 4. pozicii
    substances[,4] <- NULL
    
    # Pripojenie stlpcov zlucenina, CASno, lowest PNEC, lowest Type, LoQbiblioMin, ED score a CMR score
    result2 <- left_join(result2, substances, by = c("sus_id" = "sus_id"))
    
    # Prekonvertovanie do DataTable
    result2 <- data.table(result2)
    
    # Dokoncenie Dalsich statistik
    # sumLoq - Pocet stanic s minimalnou hodnotou LoQ < lowest PNEC
    # sumConc - Pocet stanic s maximalnou koncentraciou > lowest PNEC
    # MEC95 - 95ty percentil z MECsite
    # MEC99 - 99ty percentil z MECsite
    
    # 29.11.2023
    # FQ_site - Number of sites with concentration > LOQ /Total number of sites with analysis
    # sumFQsite = Number of sites with concentration > LOQ
    
    # 29.11.2023
    # FoE - (Nb sites where substance i shows RQ_i â‰Ą 1) / (Nb sites where substance i was monitored)
    # RQ_i = (MEC_site(i) / Lowest PNEC(i))
    # sumConc = Nb sites where substance i shows RQ_i â‰Ą 1    
    
    # 29.11.2023
    # MRC - MRC_i = (Nb sites where substance i shows 0.1 â‰¤ RQi < 1) / (Nb sites where substance was monitored)  
    # RQ_i = (MEC_site(i) / Lowest PNEC(i))
    # sumMRC = Nb sites where substance i shows 0.1 â‰¤ RQi < 1       
    
    result2 <- result2[, .(
      sumLoq = sum(minLoq < lowest_pnec_value),
      sumConc = sum(maxConc >= lowest_pnec_value),
      sumMRC = sum(10*maxConc >= lowest_pnec_value & maxConc < lowest_pnec_value),
      mecSite95 = quantile(maxConc, .95, na.rm = TRUE),
      mecSite99 = quantile(maxConc, .99, na.rm = TRUE)
    ), by = list(sus_id, matrix)]
    
    #create final result
    result <- cbind(result1, 
                    countStationsLoqLowerThanLowestPnec = result2$sumLoq, 
                    countStationsConcHigherThanLowestPnec = result2$sumConc, 
                    sumMRC = result2$sumMRC, 
                    mecSite95 = result2$mecSite95, 
                    mecSite99 = result2$mecSite99)
    #fwrite(result, file= "nasa_final.csv", sep=";")
    
    #remove unnecessary parts
    rm(result1, result2)
    
    #order, round, change order columns of final dataset
    result <- result[order(sus_id, matrix)] %>%
      left_join(substances, by=c("sus_id" = "sus_id")) %>%
      left_join(matrices, by=c("matrix" = "matrice_id")) %>%
      select(sus_name, sus_id, sus_cas, lowest_pnec_value, matrice_title, everything())
    
    result <- result %>%
      mutate(C0 = "Y") %>%
      mutate(C1 = ifelse(countCountries >= ccaC1 & !is.na(countCountries), "Y", "N")) %>%
      mutate(C2 = ifelse(countStations >= ccaC2 & !is.na(countStations), "Y", "N")) %>%
      mutate(C3 = ifelse(countStationsConcHigherThanLoq >= ccaC3 & !is.na(countStationsConcHigherThanLoq), "Y", "N")) %>%
      mutate(C4 = ifelse(countStationsLoqLowerThanLowestPnec >= ccaC4 & !is.na(countStationsLoqLowerThanLowestPnec), "Y", "N")) %>%
      mutate(C5 = ifelse(countStationsLoqLowerThanLowestPnec > 0 & !is.na(countStationsLoqLowerThanLowestPnec), "Y", "N")) %>%
      mutate(C6 = ifelse(!is.na(loq_biblio_min) | !is.na(minLoq), "Y", "N")) %>%
      mutate(C7 = ifelse(((loq_biblio_min < lowest_pnec_value & !is.na(loq_biblio_min)) | (minLoq < lowest_pnec_value & !is.na(minLoq) & is.na(loq_biblio_min))) & !is.na(lowest_pnec_value), "Y", "N")) %>%
      mutate(C8 = ifelse((C5 == "Y") | (C7 == "Y"), "Y", "N")) %>%
      mutate(C9 = ifelse(lowest_pnec_type == 'P-PNEC pred' | is.na(lowest_pnec_type), "N", "Y")) %>%
      mutate(C10 = ifelse(mecSite95/lowest_pnec_value >= 1 & !is.na(mecSite95) & !is.na(lowest_pnec_value) & lowest_pnec_value != 0, "Y", "N")) %>%
      mutate(C11 = ifelse(mecSite99/lowest_pnec_value >= 1 & !is.na(mecSite99) & !is.na(lowest_pnec_value) & lowest_pnec_value != 0, "Y", "N")) %>%
      mutate(C12 = ifelse((ED == 0 | ED == 0.25) | is.na(ED), "Y", "N")) %>%
      mutate(C13 = ifelse((CMR == 0 | CMR == 0.25) | is.na(CMR), "Y", "N")) %>%
      mutate(C14 = ifelse(mecSite95/lowest_pnec_value >= 0.1 & mecSite95/lowest_pnec_value < 1 & !is.na(mecSite95) & !is.na(lowest_pnec_value) & lowest_pnec_value != 0, "Y", "N")) %>%
      mutate(Category =
               ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C9 == "Y" & C10 == "Y" & C11 == "Y" & C14 == "N", "1A",
                      ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "Y" & C14 == "N", "1B",
                             ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C9 == "Y" & C10 == "N" & C11 == "Y" & C14 == "N", "1B",
                                    ifelse(C0 == "Y" & C1 == "N" & C2 == "Y" & C8 == "Y" & C9 == "Y", "2A",
                                           ifelse(C0 == "Y" & C1 == "Y" & C2 == "N" & C8 == "Y" & C9 == "Y", "2A",
                                                  ifelse(C0 == "Y" & C1 == "N" & C2 == "N" & C8 == "Y" & C9 == "Y", "2A",
                                                         ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "N" & C8 == "Y" & C9 == "Y", "2B",
                                                                ifelse(C0 == "N" & C6 == "Y" & C7 == "Y" & C8 == "Y" & C9 == "Y", "2F",
                                                                       ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C9 == "N", "3",
                                                                              ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C8 == "Y" & C9 == "N", "3",
                                                                                     ifelse(C0 == "Y" & C1 == "Y" & C2 == "N" & C4 == "N" & C5 == "N" & C6 == "Y" & C7 == "N" & C8 == "N", "4A",
                                                                                            ifelse(C0 == "Y" & C1 == "N" & C2 == "Y" & C4 == "N" & C5 == "N" & C6 == "Y" & C7 == "N" & C8 == "N", "4A",
                                                                                                   ifelse(C0 == "Y" & C1 == "N" & C2 == "N" & C4 == "N" & C5 == "N" & C6 == "Y" & C7 == "N" & C8 == "N", "4A",
                                                                                                          ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "N" & C5 == "N" & C6 == "Y" & C7 == "N" & C8 == "N", "4B",
                                                                                                                 ifelse(C0 == "N" & C6 == "N", "4F",
                                                                                                                        ifelse(C0 == "Y" & C1 == "N" & C2 == "Y" & C8 == "Y" & C9 == "N", "5A",
                                                                                                                               ifelse(C0 == "Y" & C1 == "Y" & C2 == "N" & C8 == "Y" & C9 == "N", "5A",
                                                                                                                                      ifelse(C0 == "Y" & C1 == "N" & C2 == "N" & C8 == "Y" & C9 == "N", "5A",
                                                                                                                                             ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "N" & C8 == "Y" & C9 == "N", "5B",
                                                                                                                                                    ifelse(C0 == "N" & C6 == "Y" & C7 == "Y" & C8 == "Y" & C9 == "N", "5F",
                                                                                                                                                           ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "N" & C14 == "Y", "6A",
                                                                                                                                                                  ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "N" & C13 == "Y" & C14 == "Y", "6A",
                                                                                                                                                                         ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "Y" & C14 == "Y", "6A",
                                                                                                                                                                                ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "N" & C14 == "N", "6A",
                                                                                                                                                                                       ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "N" & C13 == "Y" & C14 == "N", "6A",
                                                                                                                                                                                              ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "Y" & C14 == "N", "6A",
                                                                                                                                                                                                     ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "N" & C14 == "Y", "6A",
                                                                                                                                                                                                            ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "N" & C13 == "Y" & C14 == "Y", "6A",
                                                                                                                                                                                                                   ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "Y" & C14 == "Y", "6A",
                                                                                                                                                                                                                          ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "N" & C14 == "N", "6A",
                                                                                                                                                                                                                                 ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "N" & C13 == "Y" & C14 == "N", "6A",
                                                                                                                                                                                                                                        ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "Y" & C14 == "N", "6A",
                                                                                                                                                                                                                                               ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "N" & C4 == "Y" & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "Y" & C14 == "N", "6B",
                                                                                                                                                                                                                                                      ifelse(C0 == "Y" & C1 == "Y" & C2 == "Y" & C3 == "Y" 		     & C5 == "Y" & C8 == "Y" & C9 == "Y" & C10 == "N" & C11 == "N" & C12 == "Y" & C13 == "Y" & C14 == "N", "6B",
                                                                                                                                                                                                                                                             "No. cat."))))))))))))))))))))))))))))))))))) %>%
      mutate(FQsitescore = ifelse(countStationsConcHigherThanLoq/countStations > 0, signif(as.numeric(countStationsConcHigherThanLoq/countStations), digits = 2), 0)) %>%
      mutate(FoEscore = ifelse(countStationsConcHigherThanLowestPnec/countStations > 0, signif(as.numeric(countStationsConcHigherThanLowestPnec/countStations), digits = 2), 0)) %>%
      mutate(MRCscore = ifelse(sumMRC/countStations > 0, signif(as.numeric(sumMRC/countStations), digits = 2), 0)) %>%
      mutate(EoEscore = 
               ifelse(mecSite95/lowest_pnec_value < 1, 0,
                      ifelse(mecSite95/lowest_pnec_value <= 10, 0.1,
                             ifelse(mecSite95/lowest_pnec_value <= 100, 0.25,
                                    ifelse(mecSite95/lowest_pnec_value <= 1000, 0.5, 1))))) %>%
      mutate(FinalRISKscore = ifelse(as.numeric(FoEscore) > 0, signif(as.numeric(FoEscore) + EoEscore, digits = 2), signif(EoEscore, digits = 2)))
    
    # FoEvalue = number of sites where MECsite > Lowest PNEC divided by total number of sites 
    # where the substance was measured (recent data)
    # FoEscore = FoEvalue as a fraction rounded to two decimals
    
    # EoEvalue = MEC95 / Lowest PNEC
    # EoEscore = 
    #	IF MEC95/Lowest PNEC <1, then 0
    #	IF MEC95/Lowest PNEC â‰Ą1 â‰¤10, then 0.1
    #	IF MEC95/Lowest PNEC >10 â‰¤100,	then 0.25
    #	IF MEC95/Lowest PNEC >100 â‰¤1000, then 0.5
    #	IF MEC95/Lowest PNEC >1000, then 1
    
    # 29.11.2023
    # EoE_i = ( (MEC95(i)) / (Lowest PNEC (i)) )  
    
    # FinalRISKscore = FoEscore + EoEscore
    
    # FinalHazardScore = hazardScore = max(PBMT; CMR; ED) = z DB
    result <- left_join(result, hazardScore, by = c("sus_id" = "sus_id"))	
    
    # EXPOSURE score for category 1, 3, 6
    # FinalEXPOSUREscore = (A+B+C)/3 + D + E
    # A) Number of countries with concentration >LOQ divided by total number of countries where the substance was measured 
    # B) Number of sites with concentration >LoQ divided by total number of sites where the substance was measured 
    # C) Number of observations with concentration >LOQ divided by total number of analysis 
    # D) Concentration trend 
    # E) Observation in groundwater
    result <- result %>%
      mutate(FinalEXPOSUREscore = signif((countCountriesConcHigherThanLoq/countCountries + countStationsConcHigherThanLoq/countStations + countAnalysesConcHigherThanLoq/countAnalyses)/3, digits = 2))
    
    result <- result %>%
      mutate(FINALscore = signif((hazardScore + FinalRISKscore + FinalEXPOSUREscore)/1, digits = 2))
    
    # Final EXPOSURE score (category 2, 4, 5) = Exposure score fw KEMI = z DB
    result <- left_join(result, ExposureScoreWaterKEMI, by = c("sus_id" = "sus_id"))	
    
    # Final EXPOSURE score (category 2, 4, 5) = Exposure score fw KEMI = z DB
    result <- left_join(result, PBMT, by = c("sus_id" = "sus_id"))	
    
    # Odstranenie ID matice (6. stlpec z result)
    result[,6] <- NULL
    
    result <- result %>%
      mutate(FINALscoreNEW =
               ifelse(Category == "1A" | Category == "1B", signif((FQsitescore + FoEscore + MRCscore + EoEscore)/1, digits = 2),
                      ifelse(Category == "2A" | Category == "2B", signif((FQsitescore + FoEscore + MRCscore)/1, digits = 2),
                             ifelse(Category == "2F", signif(ExposureScoreWaterKEMI, digits = 2),
                                    ifelse(Category == "3", signif((FQsitescore + FoEscore + MRCscore + EoEscore)/1, digits = 2),
                                           ifelse(Category == "4A" | Category == "4B", signif((FQsitescore + FoEscore + MRCscore + EoEscore)/1, digits = 2),
                                                  ifelse(Category == "4F", signif(ExposureScoreWaterKEMI, digits = 2),
                                                         ifelse(Category == "5A" | Category == "5B", signif((FQsitescore + FoEscore + MRCscore)/1, digits = 2),
                                                                ifelse(Category == "5F", signif(ExposureScoreWaterKEMI, digits = 2),
                                                                       ifelse(Category == "6A", signif((FQsitescore + FoEscore + MRCscore)/1, digits = 2),
                                                                              ifelse(Category == "6B", signif((FQsitescore)/1, digits = 2),
                                                                                     "-")))))))))))
    
    # Zaokruhlenie
    # ------------
    # Vyber udajov na zaokruhenie na 3 "Platne" cislice 
    lowPNEC <- result$lowest_pnec_value
    
    # Funkcia na zokruhlenie
    prettyNumber<-function(x,y,z) {
      ifelse(x >= y, round(x, z), x)	
    }
    
    # 12 urovni desatinnych miest
    for (i in 0:12) {
      lowPNEC <- prettyNumber(lowPNEC, 100/10^i, i)
    }	
    # Prevod na tabulku
    lowPNEC <- data.table(lowPNEC)
    # Vratenie do povodnych dat
    result[,4] <- lowPNEC
    
    # Uprava SUSDID na format NSXXXXXXXX
    result[,2] <- result[,2] %>% mutate(sus_id = sprintf('NS%08d', sus_id))
    # ------------
    
    # Zoradenie podla "Final RISK score" - minus znamena DESC
    result <- result[order(-FinalRISKscore)]	
    
    # Premenovanie jedneho stlpca
    colnames(result)[1] <- "Substance"
    colnames(result)[2] <- "SusDat ID"
    colnames(result)[3] <- "CAS no."
    colnames(result)[4] <- "Lowest PNEC"
    colnames(result)[5] <- "Matrix"
    colnames(result)[6] <- "No. of Analyses"
    colnames(result)[7] <- "No. of Analyses with conc > LoQ"
    colnames(result)[8] <- "No. of Basins"
    colnames(result)[9] <- "No. of Countries"
    colnames(result)[10] <- "No. of Sites"
    colnames(result)[11] <- "No. of Years"
    colnames(result)[12] <- "No. of Basins with conc > LoQ"
    colnames(result)[13] <- "No. of Countries with conc > LoQ"
    colnames(result)[14] <- "No. of Sites with conc > LoQ"
    colnames(result)[15] <- "Min Conc"
    colnames(result)[16] <- "Max Conc"
    colnames(result)[17] <- "10th Conc"
    colnames(result)[18] <- "90th Conc"
    colnames(result)[19] <- "95th Conc"
    colnames(result)[20] <- "min LoQ"
    colnames(result)[21] <- "max LoQ"
    colnames(result)[22] <- "Conc Median"
    colnames(result)[23] <- "90th LoQ"
    colnames(result)[24] <- "Frequency Sities"
    colnames(result)[25] <- "Frequency Analysis"
    colnames(result)[26] <- "No. of Sites with LoQ < Lowest PNEC"
    colnames(result)[27] <- "No. of Sites with conc > Lowest PNEC"
    colnames(result)[28] <- "95th MECsite"
    colnames(result)[29] <- "99th MECsite"
    colnames(result)[30] <- "Type of PNEC"
    colnames(result)[31] <- "LoQ Biblio min"
    colnames(result)[32] <- "score ED"
    colnames(result)[33] <- "score CMR"
    colnames(result)[51] <- "FQ site score"	
    colnames(result)[52] <- "FoE score"
    colnames(result)[53] <- "MRC score"
    colnames(result)[54] <- "EoE score"
    colnames(result)[55] <- "Final RISK score"
    colnames(result)[56] <- "Final HAZARD score"
    colnames(result)[57] <- "Final EXPOSURE score"
    colnames(result)[58] <- "Final score"	
    colnames(result)[59] <- "EXPOSURE score KEMI"
    colnames(result)[60] <- "HAZARD score KEMI"
    colnames(result)[61] <- "P score"
    colnames(result)[62] <- "B score"
    colnames(result)[63] <- "M score"
    colnames(result)[64] <- "T score"
    colnames(result)[65] <- "PBT score"
    colnames(result)[66] <- "PB score"
    colnames(result)[67] <- "Final score NEW"
    
    output$table = DT::renderDataTable(server = TRUE, {
      datatable(result,
                class = 'cell-border stripe',
                caption = 'Customized dataset',
                extensions = c('Buttons'),
                rownames = FALSE,
                filter = 'top',
                options = list(
                  dom = 'Blfrtip',
                  pageLength = 100,
                  buttons = list(list(extend = 'colvis', columns = seq(11,48,1))),
                  columnDefs = list(list(visible = FALSE, targets = seq(11,48,1)), list(className = 'dt-nowrap', targets = 1))
                )
      )
    })
    #Blfrtip
    
    #JS UI
    shinyjs::enable("save")
    shinyjs::hideElement("loading")
    shinyjs::showElement("downloadDataCSV")
    shinyjs::showElement("downloadDataXLSX")
    
    #download CSV button
    output$downloadDataCSV <- downloadHandler(
      filename = function() {
        paste("customizedDataset", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(result, file, row.names = FALSE)
      }
    )
    
    #download XLSX button
    output$downloadDataXLSX <- downloadHandler(
      filename = function() {
        paste("customizedDataset", ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(result, file)
      }
    )
    
  })
}

#outlier callback
outlier_callback <- function(input, output, session) {
  #update select from server side - LOAD option
  updateSelectizeInput(session, 'substances_outlier', choices = c("All", choices_substances), selected = "All", server = TRUE)
  updateSelectizeInput(session, 'year_outlier', choices = c("All", seq(from = 2003, to = as.integer(format(Sys.Date(), "%Y")), by = 1)), selected = "All", server = TRUE)
  
  #after click save(FILTER button)-this action started
  observeEvent(input$save_outlier, {
    #JS UI
    shinyjs::disable("save_outlier")
    shinyjs::showElement("loading_outlier")
    
    #filter dct
    outlier_k_1 <- dct_analysis
    
    if("All" %in% input$substances_outlier) {
      #
    }
    else{
      outlier_k_1 <- outlier_k_1[sus_id %in% as.integer(input$substances_outlier)]
    }
    if("All" %in% input$year_outlier) {
      #
    }
    else{
      outlier_k_1 <- outlier_k_1[sampling_date_y >= as.integer(input$year_outlier)]
    }
    
    #asign filter dataset to another variable- it use to complete second part final rendering dataset
    outlier_k_2 <- outlier_k_1
    outlier_k_3 <- outlier_k_1
    
    #do statistics(first part of final dataset) K=1.5
    outlier_k_1 <- outlier_k_1[, {
      k = 1.5;
      concPer25 = quantile(concentration_value[concentration_value > 0], .25, na.rm = TRUE);
      concPer75 = quantile(concentration_value[concentration_value > 0], .75, na.rm = TRUE);
      concPer95 = quantile(concentration_value[concentration_value > 0], .95, na.rm = TRUE);
      minConc = min(concentration_value[concentration_value > 0]);
      maxConc = max(concentration_value[concentration_value > 0]);
      IQR = (concPer75 - concPer25);
      LFL = (concPer25 - k*(concPer75 - concPer25));
      UFL = (concPer75 + k*(concPer75 - concPer25));
      concHigherThanLoqSmallerThanLFL = uniqueN(id[concentration_value > 0 & concentration_value < LFL]);
      concHigherThanLoqHigherThanUFL = uniqueN(id[concentration_value > 0 & concentration_value > UFL]);
      list(k = k, concPer25 = concPer25, concPer75 = concPer75, concPer95 = concPer95, minConc = minConc, maxConc = maxConc, IQR = IQR, LFL = LFL, UFL = UFL, concHigherThanLoqSmallerThanLFL = concHigherThanLoqSmallerThanLFL, concHigherThanLoqHigherThanUFL = concHigherThanLoqHigherThanUFL)
    }, by = list(sus_id)]
    
    #do statistics(first part of final dataset) K=10
    outlier_k_2 <- outlier_k_2[, {
      k = 10;
      concPer25 = quantile(concentration_value[concentration_value > 0], .25, na.rm = TRUE);
      concPer75 = quantile(concentration_value[concentration_value > 0], .75, na.rm = TRUE);
      concPer95 = quantile(concentration_value[concentration_value > 0], .95, na.rm = TRUE);
      minConc = min(concentration_value[concentration_value > 0]);
      maxConc = max(concentration_value[concentration_value > 0]);
      IQR = (concPer75 - concPer25);
      LFL = (concPer25 - k*(concPer75 - concPer25));
      UFL = (concPer75 + k*(concPer75 - concPer25));
      concHigherThanLoqSmallerThanLFL = uniqueN(id[concentration_value > 0 & concentration_value < LFL]);
      concHigherThanLoqHigherThanUFL = uniqueN(id[concentration_value > 0 & concentration_value > UFL]);
      list(k = k, concPer25 = concPer25, concPer75 = concPer75, concPer95 = concPer95, minConc = minConc, maxConc = maxConc, IQR = IQR, LFL = LFL, UFL = UFL, concHigherThanLoqSmallerThanLFL = concHigherThanLoqSmallerThanLFL, concHigherThanLoqHigherThanUFL = concHigherThanLoqHigherThanUFL)
    }, by = list(sus_id)]
    
    #do statistics(first part of final dataset) K=1000
    outlier_k_3 <- outlier_k_3[, {
      k = 1000;
      concPer25 = quantile(concentration_value[concentration_value > 0], .25, na.rm = TRUE);
      concPer75 = quantile(concentration_value[concentration_value > 0], .75, na.rm = TRUE);
      concPer95 = quantile(concentration_value[concentration_value > 0], .95, na.rm = TRUE);
      minConc = min(concentration_value[concentration_value > 0]);
      maxConc = max(concentration_value[concentration_value > 0]);
      IQR = (concPer75 - concPer25);
      LFL = (concPer25 - k*(concPer75 - concPer25));
      UFL = (concPer75 + k*(concPer75 - concPer25));
      concHigherThanLoqSmallerThanLFL = uniqueN(id[concentration_value > 0 & concentration_value < LFL]);
      concHigherThanLoqHigherThanUFL = uniqueN(id[concentration_value > 0 & concentration_value > UFL]);
      list(k = k, concPer25 = concPer25, concPer75 = concPer75, concPer95 = concPer95, minConc = minConc, maxConc = maxConc, IQR = IQR, LFL = LFL, UFL = UFL, concHigherThanLoqSmallerThanLFL = concHigherThanLoqSmallerThanLFL, concHigherThanLoqHigherThanUFL = concHigherThanLoqHigherThanUFL)
    }, by = list(sus_id)]
    
    #create final result
    outlier <- rbind(outlier_k_1, outlier_k_2, outlier_k_3)
    
    #order, round, change order columns,joins of final dataset
    outlier <- outlier %>%
      arrange(sus_id) %>%
      left_join(substances, by=c("sus_id" = "sus_id")) %>%
      select(sus_name, sus_cas, everything()) %>%
      mutate_if(is.numeric, signif, digits = 3)
    
    #remove unnecessary parts
    rm(outlier_k_1, outlier_k_2, outlier_k_3)
    
    
    #first small-final dataset of outlier part
    outlier_final <- outlier %>%
      group_by(k) %>%
      summarise(
        nOfSubstances = n_distinct(sus_id[maxConc > UFL]),
        n = n(),
        percentOfSubstances = (nOfSubstances/n) * 100
      ) %>%
      mutate_if(is.numeric, signif, digits = 3) %>%
      mutate_if(is.numeric, format, big.mark = " ")
    
    #second small-final dataset of outlier part
    outlier_final_p95 <- outlier %>%
      group_by(k) %>%
      summarise(
        nOfSubstances = n_distinct(sus_id[concPer95 > UFL]),
        n = n(),
        percentOfSubstances = (nOfSubstances/n) * 100
      ) %>%
      mutate_if(is.numeric, signif, digits = 3) %>%
      mutate_if(is.numeric, format, big.mark = " ")
    
    # Do datasetu sa dostali PNEC type, ED CMR z customized	- vymazat
    outlier$loq_biblio_min = NULL
    outlier$lowest_pnec_type = NULL
    outlier$ED = NULL
    outlier$CMR = NULL
    
    
    colnames(outlier)[1] <- "Substance"
    
    #render outlier dataset
    output$table1 = DT::renderDataTable(server = TRUE, {
      datatable(outlier,
                class = 'cell-border stripe',
                caption = 'Outliers dataset',
                extensions = c('Buttons'),
                rownames = FALSE,
                options = list(
                  dom = 'Blfrtip',
                  pageLength = 10,
                  buttons = list(list(extend = 'colvis', columns = seq(4,14,1))),
                  columnDefs = list(list(visible = FALSE, targets = c(1,2,4,5,6,7,8,14)), list(className = 'dt-center', targets = 0:14))
                )
      )
    })
    
    #render first small-outlirt dataset
    output$table2 = DT::renderDataTable(server = FALSE, {
      datatable(outlier_final,
                class = 'cell-border stripe',
                caption = 'First final table',
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel'),
                  columnDefs = list(list(className = 'dt-center', targets = 0:4))
                )
      )
    })
    
    #render second small-outlirt dataset
    output$table3 = DT::renderDataTable(server = FALSE, {
      datatable(outlier_final_p95,
                class = 'cell-border stripe',
                caption = 'First final outliers dataset',
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('csv', 'excel'),
                  columnDefs = list(list(className = 'dt-center', targets = 0:4))
                )
      )
    })
    
    #JS UI
    shinyjs::enable("save_outlier")
    shinyjs::hideElement("loading_outlier")
    shinyjs::showElement("downloadDataCSVOutlier")
    shinyjs::showElement("downloadDataXLSXOutlier")
    
    #download CSV button
    output$downloadDataCSVOutlier <- downloadHandler(
      filename = function() {
        paste("outliersDataset", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(outlier, file, row.names = FALSE)
      }
    )
    
    #download XLSX button
    output$downloadDataXLSXOutlier <- downloadHandler(
      filename = function() {
        paste("outliersDataset", ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(outlier, file)
      }
    )
    
  })
}

maps_callback <- function(input, output, session) {
  #update substances select option from server side
  updateSelectizeInput(session, 'substances_map', choices = choices_substances, selected = 4, server = TRUE)
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    if(input$type_of_map == "No. of stations with measurements") {
      dct_analysis %>%
        filter(latitude_decimal < 70.0 & longitude_decimal < 70.0 & sus_id %in% as.integer(input$substances_map)) %>%
        group_by(sus_id, latitude_decimal, longitude_decimal, country, station_name) %>%
        summarise(
          n = n()
        ) %>%
        left_join(countries, by = c("country" = "country"))
    }
    else if(input$type_of_map == "Maximum concentration per site (MEC95)") {
      dct_analysis %>%
        filter(latitude_decimal < 70.0 & longitude_decimal < 70.0 & sus_id %in% as.integer(input$substances_map)) %>%
        group_by(sus_id,country) %>%
        summarise(
          n = n(),
          maxConc = max(concentration_value),
          maxConcLon = longitude_decimal[which.max(concentration_value)],
          maxConcLat = latitude_decimal[which.max(concentration_value)],
          maxConcStation = station_name[which.max(concentration_value)]
        ) %>%
        left_join(countries, by = c("country" = "country"))
    }
    else if(input$type_of_map == "No. of stations with measurements > LOQ") {
      dct_analysis %>%
        filter(latitude_decimal < 70.0 & longitude_decimal < 70.0 & concentration_value > 0 & concentration_value > 0 & sus_id %in% as.integer(input$substances_map)) %>%
        group_by(sus_id, latitude_decimal, longitude_decimal, country, station_name) %>%
        summarise(
          n = n()
        ) %>%
        left_join(countries, by = c("country" = "country"))
    }
    else if(input$type_of_map == "No. of stations with measurements > Lowest PNEC") {
      dct_analysis %>%
        filter(latitude_decimal < 70.0 & longitude_decimal < 70.0 & concentration_value > 0 & concentration_value > lowest_pnec_value & lowest_pnec_value > 0 & sus_id %in% as.integer(input$substances_map)) %>%
        group_by(sus_id, latitude_decimal, longitude_decimal, country, station_name) %>%
        summarise(
          n = n()
        ) %>%
        inner_join(lowest_pnec, by = c("sus_id" = "sus_id"))
      left_join(countries, by = c("country" = "country"))
    }
  })
  
  #output chosen final map
  output$mymap <- renderLeaflet({
    if(input$type_of_map == "No. of stations with measurements") {
      leaflet(data = filteredData(), options = leafletOptions(minZoom = 2, maxZoom = 18) ) %>%
        clearShapes() %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions(),
                   ~as.numeric(longitude_decimal),
                   ~as.numeric(latitude_decimal),
                   popup = ~as.character(paste("Station: ",station_name, "<br>","Country: ",country_name, "<br>", "No. of measurements: ",n))
        )
    }
    else if(input$type_of_map == "Maximum concentration per site (MEC95)") {
      leaflet(data = filteredData(), options = leafletOptions(minZoom = 2, maxZoom = 18) ) %>%
        clearShapes() %>%
        addTiles() %>%
        addMarkers(~as.numeric(maxConcLon), ~as.numeric(maxConcLat), popup = ~as.character(paste("Station: ",maxConcStation, "<br>", "Country: ",country_name, "<br>", "MaxConc: ",maxConc)))
    }
    else if(input$type_of_map == "No. of stations with measurements > LOQ") {
      leaflet(data = filteredData(), options = leafletOptions(minZoom = 2, maxZoom = 18) ) %>%
        clearShapes() %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions(),
                   ~as.numeric(longitude_decimal),
                   ~as.numeric(latitude_decimal),
                   popup = ~as.character(paste("Station: ",station_name, "<br>","Country: ",country_name, "<br>", "No. of measurements: ",n))
        )
    }
    else if(input$type_of_map == "No. of stations with measurements > Lowest PNEC") {
      leaflet(data = filteredData(), options = leafletOptions(minZoom = 2, maxZoom = 18) ) %>%
        clearShapes() %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions(),
                   ~as.numeric(longitude_decimal),
                   ~as.numeric(latitude_decimal),
                   popup = ~as.character(paste("Station: ",station_name, "<br>","Country: ",country_name, "<br>", "No. of measurements: ",n))
        )
    }
  })
}

# Creates router. We provide routing path, a UI as
# well as a server-side callback for each page.
router <- make_router( 
  route("/", root_page),
  route("search", search),
  route("basic", basic, basic_callback),
  route("customized", customized, customized_callback),
  route("outlier", outlier, outlier_callback),
  route("maps", maps, maps_callback)
)



# Plug router into Shiny server.
server <- shinyServer(function(input, output, session) {
  router(input, output, session)
})

# Run server in a standard way.
shinyApp(ui = htmlTemplate("index.html"), server)
