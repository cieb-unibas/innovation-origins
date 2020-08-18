#########################################################
# Use this script to identify cross-border commuters    #
# of EPO patents.                                       #
# Authors:        Matthias Niggli/CIEB UniBasel         #
#                 Christian Rutzer/CIEB UniBasel        #
# Date:           18.08.2020                            #
#########################################################


###########################################
#### LOAD PACKAGES AND SET DIRECTORIES ####
###########################################

library("tidyverse")
library("data.table")
mainDir1 <- c("/scicore/home/weder/GROUP/Innovation/01_patent_data")

print("Packages loaded and directories set.")

###########################################
####### SPECIFY TECHNOLOGY FIELD ##########
###########################################

# manually specify single tech_field
tech_field_start <- c(16)

# # calculation from first tech_field (input one) to last tech_field (input two) 
# tech_field_start_index <- c(1, 35)

# Determine the tech_field using slurm
# args <- commandArgs(TRUE)
# tech_field_start_index <- c(as.numeric(as.character(args[1])), as.numeric(as.character(args[2])))

###########################################
############ DEFINE FUNCTION ##############
###########################################

cross_bord_EPO_func <- function(tech_field_start, ctry_firm = "CH", 
                                DE_regions = c("Freiburg", "Karlsruhe", "Stuttgart", "Schwaben"),
                                FR_regions = c("Alsace", "Rhône-Alpes", "Lorraine", "Franche-Comté"),
                                IT_regions = c("Lombardy", "Piedmont"),
                                AT_regions = c("Vorarlberg")){
        
        # Load inventor data, 
        inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg_", tech_field_start, ".rds"))
        
        # Load USPTO patents and remove those inventors from the sample
        us_inv <- readRDS(paste0(mainDir1, "/created data/inv_reg/inv_reg_us_", tech_field_start, ".rds"))
        drop_keys <- which(inv_reg$p_key %in% unique(us_inv$p_key))
        inv_reg <- inv_reg[-drop_keys, ]
        
        # determine number of inventors per p_key and drop all inventors that are listed more than once per p_key
        inv_reg <- inv_reg %>% dplyr::select(p_key, name, Ctry_code, Up_reg_label, patent_id)
        inv_reg <- setDT(inv_reg)[, num_inv := .N, .(p_key)]
        inv_reg <- distinct(inv_reg, p_key, name, .keep_all =  TRUE)
        
        # Load firm data and drop firms that are listed more than once per p_key
        firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg_", tech_field_start, ".rds")) 
        firm_reg <- firm_reg %>% dplyr::select(p_key, organization, country, Up_reg_label)
        firm_reg <- distinct(firm_reg, p_key, organization, .keep_all = TRUE)
        
        # match firms and inventors
        inv_firm <- inner_join(inv_reg, firm_reg, by = c("p_key"))
        
        # subset to Swiss-based firms
        inv_firm <- inv_firm %>% filter(country == ctry_firm)
        
        ## identify commuters ---------------------------------------------------
        
        # identify German commuters (lower bound)
        DE_commuters <- inv_firm %>% filter(Up_reg_label.y %in% c("Northwestern Switzerland ",
                                                              "Zürich", "Eastern Switzerland"),
                                        Up_reg_label.x %in% DE_regions)
        
        # identify French commuters (lower bound)
        FR_commuters <- inv_firm %>% filter(Up_reg_label.y %in% c("Northwestern Switzerland ",
                                                                  "Lake Geneva Region"),
                                            Up_reg_label.x %in% FR_regions)
        
        # identify Austrian commuters (lower bound)
        AT_commuters <- inv_firm %>% filter(Up_reg_label.y %in% c("Eastern Switzerland"),
                                            Up_reg_label.x %in% AT_regions)
        
        # identify Italian commuters (lower bound)
        IT_commuters <- inv_firm %>% filter(Up_reg_label.y %in% c("Ticino"),
                                            Up_reg_label.x %in% IT_regions)
        
        # combine identified commuter
        inv_firm <- rbind(DE_commuters, FR_commuters, AT_commuters, IT_commuters)
        
        # add crossbord information & assign CH and CH-region to patent
        inv_firm <- mutate(inv_firm,
                           cross_bord = "yes",
                           ctry_pat = country,
                           regio_pat = Up_reg_label.y)
        
        # return the data
        return(inv_firm)
}


##########################################
############ RUN THE FUNCTION ############
##########################################

## TEST:
df <- cross_bord_EPO_func(tech_field_start = 16)

##########################################
######### REGIONS TO CHOOSE FROM #########
##########################################

## GERMANY ---------------------------------------------------------------------
# [1] "Germany - not regionalised" "Oberbayern"                 "Freiburg"                   "Weser-Ems"                 
# [5] "Karlsruhe"                  "Oberfranken"                "Darmstadt"                  "Hannover"                  
# [9] "Düsseldorf"                 "Thüringen"                  "Rheinhessen-Pfalz"          "Köln"                      
# [13] "Berlin"                     "Leipzig"                    "Gießen"                     "Unterfranken"              
# [17] "Saarland"                   "Brandenburg"                "Niederbayern"               "Tübingen"                  
# [21] "Sachsen-Anhalt"             "Mittelfranken"              "Arnsberg"                   "Koblenz"                   
# [25] "Detmold"                    "Oberpfalz"                  "Stuttgart"                  "Bremen"                    
# [29] "Hamburg"                    "Braunschweig"               "Schleswig-Holstein"         "Schwaben"                  
# [33] "Dresden"                    "Münster"                    "Lüneburg"                   "Kassel"            
        
## FRANCE ----------------------------------------------------------------------
# [1] "Alsace "                    "Rhône-Alpes"                "Auvergne"                   "Île-de-France"             
# [5] "Champagne-Ardenne"          "Centre-Val de Loire"        "Provence-Alpes-Côte d'Azur" "France - not regionalised" 
# [9] "Burgundy"                   "Nord-Pas-de-Calais"         "Picardy"                    "Pays de la Loire"          
# [13] "Aquitaine"                  "Lorraine"                   "Midi-Pyrénées"              "Languedoc-Roussillon "     
# [17] "Lower Normandy "            "Upper Normandy"             "Brittany"                   "Guadeloupe"                
# [21] "Franche-Comté"              "Poitou-Charentes"

## AUSTRIA ---------------------------------------------------------------------
# [1] "Lower Austria"              "Vienna"                     "Tyrol"                      "Vorarlberg"                
# [5] "Upper Austria"              "Styria"                     "Austria - not regionalised" "Carinthia"  

## ITALY  ----------------------------------------------------------------------
# [1] "Friuli-Venezia Giulia"     "Lombardy"                  "Tuscany"                   "Campania"                 
# [5] "Umbria"                    "Veneto"                    "Piedmont"                  "Italy - not regionalised" 
# [9] "Emilia-Romagna"            "Marche"                    "Lazio"                     "Liguria"                  
# [13] "Apulia"                    "Sicily"                    "Province of Bolzano-Bozen" "Abruzzo"                  
# [17] "Calabria"


##########################################
############ SAVE THE DATASET ############
##########################################





