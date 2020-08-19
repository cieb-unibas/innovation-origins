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

# # calculation from first tech_field (input one) to last tech_field (input two) 
tech_field_start_index <- c(1, 35)

# Determine the tech_field using slurm
# args <- commandArgs(TRUE)
# tech_field_start_index <- c(as.numeric(as.character(args[1])), as.numeric(as.character(args[2])))


###########################################
############ DEFINE FUNCTION ##############
###########################################

cross_bord_EPO_func <- function(tech_field_start, 
                                DE_regions = c("Freiburg", "Schwaben"),
                                FR_regions = c("Alsace", "Rhône-Alpes", "Lorraine", "Franche-Comté"),
                                IT_regions = c("Lombardy", "Piedmont"),
                                AT_regions = c("Vorarlberg")){
        
        # Load inventor data 
        inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg_", tech_field_start, ".rds"))
        
        # Load USPTO patents and remove those inventors from the sample
        us_inv <- readRDS(paste0(mainDir1, "/created data/inv_reg/inv_reg_us_", tech_field_start, ".rds"))
        drop_keys <- which(inv_reg$p_key %in% unique(us_inv$p_key))
        inv_reg <- inv_reg[-drop_keys, ]
        
        # determine number of inventors per p_key and drop all inventors that are listed more than once per p_key
        inv_reg <- inv_reg %>% dplyr::select(p_key, name, Ctry_code, Up_reg_label, patent_id)
        inv_reg <- setDT(inv_reg)[, num_inv := .N, .(p_key)]
        inv_reg <- mutate(inv_reg, Up_reg_label = trimws(Up_reg_label))
        inv_reg <- distinct(inv_reg, p_key, name, .keep_all =  TRUE)
        
        # Load firm data and drop firms that are listed more than once per p_key
        firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg_", tech_field_start, ".rds")) 
        firm_reg <- firm_reg %>% dplyr::select(p_key, organization, country, Up_reg_label)
        firm_reg <- mutate(firm_reg, Up_reg_label = trimws(Up_reg_label))
        
        # Keep only patents for which all firms are from CH
        firm_reg <- setDT(firm_reg)[, firm_ctry := paste0(unique(country), collapse = ";"), .(p_key)]
        firm_reg <- filter(firm_reg, firm_ctry == "CH")
        firm_reg <- distinct(firm_reg, p_key, organization, Up_reg_label, .keep_all = TRUE)
        
        # match firms and inventors
        inv_firm <- inner_join(inv_reg, firm_reg, by = c("p_key"))
        
        # subset to Swiss-based firms
        inv_firm <- inv_firm %>% filter(country == "CH")
        
        ## identify commuters ---------------------------------------------------
        
        # identify German commuters (lower bound)
        DE_commuters <- inv_firm %>% filter(Up_reg_label.y %in% c("Northwestern Switzerland",
                                                              "Zürich", "Eastern Switzerland"),
                                        Up_reg_label.x %in% DE_regions)
        
        # identify French commuters (lower bound)
        FR_commuters <- inv_firm %>% filter(Up_reg_label.y %in% c("Northwestern Switzerland",
                                                                  "Lake Geneva Region"),
                                            Up_reg_label.x %in% FR_regions)
        
        # identify Austrian commuters (lower bound)
        AT_commuters <- inv_firm %>% filter(Up_reg_label.y %in% c("Eastern Switzerland"),
                                            Up_reg_label.x %in% AT_regions)
        
        # identify Italian commuters (lower bound)
        IT_commuters <- inv_firm %>% filter(Up_reg_label.y %in% c("Ticino"),
                                            Up_reg_label.x %in% IT_regions)
        
        # combine identified commuters
        inv_firm <- rbind(DE_commuters, FR_commuters, AT_commuters, IT_commuters)
        
        # add cross-bord information & assign CH and CH-region to patent
        inv_firm <- mutate(inv_firm,
                           cross_bord = "yes",
                           ctry_pat  = country,
                           regio_pat = Up_reg_label.y, 
                           regio_firm  = Up_reg_label.y, 
                           ctry_firm = "CH",
                           regio_inv   = Up_reg_label.x,
                           ctry_inv  = Ctry_code,   
                           tech_field = tech_field_start)
        
        inv_firm <- dplyr::select(inv_firm, 
                                  p_key, organization, name, ctry_inv, ctry_firm, 
                                  ctry_pat, regio_firm, regio_inv, regio_pat, 
                                  cross_bord, tech_field)
        
        # return the data
        return(inv_firm)
}


##########################################
############ RUN THE FUNCTION ############
##########################################

## TEST:
# df <- cross_bord_EPO_func(tech_field_start = 16)

## Create data for all tech-fields
inv_EPO_adj <- do.call(rbind.fill, lapply(seq(tech_field_start_index[1], tech_field_start_index[2], 1), function(x) cross_bord_EPO_func(x))) 
print("Cross-border commuters identified for all tech-fields")

##########################################
############ SAVE THE DATASET ############
##########################################

inv_EPO_adj %>% saveRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_reg_adj_commuters_ep.rds")
print("Dataset saved as 'inv_reg_adj_commuters_ep.rds'")


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
# [1] "Alsace"                    "Rhône-Alpes"                "Auvergne"                   "Île-de-France"             
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
