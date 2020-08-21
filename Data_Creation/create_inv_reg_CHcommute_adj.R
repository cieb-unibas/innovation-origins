#########################################################
# Use this script to create a dataset that takes        #
# cross-border commuters in Switzerland into account    #
# and thereby adjusts Swiss patent counts               #
# Authors:        Matthias Niggli/CIEB UniBasel         #
#                 Christian Rutzer/CIEB UniBasel        #
# Date:           19.08.2020                            #
#########################################################


###########################################
#### LOAD PACKAGES AND SET DIRECTORIES ####
###########################################

library("tidyverse")
library("data.table")
mainDir1 <- c("/scicore/home/weder/GROUP/Innovation/01_patent_data")

print("Packages loaded and directories set.")


###########################################
############ LOAD THE DATA ################
###########################################

## origin of all patents in the sample non-adjusted for cross-border commuters:
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds"))

## USPTO patents with adjusted origin:
adj_commuters_us <- readRDS(paste0(mainDir1, "/created data/inv_reg_adj_commuters_us.rds"))
adj_commuters_us <- adj_commuters_us %>% filter(cross_bord == "yes")
adj_commuters_us <- adj_commuters_us %>% select(p_key, name, ctry_pat, regio_pat, cross_bord)

## EPO patents with adjusted origin:
adj_commuters_epo <- readRDS(paste0(mainDir1, "/created data/inv_reg_adj_commuters_ep.rds"))
adj_commuters_epo <- adj_commuters_epo %>% select(p_key, name, ctry_pat, regio_pat, cross_bord)

## combine EPO and USPTO patents with adjusted origin
adj_commuters_all <- rbind(adj_commuters_us, adj_commuters_epo)

print(paste("All datasets loaded. The origin of", nrow(adj_commuters_all),
            "is going to be adjusted"))

######################################################
###### ADD RE-ASSIGNED ORIGIN OF PATENTS #############
######################################################

inv_reg_CHcommute_adj <- left_join(inv_reg, adj_commuters_all,
                                by = c("p_key", "name"))
if(nrow(inv_reg_CHcommute_adj) != nrow(inv_reg)){
        warning("Number of observations in original and adjusted dataset do not match.")}else{
                print("Origin for patents with cross-border commuters in Switzerland successfully added.")}


inv_reg_CHcommute_adj <- mutate(inv_reg_CHcommute_adj,
                                regio_pat = ifelse(is.na(regio_pat), Up_reg_code, regio_pat),
                                ctry_pat = ifelse(is.na(ctry_pat), Ctry_code, ctry_pat),
                                cross_bord = ifelse(is.na(cross_bord), "no", cross_bord))

print("Origin of patents filed by cross-border commuters in Switzerland successfully re-assigned.")

######################################################
###### SAVE THE ORIGIN-CORRECTED DATASET #############
######################################################

#saveRDS(inv_reg_CHcommute_adj, paste0(mainDir1, "/created data/inv_reg_CHcommute_adj.rds"))
print("Datset saved as 'inv_reg_CHcommute_adj.rds'")

