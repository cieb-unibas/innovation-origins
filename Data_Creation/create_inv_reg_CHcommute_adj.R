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
adj_commuters_us <- adj_commuters_us %>% select(p_key, name, ctry_pat, regio_pat, cross_bord) %>% mutate(pat_off = 0)

## EPO patents with adjusted origin:
adj_commuters_epo <- readRDS(paste0(mainDir1, "/created data/inv_reg_adj_commuters_ep.rds"))
adj_commuters_epo <- adj_commuters_epo %>% select(p_key, name, ctry_pat, regio_pat, cross_bord) %>% mutate(pat_off = 1)

## combine EPO and USPTO patents with adjusted origin
adj_commuters_all <- rbind(adj_commuters_us, adj_commuters_epo)

## Keep only one p_key/name combination. Since the EP-data should be better matched to regions, we keep the EP-data for equivalent patents
adj_commuters_all <- setDT(adj_commuters_all)[order(-pat_off), .SD, .(p_key)]
adj_commuters_all <- adj_commuters_all %>% distinct(p_key, name, .keep_all = TRUE)

print(paste("All datasets loaded. The origin of", nrow(adj_commuters_all),
            "observations is going to be adjusted"))

###################################################################
###### ADD INFORMATION ON PATENTS THAT ARE REASSIGNED #############
###################################################################


firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg.rds"))
firm_reg <- dplyr::rename(firm_reg, firm_ctry = country) %>% dplyr::select(p_key, )

## merge cross-border commuter information to original sample
inv_reg_CHcommute_adj <- left_join(inv_reg, adj_commuters_all, by = c("p_key", "name"))
inv_reg_CHcommute_adj <- mutate(inv_reg_CHcommute_adj, pat_off_name = substr(patent_id, 1, 2))

## For equivalent patents use inventor country of EPO since patent texts from EPO seems to have less errors
inv_reg_CHcommute_adj <- setDT(inv_reg_CHcommute_adj)[order(pat_off_name), .SD, .(p_key)]
inv_reg_CHcommute_adj <- setDT(inv_reg_CHcommute_adj)[, Ctry_code := Ctry_code[1], .(p_key, name)]

if(nrow(inv_reg_CHcommute_adj) != nrow(inv_reg)){
        warning("Number of observations in original and adjusted dataset do not match.")}else{
                print("Origin for patents with cross-border commuters in Switzerland successfully added.")}

## adopt original region and country for non cross-border commuters
inv_reg_CHcommute_adj <- mutate(inv_reg_CHcommute_adj,
                                regio_pat = ifelse(is.na(regio_pat), Up_reg_label, regio_pat),
                                ctry_pat = ifelse(is.na(ctry_pat), Ctry_code, ctry_pat),
                                cross_bord = ifelse(is.na(cross_bord) | ctry_pat == Ctry_code, "no", cross_bord))
## rename region and country of inventor
inv_reg_CHcommute_adj <- dplyr::rename(inv_reg_CHcommute_adj, ctry_inv = Ctry_code, regio_inv = Up_reg_label)


inv_reg_CHcommute_adj <- dplyr::select(inv_reg_CHcommute_adj, -pat_off)
print("Origin of patents filed by cross-border commuters in Switzerland successfully re-assigned.")


######################################################
###### SAVE THE ORIGIN-CORRECTED DATASET #############
######################################################

saveRDS(inv_reg_CHcommute_adj, paste0(mainDir1, "/created data/inv_reg_CHcommute_adj.rds"))
print("Datset saved as 'inv_reg_CHcommute_adj.rds'")


## Some first inspections
inv_reg_CHcommute_adj <- readRDS(paste0(mainDir1, "/created data/inv_reg_CHcommute_adj.rds"))

# inv_reg_CHcommute_adj <- setDT(inv_reg_CHcommute_adj)[, share := 1/.N, .(p_key, tech_field)]
# test <- aggregate(share ~ cross_bord + ctry_pat + tech_field + p_year, data = inv_reg_CHcommute_adj, FUN = sum)
# test <- dcast(test, p_year + ctry_pat + tech_field ~ cross_bord, value.var = c("share"))
# test <- mutate(test, share = (no+yes) / no)
# 
ggplot(filter(test, ctry_pat == "CH" & p_year < 2017 & tech_field %in% c(16, 28, 30)), aes(x = p_year, y = share, color = as.factor(tech_field), group = as.factor(tech_field))) +
        geom_point() +
        geom_line()

## Some checks why share in pharma declines strongly after 2010
temp <- filter(inv_reg_CHcommute_adj, tech_field == 16 & ctry_pat %in% c("FR", "DE", "CH") & p_year %in% seq(2010, 2017, 1))
firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_", tech_field_start, ".rds")) %>% 
        dplyr::select(p_key, organization, lat, lng, country, Up_reg_label)

temp <- left_join(temp, firm_reg, by = c("p_key"))


