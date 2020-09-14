#########################################################
# Use this file to create the data used for the         #
# report on innovation origins.                         #
# Authors:        Matthias Niggli/CIEB UniBasel         #
#                 Christian Rutzer/CIEB UniBasel        #
#                 Dragan Filimonovic/CIEB UniBasel      #
# Date:           11.08.2020                            #
#########################################################


###########################################
#### LOAD PACKAGES AND SET DIRECTORIES ####
###########################################

library("tidyverse")
library("data.table")
mainDir1 <- c("/scicore/home/weder/GROUP/Innovation/01_patent_data")

print("Packages loaded and directories set.")

#######################################################
############ LOAD AND PROCESS THE DATA ################
#######################################################

# load the data
df <- readRDS(paste0(mainDir1, "/created data/inv_reg_CHcommute_adj.rds"))
ipc <- readRDS(paste0(mainDir1, "/created data/oecd_tech_field.RDS"))

# only consider patents for which at least one inventor assigned to Switzerland
keep_keys <- df %>% filter(ctry_pat == "CH")
keep_keys <- keep_keys$p_key
df <- df[df$p_key %in% keep_keys, ]

# prepare for saving the processed data
dat_list <- list()

############################################################################
#### CROSS-BORDER COMMUTER ADJUSTED PATENT COUNT OF SWITZERLAND OVERALL ####
############################################################################

# some patents are assigned to multiple technology fields. keep them only once
plot_data <- df %>% distinct(p_key, name, .keep_all = TRUE)

# calculate every inventor's share on the patent and filter to Swiss assigned
plot_data <- setDT(plot_data)[, share := 1/.N, .(p_key)]
plot_data <- filter(plot_data, ctry_pat == "CH")

# sum up patent shares for commuters and non-commuters per year and calculate ratio
plot_data <- aggregate(share ~ cross_bord + p_year, data = plot_data, FUN = sum)
plot_data <- dcast(plot_data, p_year ~ cross_bord, value.var = c("share"))
plot_data <- mutate(plot_data, share = (no+yes) / no)
plot_data <- rename(plot_data, Swiss_based = no, commuters = yes)

# store the data
dat_list[["CH_overall"]] <- plot_data

######################################################################################
## CROSS-BORDER COMMUTER ADJUSTED PATENT COUNTS BY TECHNOLOGY FIELDS IN SWITZERLAND ##
######################################################################################

# calculate every inventor's share on the patent and filter to Swiss assigned
plot_data <- setDT(df)[, share := 1/.N, .(p_key, tech_field)]
plot_data <- filter(plot_data, ctry_pat == "CH")

# sum up patent shares for commuters and non-commuters per Tech-field, year and calculate ratio
plot_data <- aggregate(share ~ cross_bord + tech_field + p_year, data = plot_data, FUN = sum)
plot_data <- dcast(plot_data, p_year + tech_field ~ cross_bord, value.var = c("share"))
plot_data$yes <- ifelse(is.na(plot_data$yes), 0, plot_data$yes)
plot_data <- mutate(plot_data, share = (no+yes) / no)
plot_data <- rename(plot_data, Swiss_based = no, commuters = yes)
plot_data$tech_field <- as.character(plot_data$tech_field)

# add names of technology fields
plot_data <- merge(plot_data, ipc, by = "tech_field", all.x = TRUE)
plot_data <- plot_data[, c(2, 1, 3:6)]

# store the data
dat_list[["CH_Techfields"]] <- plot_data

########################################################################
#### CROSS-BORDER COMMUTER ADJUSTED PATENT COUNTS BY SWISS REGIONS #####
########################################################################

# some patents are assigned to multiple technology fields. keep them only once
plot_data <- df %>% distinct(p_key, name, .keep_all = TRUE)

# calculate every inventors share on the patent and filter to Swiss assigned
plot_data <- setDT(plot_data)[, share := 1/.N, .(p_key)]
plot_data <- filter(plot_data, ctry_pat == "CH")

# sum up patent shares for commuters and non-commuters per CH-Region and calculate ratio
plot_data$regio_pat <- trimws(plot_data$regio_pat)
CH_reg <- c("Eastern Switzerland", "Zürich", "Central Switzerland",
            "Lake Geneva Region", "Espace Mittelland", "Ticino", "Northwestern Switzerland")
plot_data <- filter(plot_data, regio_pat %in% CH_reg)
plot_data <- aggregate(share ~ cross_bord + regio_pat + p_year, data = plot_data, FUN = sum)
plot_data <- dcast(plot_data, p_year + regio_pat ~ cross_bord, value.var = c("share"))
plot_data$yes <- ifelse(is.na(plot_data$yes), 0, plot_data$yes)
plot_data <- mutate(plot_data, share = (no+yes) / no)
plot_data <- rename(plot_data, Swiss_based = no, commuters = yes)

# store the data
dat_list[["CH_region"]] <- plot_data

###############################################################################
#### COUNT OF CROSS-BORDER COMMUTERS TO SWITZERLAND BY NEIGHBORING COUNTRY ####
###############################################################################

# some patents are assigned to multiple technology fields. keep them only once
plot_data <- df %>% distinct(p_key, name, .keep_all = TRUE)

# filter to cross-border commuting inventors
plot_data <- filter(plot_data, ctry_pat == "CH" & cross_bord == "yes")

# count the number of commuters per neighboring country
plot_data <- plot_data %>% group_by(ctry_inv, p_year) %>% summarise(count = n())
plot_data$ctry_inv <- trimws(plot_data$ctry_inv)
commut_ctry <- c("FR", "DE", "AT", "IT")
plot_data <- filter(plot_data, ctry_inv %in% commut_ctry)

# store the data
dat_list[["neighboring_ctry"]] <- plot_data

#########################
#### EXAMPLE PATENTs ####
#########################

## EXAMPLE OF CH PATENT BY INVENTORS FROM DIFFERENT COUNTRIES ------------------
plot_data <- df %>%
        group_by(p_key) %>%
        mutate(inventors = n(),
               commuters = paste(cross_bord),
               expl = paste(ctry_inv),
               expl = ifelse("CH" %in% expl & "DE" %in% expl & !"yes" %in% commuters,
                             "yes", "no")) %>%
        filter(expl == "yes", inventors > 4 & p_year > 2005)
print("Choose first examples from these patents")
# => chose patent: "US10029262B2"

## EXAMPLE OF CH PATENT BY CROSS-BORDER COMMUTING INVENTORS: -------------------

# some patents are assigned to multiple technology fields. 
# keep them only once and filter for patents that are assigned to CH
plot_data <- df %>% distinct(p_key, name, .keep_all = TRUE) %>%
        filter(ctry_pat == "CH")

# filter to USPTO pharma patents from 2012 where all inventors are not from CH
USPTOs <- grep("US", plot_data$patent_id)
plot_data$USPTO <- "no"
plot_data[USPTOs, "USPTO"] <- "yes"
plot_data <- plot_data %>% filter(USPTO == "yes") %>% 
        group_by(p_key) %>%
        mutate(CH_connex = ifelse(!"CH" %in% paste(ctry_inv), "no", "yes")) %>%
        filter(CH_connex == "no" & tech_field == 16)
print("Choose 2nd example from these patents")
# => choose patent "US7786128B2"

######################################
#### SAVE THE DATA FOR THE REPORT ####
######################################

for(i in 1:length(dat_list)){
        csv_name <- paste0(names(dat_list[i]), ".csv")
        write.csv(dat_list[[i]],
                  file = paste0(getwd(), "/Report/", csv_name),
                  row.names = FALSE)
        }
print("Data for the analysis sucessfully saved.")
