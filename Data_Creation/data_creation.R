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

# only consider patents with at least one inventor assigned to Switzerland
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

# store the data
dat_list[["CH_overall"]] <- plot_data

# # illustrate
# ggplot(plot_data[plot_data$p_year < 2015, ], 
#        aes(x = p_year, y = share)) +
#         geom_point() +
#         geom_line() +
#         geom_hline(yintercept = 1, color = "black", linetype = "dotted")+
#         ylim(0.75, 1.5)

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
plot_data$tech_field <- as.character(plot_data$tech_field)

# store the data
dat_list[["Techfields"]] <- plot_data

# # illustrate
# ggplot(filter(plot_data, p_year < 2015 & tech_field %in% c(13, 15, 16)), 
#        aes(x = p_year, y = share, color = tech_field, group = tech_field)) +
#         geom_point() +
#         geom_line() +
#         geom_hline(yintercept = 1, color = "black", linetype = "dotted")+
#         ylim(0.75, 1.5)

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


# store the data
dat_list[["CH_region"]] <- plot_data

# # illustrate
# ggplot(filter(plot_data, p_year < 2015), 
#        aes(x = p_year, y = share, color = regio_pat, group = regio_pat)) +
#         geom_point() +
#         geom_line() +
#         geom_hline(yintercept = 1, color = "black", linetype = "dotted")+
#         ylim(0.75, 1.5)

##########################################################################
#### COUNT OF CROSS-BORDER COMMUTERS TO SWITZERLAND BY FOREIGN REGION ####
##########################################################################

# some patents are assigned to multiple technology fields. keep them only once
plot_data <- df %>% distinct(p_key, name, .keep_all = TRUE)

# filter to cross commuters
plot_data <- filter(plot_data, ctry_pat == "CH" & cross_bord == "yes")

# count the number of commuters per foreign region
plot_data <- plot_data %>% group_by(regio_inv, p_year) %>% summarise(count = n())
plot_data$regio_inv <- trimws(plot_data$regio_inv)
commut_regions <- c("Alsace", "Freiburg", "Rhône-Alpes", "Lombardy",
                    # "Stuttgart", "Schwaben", "Lorraine",
                    "Vorarlberg", "Franche-Comté")
plot_data <- filter(plot_data, regio_inv %in% commut_regions)

# store the data
dat_list[["Foreign_region"]] <- plot_data

# # illustrate
# ggplot(filter(plot_data, p_year < 2015), 
#        aes(x = p_year, y = count, color = regio_inv, group = regio_inv)) +
#         geom_point() +
#         geom_line()


######################################
#### SAVE THE DATA FOR THE REPORT ####
######################################

saveRDS(dat_list, paste0(getwd(), "/Report/commuter_data.rds"))
print("Data for the analysis sucessfully saved.")



