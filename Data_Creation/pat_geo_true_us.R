#######################################################################
print("Calculate true place of  where innovations happen/ CR 21.8.2020")
#######################################################################

require("data.table")
require("ggplot2")
require("dplyr")
require("geosphere")
require("fuzzyjoin")
require("plyr")
require("stringr")
require("stringdist")
require("RecordLinkage")
require("tm")

mainDir1 <- c("/scicore/home/weder/GROUP/Innovation/01_patent_data")

## Initial parameters

# Determine the tech_field manually 
tech_field_start_index <- c(1, 35) ## calculation is done from first tech_field (input one) to last tech_field (input two) 

tech_field_start <- 16
# Determine the tech_field using slurm
# args <- commandArgs(TRUE)
# tech_field_start_index <- c(as.numeric(as.character(args[1])), as.numeric(as.character(args[2])))


## Background: -------------------------------
# Load data created in Scripts No. 20 (regional of inventors) and 21 (region of firms): 
# We only use USPTO patents, because for EPO patents we do not have longitudes or latitudes. 
# So the results can only be seen as a representative sample. 
# => WE COULD DISCUSS AN EXTENSION TO EPO PATENTS however

cross_bord_func <- function(tech_field_start, ctry_firm_start = c("CH"), inv_ctry = c("DE", "FR", "IT", "AT", "LI")){

# Load inventor data, determine number of inventors per p_key 
# and drop inventors that are listed more than once per p_key
inv_reg_us <- readRDS(paste0(mainDir1, "/created data/inv_reg/inv_reg_us_", tech_field_start, ".rds")) %>%
  dplyr::select(p_key, name, inventor_id, lat, lng, ctry_code, Up_reg_label, patent_id)
inv_reg_us <- filter(inv_reg_us, ctry_code %in% inv_ctry)
inv_reg_us <- setDT(inv_reg_us)[, num_inv := .N, .(p_key)]
inv_reg_us <- distinct(inv_reg_us, p_key, name, .keep_all =  T)

# Load firm data and drop firms that are listed more than once per p_key
firm_reg_us <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_us_", tech_field_start, ".rds")) %>% 
  dplyr::select(p_key, organization, lat, lng, country, Up_reg_label)
firm_reg_us <- distinct(firm_reg_us, p_key, organization, lat, lng, .keep_all = T)

## Since some firms have unknown country in US data, we use EPO data to get country for patents applied at the US and EPO
firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_", tech_field_start, ".rds")) %>% 
  dplyr::select(p_key, organization, lat, lng, country, Up_reg_label)
firm_reg <- filter(firm_reg, is.na(lat) == T & is.na(lng) == T) %>% dplyr::select(-lat, -lng)

## Do the following calculation only for US patents with country unknown or not existent
firm_reg_us_temp <- filter(firm_reg_us, country == "un" | is.na(country) == T)
firm_reg_us_temp <- setDT(firm_reg_us_temp)[, num_firm := .N, .(p_key)]
firm_reg_us_temp <- left_join(firm_reg_us_temp, firm_reg, by = c("p_key"))
firm_reg_us_temp <- mutate(firm_reg_us_temp, name_dist = stringdist(organization.x, organization.y)) %>% mutate(name_dist = ifelse(is.na(name_dist), 0, name_dist))
firm_reg_us_temp <- setDT(firm_reg_us_temp)[order(name_dist), .SD, p_key]
firm_reg_us_temp <- setDT(firm_reg_us_temp)[, .SD[1:num_firm], p_key]
firm_reg_us_temp <- mutate(firm_reg_us_temp, country = country.y, Up_reg_label = Up_reg_label.y, organization = organization.x, p_key_org = paste0(p_key, organization.x))

## Combine adjusted and not-adjusted data together
firm_reg_us <- mutate(firm_reg_us, p_key_org = paste0(p_key, organization))
firm_reg_us <- filter(firm_reg_us, !(p_key_org %in% firm_reg_us_temp$p_key_org)) %>% dplyr::select(-p_key_org)
firm_reg_us_temp <- dplyr::select(firm_reg_us_temp, p_key, organization, country, Up_reg_label, lat, lng)
firm_reg_us <- rbind(firm_reg_us, firm_reg_us_temp)

## Find crossborder-commuters directly: ----------------------------------------
# Crossborder-commuter status is assigned if 
# (a) An inventor does not live in the same country as the firm is located,
# AND 
# (b) the geographic location of the inventor and the firm are not too far away.

# 1) match firms and inventors, calculate location distance between them and 
#    only consider the closest match.
inv_firm <- inner_join(inv_reg_us, firm_reg_us, by = c("p_key")) 
inv_firm <- mutate(inv_firm, lat_diff = abs(lat.x -lat.y), lng_diff = abs(lng.x - lng.y)) %>%
  filter(is.na(lat_diff) != T) %>% mutate(dist = lat_diff^2 + lng_diff^2, 
                                          max_dist = ifelse(lat_diff < 1.2 & lng_diff < 1.2, 0, 1))
inv_firm <- setDT(inv_firm)[order(dist), .SD[1], .(p_key, inventor_id)] 

# 2) If country of inventor and firm is not identical AND distance is small,
# assign crossborder-commuter status to the inventor/patent
inv_firm <- mutate(inv_firm, 
                   cross_bord = ifelse(ctry_code != country & max_dist == 0, "yes",
                                       ifelse(ctry_code == country, "no", "maybe"))) 
inv_firm <- dplyr::rename(inv_firm, lat = lat.x, lng = lng.x)
inv_firm <- distinct(inv_firm, p_key, organization, inventor_id, lat, lng, .keep_all = T)

## Create function to derive crossborder-commuters for patents with ambiguous information. -------------------
# This is the case for eg Swiss firms that apply for US patents via US affiliations. 
# As a result, firm and inventor(s) are from two different countries but geographically too far away to meet the previous criteria. 
# However, the true invention location (= where R&D took place) would nevertheless be in Switzerland

derive_cross_bord_func <- function(ctry_firm_start, inv_ctry){
  # first input parameter is for the country where the patent has been invented,
  # the second for the countries where the inventors may come from (crossborder-commuters and domestic residence). 
  
  # Get list of firms of a country/region and tech_field
  firm_list <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_", tech_field_start, ".rds")) %>% 
    dplyr::select(p_key, organization, lat, lng, country, Up_reg_label, pub_nbr)
  firm_list <- filter(firm_list, country %in% ctry_firm_start) %>% 
    mutate(organization = tolower(organization))
  firm_list$organization <- gsub('[[:punct:] ]+',' ', firm_list$organization)
  firm_list <- mutate(firm_list, organization = trimws(removeWords(organization, c("ex", "us", "gmbh", "f", "s a", "ac", "sa", "ing", "a g", "co", "corp", "inc", "ltd", "llc", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "pourrecherchemicrotechnique", 
                                                                            "corp", "div", "kaisha", "cie",  "incorporated", "industries", "/", "international", "technologies", "technology", "products", "group", "holdings", "elevator", "china", "germany", 
                                                                            "usa", "engineering", "gmbh", "ing", "corp", "inc", "ltd", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "pourrecherchemicrotechnique", "corp", "div", 
                                                                            "kaisha", "cie",  "incorporated", "industries", "/", "gmbh", "ing", "corp", "inc", "ltd", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "industrietreuhand", 
                                                                            "kommanditgesellschaft", "pourrecherchemicrotechnique", "corp", "div", "kaisha", "cie",  "incorporated", "industries", "/", "research")), which = "both")) 
  
  # use only firms for which we know that they have at least some patents in the country of firm (eg Switzerland)
  firm_list <- setDT(firm_list)[, num := .N, .(organization, Up_reg_label)]
  firm_list <- filter(firm_list, num > 5)
  firm_list <- aggregate(cbind(lat, lng) ~ organization + Up_reg_label, FUN = function(x)mean(x, na.rm = T, na.action = NULL), data = firm_list)
  firm_list <- distinct(firm_list, organization, lat, lng, Up_reg_label)
  
  # subset to those patents where inventor and firm location are not identical (= "maybe"),
  # subset again to inventors with an address in chosen inv_countries (e.g. CH, DE, FR)
  # and subset again to firms that are not in these countries (e.g. US-affiliates)
  inv_firm_maybe <- filter(inv_firm, 
                           cross_bord == "maybe" & ctry_code %in% inv_ctry & 
                             !(country %in% c(ctry_firm_start, inv_ctry))) %>% 
    dplyr::select(organization, ctry_code, p_key, inventor_id, lat, lng) 
  
if(nrow(inv_firm_maybe) != 0){
  inv_firm_maybe$organization <- gsub('[[:punct:] ]+',' ', inv_firm_maybe$organization)
  inv_firm_maybe <- mutate(inv_firm_maybe, organization = trimws(removeWords(tolower(organization), c("ex", "us", "gmbh", "f", "s a", "ac", "sa", "ing", "a g", "co", "corp", "inc", "ltd", "llc", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "pourrecherchemicrotechnique", 
                                                                            "corp", "div", "kaisha", "cie",  "incorporated", "industries", "/", "international", "technologies", "technology", "products", "group", "holdings", "elevator", "china", "germany", 
                                                                            "usa", "engineering", "gmbh", "ing", "corp", "inc", "ltd", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "pourrecherchemicrotechnique", "corp", "div", 
                                                                            "kaisha", "cie",  "incorporated", "industries", "/", "gmbh", "ing", "corp", "inc", "ltd", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "industrietreuhand", 
                                                                            "kommanditgesellschaft", "pourrecherchemicrotechnique", "corp", "div", "kaisha", "cie",  "incorporated", "industries", "/", "research")), which = "both")) 
  
  # assign crossborder-commuter status to "maybe" observations if: 
  # (i) inventor lives not in the same country as location of firm AND 
  # (ii) both geographic locations (firm and inventor) are not too far away. 
  
  # Moreover, using distance among names of firm of inventor and list of Swiss firms as a second criteria. 
  # These two criteria determine the degree of accuracy
  close_firm <- difference_left_join(inv_firm_maybe, firm_list, by = c("lat", "lng"), max_dist = 2)
  close_firm <- filter(close_firm, is.na(organization.y) != T)
  close_firm <- mutate(close_firm, name_diff = stringdist(organization.x, organization.y)) 
  close_firm <- mutate(close_firm, lat_diff = abs(lat.x -lat.y), 
                       lng_diff = abs(lng.x - lng.y), 
                       dist = lat_diff^2 + lng_diff^2) %>% 
    filter(lat_diff < 1.3 & lng_diff < 1.3 & name_diff < 3)
  close_firm <- setDT(close_firm)[order(dist, name_diff), .SD[1], .(p_key, inventor_id)]
  
  if(nrow(close_firm) != 0){
  close_firm <- filter(close_firm, is.na(organization.y) != T) %>% 
    mutate(cross_bord = "yes", ctry_pat = ctry_firm_start) %>% 
    select(inventor_id, organization.x, organization.y, p_key, cross_bord, ctry_pat, Up_reg_label)
  print(paste0("A total of ", nrow(close_firm), " maybes can be attributed as crossborder commuters"))
  return(close_firm)
  } else {
  print("No maybes can be attributed as crossborder commuters")
  return(data.frame(inventor_id = 0, organization.x = 0, organization.y = 0, p_key = 0, cross_bord = 0, ctry_pat = 0, Up_reg_label = 0))
  }
} else {
  print("No maybes can be attributed as crossborder commuters")
  return(data.frame(inventor_id = 0, organization.x = 0, organization.y = 0, p_key = 0, cross_bord = 0, ctry_pat = 0, Up_reg_label = 0))
  
}
} 

derive_cross_bord <- derive_cross_bord_func(ctry_firm_start, inv_ctry) 
derive_cross_bord <- mutate(derive_cross_bord, p_key = as.character(p_key), 
                            inventor_id = as.character(inventor_id))

## Adjustment of maybe-crossborder commuter ------------------------------------ 
# This might be possible, would however require some effort. 
inv_firm <- left_join(inv_firm, derive_cross_bord, by = c("p_key", "inventor_id"))

## Derive the "true" country and region of a patent based on the previously derive
inv_firm <- mutate(inv_firm, 
                   cross_bord = ifelse(cross_bord.y == "yes" & is.na(cross_bord.y) != T, "yes", cross_bord.x),
                   regio_firm = ifelse(is.na(Up_reg_label), Up_reg_label.y, Up_reg_label), 
                   regio_inv = Up_reg_label.x) %>% 
            mutate(ctry_pat = ifelse(ctry_code == country, ctry_code, 
                                     ifelse(ctry_code != country & cross_bord.x == "yes", country, 
                                            ifelse(ctry_code != country & is.na(cross_bord.y) != T, ctry_pat, ctry_code)))) %>%
            mutate(inv_firm, regio_pat = ifelse(regio_firm == regio_inv, regio_firm,
                                                ifelse(regio_firm != regio_inv & cross_bord == "yes", regio_firm, NA)))

inv_firm <- distinct(inv_firm, p_key, inventor_id, .keep_all = T)
inv_firm <- mutate(inv_firm, tech_field = tech_field_start) %>% 
  dplyr::rename(ctry_inv = ctry_code, ctry_firm = country) %>%
  dplyr::select(p_key, organization, inventor_id, name, lat, lng, ctry_inv, ctry_firm, ctry_pat, regio_firm, regio_inv, regio_pat, cross_bord, tech_field)

## Keep only commuters to the country determined by ctry_firm
inv_firm <- filter(inv_firm, cross_bord == "yes" & ctry_pat == ctry_firm_start & ctry_inv != ctry_firm)
inv_firm <- distinct(inv_firm, p_key, inventor_id, name, .keep_all = T)
print(paste0("Calculation for tech_field ", tech_field_start, " done"))
return(inv_firm)
}

## apply previous created function to different technology fields. The tech_fields are determined by the first input of the lapply function
inv_firm_adj <- do.call(rbind.fill, lapply(seq(tech_field_start_index[1], tech_field_start_index[2], 1), function(x) cross_bord_func(x, "CH", c("DE", "FR", "IT", "AT", "LI")))) 
inv_firm_adj %>% saveRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_reg_adj_commuters_us.rds")

## The tech_fields are determined by the first input of the lapply function
## cross_bord_func: first input parameter is for the country where the patent has been invented and the second for the countries where the inventors may come from (crossborder-commuters and domestic residence). 
## Description of variables: 
# ctry_firm: Country of the firm as written in the patent; 
# ctry_inv: country of the place of resindece of the inventor as written in the patent; 
# ctry_pat: Our derived country where the innovation has happened

###################
## Some analysis ##
###################
## Detect patents which are developed only by crossborder-commuters and thus cannot be attributed to the true country of invention (even if each country with at leat one inventor gets a share of one for a patent)
# inv_firm_adj <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/inv_reg_adj_commuters_us.rds")
# 
# inv_firm_adj <- setDT(inv_firm_adj)[, cbind("num_inv", "num_cross") := list(.N, sum(cross_bord == "yes", na.action = NULL)), .(p_key)]
# inv_firm_adj <- mutate(inv_firm_adj, pat_only_crossbord = ifelse(num_inv == num_cross, "yes", "no"))
# 
# ## Create variable to see whether using inventors' or firms' location lead to correct results regarding the "true" location of an invention
# inv_firm_adj <- mutate(inv_firm_adj, correct_ctry_firm = ifelse(ctry_firm == ctry_pat, "yes", "no"), correct_ctry_inv = ifelse(ctry_inv == ctry_pat, "yes", "no"))
# inv_firm_adj <- setDT(inv_firm_adj)[, inv_share := 1/.N, .(p_key)]
# 
# ## Add p_year
# dat_p_year <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/dat_p_year.rds")  %>% mutate(p_key = as.character(p_key)) %>% dplyr::select(p_key, pub_nbr, p_year)
# inv_firm_adj <- left_join(inv_firm_adj, dat_p_year, by = c("p_key"))
# 
# ## Set share = 1 per country and patent
# inv_ctry <- distinct(inv_firm_adj, p_key, ctry_inv, .keep_all = T)
# inv_ctry <- aggregate(inv_share ~ ctry_inv + p_year, FUN = length, data = inv_ctry)
# 
# pat_ctry <- distinct(inv_firm_adj, p_key, ctry_pat, .keep_all = T)
# pat_ctry <- aggregate(inv_share ~ ctry_pat + p_year, FUN = length, data = pat_ctry)
# 
# inv_pat_ctry <- left_join(pat_ctry, inv_ctry, by = c("ctry_pat" = "ctry_inv", "p_year"))
# inv_pat_ctry  <- mutate(inv_pat_ctry, pat_inv_ctry = inv_share.x/(inv_share.y))
# 
# ## Set share equal to the number of inventors per country
# inv_share_correct <- aggregate(inv_share ~ ctry_pat + correct_ctry_inv + p_year + tech_field, FUN = sum, data = inv_firm_adj)
# inv_share_correct <- dcast(inv_share_correct, ctry_pat + p_year + tech_field ~ correct_ctry_inv, value.var = "inv_share")
# inv_share_correct <- mutate(inv_share_correct, share_correct = (ifelse(is.na(no), 0, no) + yes)/yes)
# 
# table(inv_firm_adj$correct_ctry_inv, inv_firm_adj$ctry_pat)
# table(inv_firm_adj$correct_ctry_firm, inv_firm_adj$ctry_pat)
# 
# ggplot(filter(inv_share_correct, ctry_pat %in% c("CH")  & p_year < 2016), aes(x = p_year, y = share_correct, color = tech_field, shape = as.factor(tech_field))) +
#   geom_line() +
#   geom_point() +
#   xlab("Priority Year") +
#   ylab("True location / inventor location")


