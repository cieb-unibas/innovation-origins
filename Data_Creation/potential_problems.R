# load the data / this takes a bit of time
df <- readRDS(paste0(mainDir1, "/created data/inv_reg_CHcommute_adj.rds"))

################################################################################## 
## PROBLEM 1: There are people with ctry_inv == "CH" AND cross_border status #####
##################################################################################
# How can this be?
tmp <- df %>% filter(cross_bord == "yes", ctry_inv == "CH")
# check the following example: "alexander mayweg"
tmp[tmp$name == "alexander mayweg", ]

# ctry_inv is always Switzerland but he is still considered a cross-border commuter...
# These are EPO patents, but the p_keys are (correctly so) not in the adj_commuters_epo.rds dataset!!
# however, they are contained in the US dataset!! 
# => HOW IS THAT?

# Hypothesis I: 
# It seems to be something with the USPTO script or the merging for USPTO and EPO commuters to inv_reg_adj. 
# Could the USPTO-commuter assignment script assign cross-border status to ALL 'maybe'-p_keys/patents 
# even if the inventor was a cross-border commuter only on ONE patent/p_key? 
# check e.g. "alexander mayweg" who should be a cross-border commuter for p_key's 
# 16261047, 16356724, 16264836 but not for all others.
# => there are too many commuter status == "yes for this person in the adj_commuters_us.rds dataset!

# Hypothessis II:
# GitHub commit CR in create_inv_reg_CHcommute_adj.R :
# 'Keep only one p_key/name combination. Since the EP-data should be better matched to regions, we keep the EP-data for equivalent patents'
# could it be something in this regard? 


# Hypothessis III:
# meanwhile I find this most likely.... PATENT DATA QUALITY IS TERRIBLE SOMETIMES
# see e.g. https://patentimages.storage.googleapis.com/1a/e4/15/efe6d202a1e83b/EP1583742B1.pdf
# and https://patentimages.storage.googleapis.com/1f/23/cd/0403bd525ae8d5/US7294644B2.pdf
# => this is the same thing and we have it classified under the same p_key (but for different tech_fields)
# p_key = 16080355
# however, Mr. alexander mayweg lived in Loerrach according to the USPTO (= which is why we classified him as a cross-border commuter)
# and in Basel for the EPO.
# terrifique...
# cross-bord == "yes" is assigned based on p_key and name/inventor_id. Thus, both times alexander mayweg gets assigned as a commuter

# another example:
# https://patentimages.storage.googleapis.com/8c/fa/a5/06012ae81c4a9e/US8808260.pdf
# According to the USPTO, Ebikon, Luzern is in Italy...
# and thus the guy is assigned as a cross-border commuter

# overall, there are only 219 (0.7%) potential errors. Maybe we just let them be.
paste(nrow(tmp), "patents are potentially wrongly assigned as cross-border commuting inventors")
paste0(round(nrow(tmp) / nrow(df[df$cross_bord == "yes" & df$ctry_pat == "CH", ]),3) * 100, "% of patents potentially misclassified")

########################################################################## 
## PROBLEM 2: Cross-border commuters in non-border regions before 2008 ###
##########################################################################

# according to Beerli et al. (2020) cross-commuting was not possible to regions within Switzerland
# before liberalization in 2007. However, we still find cross-border commuters in Espace Mittelland
# and Central Switzerland for this period.

## Espace Mittelland:
tmp <- df %>% filter(cross_bord == "yes", p_year <= 2007, regio_pat == "Espace Mittelland")
# OECD definition for Espace Mittellland includes Neuchatel, Jura which are border regions.
# => No problems there

## Central Switzerland
tmp <- df %>% filter(cross_bord == "yes", p_year <= 2007, regio_pat == "Central Switzerland")

# There should not be any because all cantons belonging to this OECD region are not close to the border. 
# => potentail problem here:
# The reason why we still find some of them could be a) misclassifications (see problem 1), 
# (b) invention was in a lab close to the border and the patent was only filed from a headquarter in Central Switzerland
# (c) inventors were basically living in Switzerland (e.g. had a B permit) but lived in neighboring countries on
# weekends etc.



