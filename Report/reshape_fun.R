#################################################################
# Use this custom helper-function to arrange and reshape patent #
# data on Swiss regions and technology fields for plotting      #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           14.09.2020                                    #
#################################################################


reshape_fun <- function(dataset){
        
        # load the data
        plot_data <- df_list[[dataset]]
        
        # use the same variable names and kick out tech_field No. 22 becuase of missing values
        if(dataset == "CH_Techfields"){
                plot_data <- subset(plot_data, plot_data$tech_field != 22)
                plot_data <- plot_data %>% rename(regio_pat = tech_field)
        }
        
        # calculate 3-year patent count averages for regions and techfields
        plot_data <- plot_data %>% dplyr::arrange(p_year) %>% filter(as.numeric(p_year) <= 2015)
        plot_data <- plot_data %>% group_by(regio_pat) %>%
                mutate(period = rep(seq(1990, 2015, by =3), each = 3)[1:n()])
        plot_data <- plot_data %>% group_by(regio_pat, period) %>%
                summarise(Swiss_based = sum(Swiss_based), commuters = sum(commuters)) %>%
                mutate(share = commuters / Swiss_based) %>%
                rename(p_year = period)
        
        # rename to the original variable names again
        if(dataset == "CH_Techfields"){plot_data <- plot_data %>% rename(tech_field = regio_pat)}
        
        # change type names and adapt the data structure from wide to long format
        plot_data <- plot_data %>%
                rename(`Swiss residents` = Swiss_based, Commuters = commuters) %>%
                gather(key = Type, value = number, `Swiss residents`, Commuters)
        
        # define text-labels for the plots
        plot_data <- plot_data %>% mutate(Status = paste0(Type, "\nCount: ", trunc(number)))
        plot_data <- plot_data %>% mutate(Share = paste0(round(100 * share, 1), "%"))
        
        # return the processed data
        return(plot_data %>% as.data.frame)
}