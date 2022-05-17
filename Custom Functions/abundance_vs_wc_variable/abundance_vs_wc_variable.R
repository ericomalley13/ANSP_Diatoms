#wc_ids - water chemistry dataset ids
#diat_ids - diatom surface sample data set ids (that correspond to the water chemistry data)
#taxa_name - Genus species of taxa as it appears in samples output (put in quotes)
#variable - the water chemistry variable name that appears in variablename in samples output)
#title - title of plot

wc_plot <- function(wc_ids,diat_ids,taxa_name = "Eunotia exigua", variable = "Total Phosphorus (unfiltered)", title = "Phosphorus") {
  wc_samp <- get_datasets(wc_ids) %>%
    get_downloads() %>%
    samples()
  
  wc_samp_filt <- wc_samp %>%
    dplyr::filter(variablename == variable)
  
  wc_df <- data.frame(wc_samp_filt$datasetid, wc_samp_filt$siteid, wc_samp_filt$sitename, wc_samp_filt$units, wc_samp_filt$value, wc_samp_filt$variablename)
  
  colnames(wc_df) <- c("datasetid","siteid", "sitename", "units", "value", "variablename")
  
  wc_df <- arrange(wc_phos_df,siteid)
  
  
  diat_abun <- get_datasets(diat_ids) %>%
    get_downloads() %>%
    pc_abun()
  
  diat_taxa <- diat_abun %>%
    dplyr::filter(taxa == taxa_name)
  
  #dplyr::filter(taxa == "Aulacoseira lirata" | taxa == "Melosira lirata")
  #dplyr::filter(taxa == "Aulacoseira granulata" | taxa == "Melosira granulata")
  #dplyr::filter(taxa == "Melosira granulata")
  #filter(, grepl("crotonensis", taxa, fixed = TRUE))
  #dplyr::filter(taxa == "Eunotia exigua")
  
  diat_taxa <- arrange(diat_taxa,siteid)
  diat_taxa_1 <- aggregate(diat_taxa$count_pct ~ diat_taxa$siteid + diat_taxa$sitename + diat_taxa$lat + diat_taxa$long, data = diat_taxa, FUN = mean)
  
  test <- subset(wc_phos_df, wc_phos_df$siteid %in% diat_taxa$siteid)
  test2 <- subset(diat_taxa, diat_taxa$siteid %in% test$siteid)
  test3 <- aggregate(test2$count_pct ~ test2$siteid + test2$sitename + test2$lat + test2$long, data = test2, FUN = mean)
  
  test3 <- arrange(test3,`test2$siteid`)
  test <- arrange(test,siteid)
  
  x_ph <- test$value
  y_abun <- test3$`test2$count_pct`
  
  variable_vs_abun_ <- plot(x_ph,y_abun, xlab = variable, ylab = "% Abundance", main = title)
}