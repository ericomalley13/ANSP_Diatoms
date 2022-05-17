#dl_object - a sites object returned from `get_downloads()`

pc_abun <- function(dl_object) {
  
  samp <- samples(dl_object)
  taxa <- samp$variablename
  count <- samp$value
  sitename <- samp$sitename
  datasetid <- samp$datasetid
  siteid <- samp$siteid
  sampleid <- samp$sampleid
  lat <- samp$lat
  long <- samp$long
  
  
  countDF <- data.frame(lat, long, siteid, sitename, datasetid, sampleid, taxa, count) %>%
    group_by(sampleid) %>%
    mutate(count_pct = count*100 / sum(count))
  return(countDF)
}