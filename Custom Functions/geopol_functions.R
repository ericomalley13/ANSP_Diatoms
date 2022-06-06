#x - sites object from `get_downloads()`

geopol_form <- function(x) {
  allids <- getids(x)
  siteids <- unique(allids$siteid)
  return(siteids)
}

###############################################################################

##USE OUTPUT FROM ABOVE FUNCTION INTO NEXT FUNCTION
##example: y <- geopol_form(x)
########## geopol(y)


geopol_state <- function(x) {
  state <- purrr::map(x, function(y) {
    site <- get_sites(y)
    dls <- get_downloads(site)
    dls[[1]]@geopolitical[[1]][[2]]
  })
  as.data.frame(x,(unlist(state))) %>%
    rename(siteid = x) %>%
    print()
}

geopol_county <- function(x) {
  county <- purrr::map(x, function(y) {
    site <- get_sites(y)
    dls <- get_downloads(site)
    dls[[1]]@geopolitical[[1]][[3]]
  })
  as.data.frame(x,(unlist(states))) %>%
    rename(siteid = x) %>%
    print()
}

###############################################################################

#function to add states to samples output dataframe
# x = geopol() output
# y = samples() output (or any dataframe with siteid)
sampdf <- function(x,y) {
  x$states <- row.names(x)
  merge(y, x, by = 'siteid')
}