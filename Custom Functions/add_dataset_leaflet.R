add_dataset_leaflet <- function(ids, previousmap, circlecolor) {
  
  sites_ids <- get_sites(ids)
  
  form <- map(sites_ids@sites, function(x) {
    df <- data.frame(siteid = x@siteid,
                     sitename = x@sitename,
                     lat = mean(st_coordinates(x@geography)[, 2]),
                     long = mean(st_coordinates(x@geography)[, 1]),
                     elev = x@altitude,
                     description = x@description)
  }) %>%
    bind_rows()
  
  leaflet_map <- previousmap %>%
    addCircleMarkers(data = form, lng = form$long, lat = form$lat, radius = 0.5, color = circlecolor, opacity = 1,
                     popup = paste0("<b>", form$sitename,
                                    "</b><br><b>Description:</b> ",
                                    form$description,
                                    "<br><a href=http://apps.neotomadb.org/explorer/?siteids=",
                                    form$siteid,
                                    ">Explorer Link</a>"),
                     options = markerOptions(riseOnHover = TRUE))
  return(leaflet_map)
}