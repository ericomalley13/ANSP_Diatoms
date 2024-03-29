#Using the `plotLeaflet()` function definition in `neotoma2` and removing clustering. 

#' @title plotLeaflet
#' @description Plot sites on a leaflet map
#' @param object Sites object to plot
#' @param save_im save output
#' @param path location where output should be saved in. save_im must be TRUE
#' @export
setMethod(f = "plotLeaflet",
          signature = "sites",
          definition = function(object, save_im=FALSE, path = "") {
            df1 <- map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@geography)[, 2]),
                               long = mean(st_coordinates(x@geography)[, 1]),
                               elev = x@altitude,
                               description = x@description)
            }) %>%
              bind_rows()
            map1 <- leaflet(df1) %>%
              addProviderTiles(providers$Stamen.TerrainBackground) %>%
              addTiles() %>%
              addCircleMarkers(lng = df1$long, lat = df1$lat, radius = 0.5, opacity = 1,
                               popup = paste0("<b>", df1$sitename,
                                              "</b><br><b>Description:</b> ",
                                              df1$description,
                                              "<br><a href=http://apps.neotomadb.org/explorer/?siteids=",
                                              df1$siteid,
                                              ">Explorer Link</a>"),
                               options = markerOptions(riseOnHover = TRUE))
            
            if (save_im == TRUE) {
              mapshot(map1, file = path)
            }
            map1
          })
# End plot methods

#' @title plotLeaflet
#' @description Plot a site on a leaflet map
#' @param object Site object to plot
#' @param save_im save output
#' @param path location where output should be saved in. save_im must be TRUE
#' @export
setMethod(f = "plotLeaflet",
          signature = "site",
          definition = function(object, save_im=FALSE, path = "") {
            
            df1 <- data.frame(siteid = object@siteid,
                              sitename = object@sitename,
                              lat = mean(st_coordinates(object@geography)[, 2]),
                              long = mean(st_coordinates(object@geography)[, 1]),
                              elev = object@altitude,
                              description = object@description)
            
            map1 <- leaflet(df1) %>%
              addProviderTiles(providers$Stamen.TerrainBackground) %>%
              addTiles() %>%
              addCircleMarkers(lng = df1$long, lat = df1$lat, radius = 0.5, opacity = 1,
                               popup = paste0("<b>", df1$sitename,
                                              "</b><br><b>Description:</b> ",
                                              df1$description,
                                              "<br><a href=http://apps.neotomadb.org/explorer/?siteids=",
                                              df1$siteid,
                                              ">Explorer Link</a>"),
                               options = markerOptions(riseOnHover = TRUE))
            
            if (save_im == TRUE) {
              mapshot(map1, file = path)
            }
            map1
          })
# End plot methods