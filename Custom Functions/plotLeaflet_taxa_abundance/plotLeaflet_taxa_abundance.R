#object - sites object from `get_sites`, `get_datasets`, `get_downloads`

#df_with_count_pct - a dataframe that has the % abundance in a column named "count_pct."
##The output from custom function `pc_abun()` will give this dataframe

#title - title of leaflet map

#save_im - to save the plot
#path - the path to save the plot if `save_im` is equal to TRUE. Default path is cwd.

plotLeaflet_taxa <- function(object, df_with_count_pct, title = "Taxa % Abundance", save_im=FALSE, path = "") {
            df1 <- map(object@sites, function(x) {
              df <- data.frame(siteid = x@siteid,
                               sitename = x@sitename,
                               lat = mean(st_coordinates(x@geography)[, 2]),
                               long = mean(st_coordinates(x@geography)[, 1]),
                               elev = x@altitude,
                               description = x@description)
            }) %>%
              bind_rows()
            numPal <- colorNumeric('viridis', df_with_count_pct$count_pct)
            symbols <- makeSizeIcons(values = df_with_count_pct$count_pct,
                                     shape = 'diamond',
                                     color = 'black',
                                     pal = numPal,
                                     opacity = .5,
                                     baseSize = 4)
            map1 <- leaflet(df1) %>%
              addProviderTiles(providers$Stamen.TerrainBackground) %>%
              addTiles() %>%
              addMarkers(icon = symbols, lng = df_with_count_pct$long, lat = df_with_count_pct$lat,
                               popup = paste0("<b>", df1$sitename,
                                              "</b><br><b>Description:</b> ",
                                              df1$description,
                                              "<br><a href=http://apps.neotomadb.org/explorer/?siteids=",
                                              df1$siteid,
                                              ">Explorer Link</a>"),
                               options = markerOptions(riseOnHover = TRUE)) %>%
              addLegendSize(values = df_with_count_pct$count_pct,
                pal = numPal,
                title = title,
                baseSize = 4,
                shape = 'diamond',
                orientation = 'horizontal',
                opacity = .5,
                fillOpacity = .3,
                position = 'bottomright',
                breaks = 5)
            
            if (save_im == TRUE) {
              mapshot(map1, file = path)
            }
            map1
          }