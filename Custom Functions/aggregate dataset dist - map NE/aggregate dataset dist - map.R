### Modified `plotLeaflet()` functions
plotLeaflet_unclustered <- function(object, path = "") {
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
  return(map1)
}

#data obtained from Diatom Aggregate Dataset
#ids = siteids
#previous map = leaflet object (results from plotLeaflet)
#circlecolor = hex color in quotes Ex:
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

#Agg #15
#Handle: NLA-2007
library(readxl)
NLA_states_excel <- read_excel("NLA_states_excel.xlsx", 
                               col_types = c("numeric", "text", "text", 
                                             "text", "numeric", "text", "numeric", 
                                             "text", "text", "text", "text", "text", 
                                             "text"))

NE_NLA <- dplyr::filter(NLA_states_excel, State %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "New York", "New Jersey", "Rhode Island", "Vermont"))

NE_NLA_ids <- NE_NLA$`Neotoma Site ID`

NE_NLA_ids <- as.numeric(NE_NLA_ids)

NE_NLA_sites <- get_sites(NE_NLA_ids)

#using modified plotLeaflet, found locally in "Custom Functions"
NE_map <- plotLeaflet_unclustered(NE_NLA_sites)

#data obtained from Diatom Aggregate Dataset
#Agg #1
#Handle: EMAP_NE1

EMAP_NE1_ids <- c(26224,26225,26226,26227,26228,26229,26230,26231,26232,26233,26234,24385,23285,26235,26236,26237,26238,26239,26240,26241,26242,26243,26244,26245,26246,26247,26248,26249,26250,26251,26252,26253,26254,26255,26256,26257,26258,26259,26260,26261,24443,26262,26263,26264,26265,26266,23369,26267,13108,26268,23379,26269,23382,24289,26270,26271,26272,26273,13232,26274,26275,26276,26277,26278,26279,26280,26281,26282,26343,26344,26345,26346,25914,26347,26348,25916,25917,26349,26350,26351,26352,26353,26354,26355,26356,26357,26358,26359,26360,26361,26362,26363,26364,26365,26366,26367,26368,26369,26370,26371,26372,26373,26374,26375,26376,26377,26378,26379,26380,26381,26382,26383,26384,26385,26386,26387,26388,26389,26390,26391,26392,26393,22384,26394,26395,26396,26397,26398,26399,25935,26400,26401,26402,26403,26404,26405,26406,26407,26408,26409,26410,26411,26412,26413,26414,26415,26416,26417,26418,13105,26419,26420,26421,26422,26423,26424,26425,26426,26427,26428,26429,24649,26430,26431,26432,26433,26434,26435,26436,26437,26438,26439,26440,26441,13144,26442,26443,26444,26445,26446,26447,26448,26449,26450,26451,26452,26453,26454,26455,26456,26457,26458,26459,26460,26461,26462,26463,26464,26465,26466,26467,26468,26469,26470,26471,26472,24739,26473,26474,26475,26476,26477,26478,26479,24752,26480,26481,26482,26483,26484,26485,26486,26487,26488,24361,26489,26490,26491,26492,26493,26494,26495,26496,26497,26498,26499,26500,26501,26502,13056,26503)

NE_map <- add_dataset_leaflet(EMAP_NE1_ids, previousmap = NE_map, circlecolor = '#ff3333')

#data obtained from Diatom Aggregate Dataset
#Agg #4
#Handle: ADK-CAL

ADK_CAL_ids <- c(17800,17990,720,14223,18648,18650,18688,710,711,712,23410,713,716,715,717,718,721,722,723,724,725,726,727,728,729,730,736,737,739,740,741,742,743,734,22911,17914,735,714,732,24460,744,746,24431,24432,24466,24433,24439,24442,24445,24458,24459,24463,24495,24435,24477,745,24436,24437,24438,24440,24443,24444,24446,24447,24456,24448,24451,24452,24453,24454,24455,24462,24464,24465,24467,24483,24482,24484,24486,24485,24489,24492,24494,13539,24497,24473,24475,24476,24478,24479,16574,24481)

NE_map <- add_dataset_leaflet(ADK_CAL_ids, previousmap = NE_map, circlecolor = '#ffff00')

#data obtained from Diatom Aggregate Dataset
#Agg #9
#Handle: NENGPIRL

NENGPIRL_ids <- c(25899,25900,25901,25902,25903,13048,25904,24185,25905,25906,25907,25908,25909,25910,25911,25912,18865,25913,25914,25915,25916,25917,25918,25919,25920,25921,25922,25923,25924,25925,25926,25927,25928,18872,25929,25930,25931,1615,25932,25933,25934,25935,25936,25937,25938,25939,25940,25941,25942,25943,25944,25945,25946,25947,25948,25949,25950,25951,25952,25953,25954,25955,25956)

NE_map <- add_dataset_leaflet(NENGPIRL_ids, previousmap = NE_map, circlecolor = '#00cc00')

#data obtained from Diatom Aggregate Dataset
#Agg #24, 26 (25 missing from combined ?)
#Handle: NJ-PAL10, NJ_PAL14

NJ_PAL10_ids <- c(13158,13160,13163,13165,13167,13170,13172,13174,13176,13178,13181,13182,13183,13190,13191,13192,13193,13194,13195,13196,13197,13198,13199,13200,13201,13202,13203,13204,13205,13206,13207,13208,13209,13210,13211,13212,13213,13214,13215,13216,13217,13218,13219,13220,13222,13223,13224,13225,13226)

NJ_PAL14_ids <- c(13241,13232,13242,13178,13243,13244,13202,13203,13209,13245,13246,13215,13247,13219,13223,13224,13225,13248,13249,13250,13251,13252,13253,13254,13255,13256,13257,13258,953,13259,13261,13263,13264,13265,13266,13267,13268,13269,13270,13271,13272,13273)

NJ_ids <- c(NJ_PAL10_ids,NJ_PAL14_ids)

NE_map <- add_dataset_leaflet(NJ_ids, previousmap = NE_map, circlecolor = '#9900ff')


#data obtained from Diatom Aggregate Dataset
#Agg #23
#Handle: GG-PONDS
GGreener_ids <- c(13317,13318,13319,13320,13321,13322,13323,13324,13325,13326,13327,13328,13329,13330)

NE_map <- add_dataset_leaflet(GGreener_ids, previousmap = NE_map, circlecolor = '#ff9900')


#data obtained from Diatom Aggregate Dataset
#Agg #32, 33, 34, 35, 36
#Handle: VT-REF11, VT-NLA12, VT-LA13, VT-1W14, VT-OL18
VT_ids <- c(8587,24372,24369,24363,24362,24360,24368,24367,24366,24370,24365,24371,24364,25901,13132,27816,27817,27818,24369,24417,27819,27820,13049,27821,27822,27823,13142,27824,27825,27829,26467,27826,27827,24417,27828,27796,27797,27798,27799,24583,27800,26449,27801,26224,27716,27717,27718,27719,27720,27721,27722,26274,24417,27723,27724,27725,13049,27726,24355,27727,27728,27729,27730,27731,24537,27732,24356,27733,13580,27734,27735,23152,27736,27737,24706,27738,27739,27740,27741)

NE_map <- add_dataset_leaflet(VT_ids, previousmap = NE_map, circlecolor = '#000000')


#data obtained from Diatom Aggregate Dataset
#Agg #21
#Handle: NJ-NY-PL

NJ_NY_PL_ids <- c(13027,13067,13028,13029,13030,13031,13032,13033,13034,13035,13070,13036,13071,13037,13038,13074,13039,13072,13075,13076,13078,13080)

NE_map <- add_dataset_leaflet(NJ_NY_PL_ids, previousmap = NE_map, circlecolor = '#00ffff')

#data obtained from Diatom Aggregate Dataset
#Agg #22
#Handle: NE-PALEO

NE_PALEO_ids <- c(13045,13046,13047,13048,13049,13050,13051,13052,1688,13053,13054,13055,13056,13057,13058,13061,13084,13087,1697,13090,13093,13095,13096,13097,13098,13099,13100,13101,13102,13103,13104,13105,13106,13107,13108,13109,13110,13111,13112,13113,13114,13119,13120,13122,13124,13126,13128,13130,13131,13132,13133,13140,13141,13142,13143,13144,13145,13146,13148,13150,13152,13154)

NE_map <- add_dataset_leaflet(NE_PALEO_ids, previousmap = NE_map, circlecolor = '#663300')

#data obtained from Diatom Aggregate Dataset

NE_map <- NE_map %>% addLegend("bottomright", 
                               #colors used in circle markers above
                               colors =c("#03F", "#ff3333", "#ffff00", "#00cc00", "#9900ff", '#ff9900', '#000000', '#00ffff', '#663300'),
                               labels= c("NLA-2007","EMAP_NE1","ADK-CALIBR","NENG-PIRLA","NJ-PALEO", "GG-PONDS", "VT-PALEO", "NJ-NY-PALEO", "NE-PALEO"),
                               title= "NE Diatom Surface Samples",
                               opacity = 1)

