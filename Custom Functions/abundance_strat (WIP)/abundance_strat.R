# Section 1
# code obtained from "R for Paleolimnology"
# produces stratigraphic diagram of depth (y) vs. abundance (x) of taxa above a specified abundance in ONE site.

###################################################################DATA PREP
yeah <- get_datasets(datasettype = "diatom", limit = 1) %>%
  get_downloads() %>%
  samples()

yeah2 <- data.frame(yeah$depth, yeah$age, yeah$variablename, yeah$value)

colnames(yeah2) <- c("depth", "age", "taxa", "count")

yeah3 <- yeah2 %>%
  group_by(depth) %>%
  mutate(relative_abundance_percent = count / sum(count) * 100)

yeah_common_taxa <- yeah3 %>%
  group_by(taxa) %>%
  summarise(max_rel_abund = max(relative_abundance_percent)) %>%
  filter(max_rel_abund >= 2) %>%
  arrange(max_rel_abund) %>%
  pull(taxa)

yeah_counts_common <- yeah3 %>%
  filter(taxa %in% yeah_common_taxa) %>%
  mutate(taxa = factor(taxa, levels = yeah_common_taxa)) %>%
  arrange(taxa)


######################################################################PLOT

species_plot_obj <- ggplot(
  yeah_counts_common, 
  aes(y = depth, x = relative_abundance_percent)
) +
  # draw horizontal lines of the appropriate length for each depth
  geom_segment(aes(xend = 0, yend = depth), lwd = 1) +
  # facet by taxon, keeping distance on the x axis comparable
  # between facets
  facet_grid(~taxa, scales = "free_x", space = "free_x") +
  # reverse the y axis for depth
  scale_y_reverse() +
  # make all facets use the same break values
  # (helps with cluttered breaks)
  scale_x_continuous(breaks = c(0,2,4,6,8)) +
  # set the x and y labels
  labs(x = "Relative Abundance (%)", y = "Depth (cm)") +
  # customize the appearance
  theme(
    # rotate the facet labels
    strip.text.x = element_text(angle = 60, hjust = 0, vjust = 0), 
    # turn off the label background
    strip.background = element_blank()
  )

# voodoo that makes it so that facet labels can overlap
# https://stackoverflow.com/questions/49740215/ggplot-facet-grid-label-cut-off
species_plot_grob <- ggplotGrob(species_plot_obj)
for(i in which(grepl("strip-t", species_plot_grob$layout$name))){
  species_plot_grob$grobs[[i]]$layout$clip <- "off"
}

# needed to draw the modified plot_grob
grid::grid.draw(species_plot_grob)



#########################################
#########################################
#########################################
#########################################
#########################################

# Section 2
# code obtained from "R for Paleolimnology"
# goal: produce stratigraphic diagram of age (y) vs. abundance (x) of a single taxa of MULTIPLE sites.

###################################################################DATA PREP
um <- get_datasets(datasettype = "diatom", limit = 5) %>%
  get_downloads() %>%
  samples()

um2 <- data.frame(um$depth, um$age, um$variablename, um$value)

colnames(um2) <- c("depth", "age", "taxa", "count")

um3 <- um2 %>%
  group_by(age) %>%
  mutate(relative_abundance_percent = count / sum(count) * 100)

um_common_taxa <- um3 %>%
  group_by(taxa) %>%
  summarise(max_rel_abund = max(relative_abundance_percent)) %>%
  filter(max_rel_abund >= 2) %>%
  arrange(max_rel_abund) %>%
  pull(taxa)

um_counts_common <- um3 %>%
  filter(taxa %in% um_common_taxa) %>%
  mutate(taxa = factor(taxa, levels = um_common_taxa)) %>%
  arrange(taxa)


---------------------------------------------------------------------------
get_datasets(46241)
get_datasets(46242)
get_datasets(46233)

one <- get_datasets(46241) %>%
  get_downloads() %>%
  samples()

two <- get_datasets(46242) %>%
  get_downloads() %>%
  samples()

three <- get_datasets(46233) %>%
  get_downloads() %>%
  samples()



unique(one$variablename)
unique(two$variablename)
unique(three$variablename)

x1 <- unique(one$variablename)
x2 <- unique(two$variablename)
x3 <- unique(three$variablename)
Reduce(intersect, list(x1, x2, x3))

one_1 <- one %>%
  dplyr::filter(variablename == "Melosira ambigua")

two_1 <- two %>%
  dplyr::filter(variablename == "Melosira ambigua")

three_1 <- three %>%
  dplyr::filter(variablename == "Melosira ambigua")





all <- c(46241,46242,46233)

all_ambigua <- get_datasets(all) %>%
  get_downloads() %>%
  pc_abun() %>%
  dplyr::filter(taxa == "Melosira ambigua")


um2 <- data.frame(all_ambigua$depth, all_ambigua$age, all_ambigua$sitename, all_ambigua$value)

colnames(um2) <- c("depth", "age", "site", "count")

hmm <- um2 %>% 
  gather(-age, -depth, key = site, value = count) %>%
  group_by(age) %>%
  mutate(relative_abundance_percent = count / sum(count) * 100) %>%
  ungroup()


um3 <- um2 %>%
  group_by(age) %>%
  mutate(relative_abundance_percent = count / sum(count) * 100)





species_plot_ob <- ggplot(
  all_ambigua, 
  aes(y = age, x = count_pct)
) +
  # draw horizontal lines of the appropriate length for each depth
  geom_segment(aes(xend = 0, yend = age), lwd = 1) +
  # facet by taxon, keeping distance on the x axis comparable
  # between facets
  facet_grid(~sitename, scales = "free_x", space = "free_x") +
  # reverse the y axis for depth
  scale_y_reverse() +
  # make all facets use the same break values
  # (helps with cluttered breaks)
  scale_x_continuous(breaks = c(0, 1, 5, 15, 25)) +
  # set the x and y labels
  labs(x = "Relative Abundance (%)", y = "age (years before 1950)", title = "Melosira ambigua") +
  theme_bw() +
  # customize the appearance
  theme(
    # rotate the facet labels
    strip.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0),
    #center title
    plot.title = element_text(hjust = 0.5),
    # turn off the label background
    strip.background = element_blank()
  )

# voodoo that makes it so that facet labels can overlap
# https://stackoverflow.com/questions/49740215/ggplot-facet-grid-label-cut-off
species_plot_grob <- ggplotGrob(species_plot_ob)
for(i in which(grepl("strip-t", species_plot_grob$layout$name))){
  species_plot_grob$grobs[[i]]$layout$clip <- "off"
}

# needed to draw the modified plot_grob
grid::grid.draw(species_plot_grob)
