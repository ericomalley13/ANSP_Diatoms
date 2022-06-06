#code derived from:
#https://paleolimbot.github.io/r4paleolim/strat-diagrams.html#species-composition-diagrams

#testing getting chronologies (not used in generating plot)
test <- get_datasets(datasettype = "diatom", limit = 4) %>%
  + get_downloads()

chron_test <- chronologies(test)

Binford_1986 <- chron_test[[10]]$chroncontrols

###############################################
#Chronology name: Binford 1986
#Site name: Andrus Lake
#Datasetid: 46241
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
###############################################################
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
  scale_x_continuous(breaks = c(0, 10, 20, 30)) +
  # set the x and y labels
  labs(x = "Relative Abundance (%)", y = "Depth") +
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
ggplot(
  yeah_counts_common, 
  aes(y = depth, x = relative_abundance_percent)
) +
  # draw horizontal lines of the appropriate length for each depth
  geom_segment(aes(xend = 0, yend = depth), lwd = 1) +
  # facet by taxon, keeping distance on the x axis comparable
  # between facets
  facet_grid(~taxa, scales = "free_x", space = "free_x") +
  # have the same breaks for all x axes
  scale_x_continuous(breaks = c(0, 10, 20, 30)) +
  # reverse the y axis for depth
  scale_y_reverse() +
  labs(x = "Relative Abundance (%)", y = "Depth")
#########################################################################
#formatting

two <- yeah2 %>%
  select(taxa,count,depth) %>%
  t()

library("janitor")

three <- two %>%
  row_to_names(row_number = 1)



depth_test <- unique(yeah2$depth)
age_test <- unique(yeah2$age)


Binford_1986 <- yeah2 %>%
  gather(-age, -depth, key = taxa, value = count) %>%
  group_by(depth) %>%
  mutate(relative_abundance_percent = count / sum(count) * 100) %>%
  ungroup()
###############################################################################
###############################################################################
###############################################################################
###############################################################################

#BELOW IS THE CODE USED TO GENERATE THE PLOT


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

####GGPLOT WITHOUT MODIFYING HEADERS
ggplot(
  yeah_counts_common, 
  aes(y = depth, x = relative_abundance_percent)
) +
  # draw horizontal lines of the appropriate length for each depth
  geom_segment(aes(xend = 0, yend = depth), lwd = 1) +
  # facet by taxon, keeping distance on the x axis comparable
  # between facets
  facet_grid(~taxa, scales = "free_x", space = "free_x") +
  # have the same breaks for all x axes
  scale_x_continuous(breaks = c(0, 10, 20, 30)) +
  # reverse the y axis for depth
  scale_y_reverse() +
  labs(x = "Relative Abundance (%)", y = "Depth (cm)")


#### GGPLOT WITH MODIFYING HEADERS
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
  scale_x_continuous(breaks = c(0, 2, 4,6,8)) +
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