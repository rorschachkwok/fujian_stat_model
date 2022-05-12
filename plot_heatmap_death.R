# produce heatmap data frame
f_rate <- as.data.frame(c(t(f)))
names(f_rate) <- 'f_rate'
hmap_w <- sele_w %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac1', 'vac2', 'vac3', 'vac4')) %>% 
        group_by(age_group, vac_group) %>% 
        summarise(
                max       = max(Cumulative),
                .groups = 'drop'
        ) %>% 
        bind_cols(f_rate) %>%
        mutate(
                f_toll    = max * f_rate,
                f_per_10k = round(f_toll / 41540086 * 10000, digits = 3),
        )

hmap_f <- sele_f %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac1', 'vac2', 'vac3', 'vac4')) %>% 
        group_by(age_group, vac_group) %>% 
        summarise(
                max       = max(Cumulative),
                .groups = 'drop'
        ) %>% 
        bind_cols(f_rate) %>%
        mutate(
                f_toll    = max * f_rate,
                f_per_10k = round(f_toll / 41540086 * 10000, digits = 3),
        )

hmap_s <- sele_s %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac1', 'vac2', 'vac3', 'vac4')) %>% 
        group_by(age_group, vac_group) %>% 
        summarise(
                max       = max(Cumulative),
                .groups = 'drop'
        ) %>% 
        bind_cols(f_rate) %>%
        mutate(
                f_toll    = max * f_rate,
                f_per_10k = round(f_toll / 41540086 * 10000, digits = 3),
        )

pop <- read.xlsx('pop_of_S_28.xlsx', sheet = 1)
pop <- pop %>% 
        mutate(vac = case_when(d_since_vac == 'at_most_partially' ~ 'vac1',
                               d_since_vac == '61+' ~ 'vac2',
                               d_since_vac == '31-60'~ 'vac3',
                               d_since_vac == '14-30' ~ 'vac4')) %>% 
        mutate(vac = fct_relevel(vac, 'vac1', 'vac2', 'vac3', 'vac4')) %>% 
        arrange(age_cat7, vac) %>% 
        select(-c(age_group, vac_group))

hmap_ww <- bind_cols(pop, hmap_w)%>% 
        mutate(each_f_per_10k = round(f_toll / n_fujian * 10000, digits = 2)) %>% 
        mutate(each_f_per_10k = replace_na(each_f_per_10k, 0)) %>% 
        mutate(vac = fct_relevel(vac, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        arrange(age_cat7, vac) %>% 
        mutate(scenario = 'w')

hmap_ss <- bind_cols(pop, hmap_s)%>% 
        mutate(each_f_per_10k = round(f_toll / n_fujian * 10000, digits = 2)) %>% 
        mutate(each_f_per_10k = replace_na(each_f_per_10k, 0)) %>% 
        mutate(vac = fct_relevel(vac, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        arrange(age_cat7, vac)%>% 
        mutate(scenario = 'c_s')

hmap_ff <- bind_cols(pop, hmap_f)%>% 
        mutate(each_f_per_10k = round(f_toll / n_fujian * 10000, digits = 2)) %>% 
        mutate(each_f_per_10k = replace_na(each_f_per_10k, 0)) %>% 
        mutate(vac = fct_relevel(vac, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        arrange(age_cat7, vac)%>% 
        mutate(scenario = 'c_f')

hmap_wsf <- bind_rows(hmap_ww, hmap_ss, hmap_ff)
hmap_wsf <- hmap_wsf %>% 
        mutate(scenario = as.factor(scenario)) %>% 
        mutate(scenario = fct_relevel(scenario, 'w', 'c_s', 'c_f'))

# plot delta --------------------------------------------------------------
facet_label <- c('Whole population', 'School closure', 'Factory closure')
facet_hide <- levels(hmap_wsf$scenario)
names(facet_label) <- facet_hide
d <- ggplot(data = hmap_wsf,aes(x = vac,   # x-axis is case age
                           y = age_group,     # y-axis is infector age
                           fill = each_f_per_10k))+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                color = 'grey')+     # color of the tile is the Freq column in the data
        geom_text(aes(label = format(round(each_f_per_10k, 2), nsmall = 2)), color = "black", size = 4) +
        coord_fixed()+ # 保持为正方形
        
        scale_fill_distiller(palette = 'Spectral',
                             limits=c(0, 710),
                             breaks=seq(0,600,by=200))+
        
        scale_y_discrete(
                expand = c(0,0),
                labels = c('age1' = '0-9', 'age2' = '10-19', 'age3' = '20-29',
                           'age4' = '30-39', 'age5' = '40-49',
                           'age6' = '50-59', 'age7' = '60+')
        )+
        scale_x_discrete(
                expand = c(0,0),
                labels=c('vac4' = '14-30', 'vac3' = '31-60', 'vac2' = '61+',
                         "vac1" = "At most\n partially")
        )+
        
        theme_minimal()+
        theme(
                axis.text.y = element_text(vjust=0.5, size = 10, colour = 'black'),            # axis text alignment
                axis.ticks = element_line(size=0.5),
                # axis.text.x = element_blank(),
                axis.text.x =element_text(angle = 0, vjust = 0.5, colour = 'black', size = 10),
                axis.text  = element_text(size = 8),
                axis.title = element_text(face = 'bold', size = 10),
                plot.title = element_text(hjust = 0, size = 14),
                strip.text = element_text(face = 'bold', size = 10)
        )+
        facet_grid(cols = vars(scenario), 
                   labeller = labeller(scenario = facet_label))+
        labs(                         
                x = "Vaccination status",
                y = "Age category",
                title = 'a',
                fill = NULL     
        )

# plot omicron ------------------------------------------------------------

o <- ggplot(data = hmap_wsf,aes(x = vac,   # x-axis is case age
                                 y = age_group,     # y-axis is infector age
                                 fill = each_f_per_10k))+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                color = 'grey')+     # color of the tile is the Freq column in the data
        geom_text(aes(label = format(round(each_f_per_10k, 2), nsmall = 2)), color = "black", size = 4) +
        coord_fixed()+ # 保持为正方形
        
        scale_fill_distiller(palette = 'Spectral',
                             limits=c(0, 710),
                             breaks=seq(0,600,by=200))+
        
        scale_y_discrete(
                expand = c(0,0),
                labels = c('age1' = '0-9', 'age2' = '10-19', 'age3' = '20-29',
                           'age4' = '30-39', 'age5' = '40-49',
                           'age6' = '50-59', 'age7' = '60+')
        )+
        scale_x_discrete(
                expand = c(0,0),
                labels=c('vac4' = '14-30', 'vac3' = '31-60', 'vac2' = '61+',
                         "vac1" = "At most\n partially")
        )+
        
        theme_minimal()+
        theme(
                axis.text.y = element_text(vjust=0.5, size = 10, colour = 'black'),            # axis text alignment
                axis.ticks = element_line(size=0.5),
                # axis.text.x = element_blank(),
                axis.text.x =element_text(angle = 0, vjust = 0.5, colour = 'black', size = 10),
                axis.text  = element_text(size = 8),
                axis.title = element_text(face = 'bold', size = 10),
                plot.title = element_text(hjust = 0, size = 14),
                strip.text = element_text(face = 'bold', size = 10)
        )+
        facet_grid(cols = vars(scenario), 
                   labeller = labeller(scenario = facet_label))+
        labs(                         
                x = "Vaccination status",
                y = "Age category",
                title = 'b',
                fill = NULL     
        )

death_toll <- plot_grid(d, o, nrow = 2)
ggsave('death_toll.pdf', height = 10, width = 8, dpi = 300)

        