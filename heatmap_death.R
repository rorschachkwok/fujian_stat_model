pop <- read.xlsx('pop_of_S_28.xlsx', sheet = 1)
pop <- pop %>% 
        mutate(vac = case_when(d_since_vac == 'at_most_partially' ~ 'vac1',
                               d_since_vac == '61+' ~ 'vac2',
                               d_since_vac == '31-60'~ 'vac3',
                               d_since_vac == '14-30' ~ 'vac4')) %>% 
        mutate(vac = fct_relevel(vac, 'vac1', 'vac2', 'vac3', 'vac4')) %>% 
        arrange(age_cat7, vac)

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
facet_label <- c('Whole population', 'Close school', 'Close factory')
facet_hide <- levels(hmap_wsf$scenario)
names(facet_label) <- facet_hide
d <- ggplot(data = hmap_wsf,aes(x = vac,   # x-axis is case age
                           y = age_group,     # y-axis is infector age
                           fill = each_f_per_10k))+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                color = 'grey')+     # color of the tile is the Freq column in the data
        geom_text(aes(label = each_f_per_10k), color = "black", size = 4) +
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
                axis.title = element_text(face = 'bold', size = 12),
                plot.title = element_text(hjust = 0, size = 14),
                strip.text = element_text(face = 'bold', size = 12)
        )+
        facet_grid(cols = vars(scenario), 
                   labeller = labeller(scenario = facet_label))+
        labs(                         
                x = "Time since full vaccination (days)",
                y = "Age category",
                title = 'Cumulative death toll of Delta variant in each cell (1/10000)',
                fill = NULL     
        )

# 
# d2 <- ggplot(data = hmap_s,aes(x = vac,   # x-axis is case age
#                          y = age_group,     # y-axis is infector age
#                          fill = each_f_per_10k))+    # use long data, with proportions as Freq
#         geom_tile(                    # visualize it in tiles
#                 color = 'grey')+     # color of the tile is the Freq column in the data
#         geom_text(aes(label = each_f_per_10k), color = "black", size = 4) +
#         coord_fixed()+ # 保持为正方形
#         scale_fill_gradient(low = "white", high = "red",
#                             limits=c(0, 710),
#                             breaks=seq(0,700,by=100))+
#         
#         scale_y_discrete(
#                 expand = c(0,0),
#                 labels = c('age1' = '0-9', 'age2' = '10-19', 'age3' = '20-29',
#                            'age4' = '30-39', 'age5' = '40-49',
#                            'age6' = '50-59', 'age7' = '60+')
#         )+
#         scale_x_discrete(
#                 expand = c(0,0),
#                 labels=c('vac4' = '14-30', 'vac3' = '31-60', 'vac2' = '61+',
#                          "vac1" = "At most\n partially")
#         )+
#         
#         theme_minimal()+
#         theme(
#                 axis.text.y = element_text(vjust=0.5, size = 10, colour = 'black'),            # axis text alignment
#                 axis.ticks = element_line(size=0.5),
#                 # axis.text.x = element_blank(),
#                 axis.text.x =element_text(angle = 0, vjust = 0.5, colour = 'black', size = 10),
#                 axis.text  = element_text(size = 8),
#                 axis.title = element_text(face = 'bold', size = 12),
#                 plot.title = element_text(hjust = 0.5),
#                 legend.position = 'none'
#         )+
#         labs(                         
#                 x = "Time since full vaccination (days)",
#                 y = NULL,
#                 title = 'Close school',
#                 fill = NULL     
#         )
# 
# d3 <- ggplot(data = hmap_f,aes(x = vac,   # x-axis is case age
#                          y = age_group,     # y-axis is infector age
#                          fill = each_f_per_10k))+    # use long data, with proportions as Freq
#         geom_tile(                    # visualize it in tiles
#                 color = 'grey')+     # color of the tile is the Freq column in the data
#         geom_text(aes(label = each_f_per_10k), color = "black", size = 4) +
#         coord_fixed()+ # 保持为正方形
#         scale_fill_gradient(low = "white", high = "red",
#                             limits=c(0, 710),
#                             breaks=seq(0,700,by=100))+
#         
#         scale_y_discrete(
#                 expand = c(0,0),
#                 labels = c('age1' = '0-9', 'age2' = '10-19', 'age3' = '20-29',
#                            'age4' = '30-39', 'age5' = '40-49',
#                            'age6' = '50-59', 'age7' = '60+')
#         )+
#         scale_x_discrete(
#                 expand = c(0,0),
#                 labels=c('vac4' = '14-30', 'vac3' = '31-60', 'vac2' = '61+',
#                          "vac1" = "At most\n partially")
#         )+
#         
#         theme_minimal()+
#         theme(
#                 axis.text.y = element_text(vjust=0.5, size = 10, colour = 'black'),            # axis text alignment
#                 axis.ticks = element_line(size=0.5),
#                 # axis.text.x = element_blank(),
#                 axis.text.x =element_text(angle = 0, vjust = 0.5, colour = 'black', size = 10),
#                 axis.text  = element_text(size = 8),
#                 axis.title = element_text(face = 'bold', size = 12),
#                 plot.title = element_text(hjust = 0.5)
#                 # legend.justification = c(0.8, 0.8)
#         )+
#         labs(                         
#                 x = "Time since full vaccination (days)",
#                 y = NULL,
#                 title = 'Close factory',
#                 fill = NULL     
#         )
# pw1 <- d1 | d2 | d3
# pw1 <- pw1 + plot_annotation(title = 'Cumulative death toll of Delta variant in each cell (1/10000)',
#                             theme = theme(plot.title = element_text(size = 14)))
# pw1
# # pw1 <- ggplot2::last_plot()

# plot omicron ------------------------------------------------------------

o <- ggplot(data = hmap_wsf,aes(x = vac,   # x-axis is case age
                                 y = age_group,     # y-axis is infector age
                                 fill = each_f_per_10k))+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                color = 'grey')+     # color of the tile is the Freq column in the data
        geom_text(aes(label = each_f_per_10k), color = "black", size = 4) +
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
                axis.title = element_text(face = 'bold', size = 12),
                plot.title = element_text(hjust = 0, size = 14),
                strip.text = element_text(face = 'bold', size = 12)
        )+
        facet_grid(cols = vars(scenario), 
                   labeller = labeller(scenario = facet_label))+
        labs(                         
                x = "Time since full vaccination (days)",
                y = "Age category",
                title = 'Cumulative death toll of Omicron variant in each cell (1/10000)',
                fill = NULL     
        )

# 
# 
# o2 <- ggplot(data = hmap_s,aes(x = vac,   # x-axis is case age
#                                y = age_group,     # y-axis is infector age
#                                fill = each_f_per_10k))+    # use long data, with proportions as Freq
#         geom_tile(                    # visualize it in tiles
#                 color = 'grey')+     # color of the tile is the Freq column in the data
#         geom_text(aes(label = each_f_per_10k), color = "black", size = 4) +
#         coord_fixed()+ # 保持为正方形
#         scale_fill_gradient(low = "white", high = "red",
#                             limits=c(0, 210),
#                             breaks=seq(0,200,by=50))+
#         
#         scale_y_discrete(
#                 expand = c(0,0),
#                 labels = c('age1' = '0-9', 'age2' = '10-19', 'age3' = '20-29',
#                            'age4' = '30-39', 'age5' = '40-49',
#                            'age6' = '50-59', 'age7' = '60+')
#         )+
#         scale_x_discrete(
#                 expand = c(0,0),
#                 labels=c('vac4' = '14-30', 'vac3' = '31-60', 'vac2' = '61+',
#                          "vac1" = "At most\n partially")
#         )+
#         
#         theme_minimal()+
#         theme(
#                 axis.text.y = element_text(vjust=0.5, size = 10, colour = 'black'),            # axis text alignment
#                 axis.ticks = element_line(size=0.5),
#                 # axis.text.x = element_blank(),
#                 axis.text.x =element_text(angle = 0, vjust = 0.5, colour = 'black', size = 10),
#                 axis.text  = element_text(size = 8),
#                 axis.title = element_text(face = 'bold', size = 12),
#                 plot.title = element_text(hjust = 0.5),
#                 legend.position = 'none'
#         )+
#         labs(                         
#                 x = "Time since full vaccination (days)",
#                 y = NULL,
#                 title = 'Close school',
#                 fill = NULL     
#         )
# 
# o3 <- ggplot(data = hmap_f,aes(x = vac,   # x-axis is case age
#                                y = age_group,     # y-axis is infector age
#                                fill = each_f_per_10k))+    # use long data, with proportions as Freq
#         geom_tile(                    # visualize it in tiles
#                 color = 'grey')+     # color of the tile is the Freq column in the data
#         geom_text(aes(label = each_f_per_10k), color = "black", size = 4) +
#         coord_fixed()+ # 保持为正方形
#         scale_fill_gradient(low = "white", high = "red",
#                             limits=c(0, 210),
#                             breaks=seq(0,200,by=50))+
#         
#         scale_y_discrete(
#                 expand = c(0,0),
#                 labels = c('age1' = '0-9', 'age2' = '10-19', 'age3' = '20-29',
#                            'age4' = '30-39', 'age5' = '40-49',
#                            'age6' = '50-59', 'age7' = '60+')
#         )+
#         scale_x_discrete(
#                 expand = c(0,0),
#                 labels=c('vac4' = '14-30', 'vac3' = '31-60', 'vac2' = '61+',
#                          "vac1" = "At most\n partially")
#         )+
#         
#         theme_minimal()+
#         theme(
#                 axis.text.y = element_text(vjust=0.5, size = 10, colour = 'black'),            # axis text alignment
#                 axis.ticks = element_line(size=0.5),
#                 # axis.text.x = element_blank(),
#                 axis.text.x =element_text(angle = 0, vjust = 0.5, colour = 'black', size = 10),
#                 axis.text  = element_text(size = 8),
#                 axis.title = element_text(face = 'bold', size = 12),
#                 plot.title = element_text(hjust = 0.5)
#                 # legend.justification = c(0.8, 0.8)
#         )+
#         labs(                         
#                 x = "Time since full vaccination (days)",
#                 y = NULL,
#                 title = 'Close factory',
#                 fill = NULL     
#         )
# pw2 <- o1 | o2 | o3
# pw2
# pw2 <- pw2 + plot_annotation(title = 'Cumulative death toll of Omicron variant in each cell (1/10000)',
#                       theme = theme(plot.title = element_text(size = 14)))
# pw1 + pw2 + 
#         plot_layout(nrow = 2, ncol = 3)
# pw_bind <- pw1/
#         pw2
# # pw_bind + 
#         theme(plot.margin = margin())

# pw_do <- d/o+  plot_layout(guides = 'collect')

death_toll <- plot_grid(d, o, nrow = 2)
ggsave('death_toll.pdf', height = 10, width = 8, dpi = 300)

# temp --------------------------------------------------------------------
# 
# 
# s_pop %>% 
#         filter(age_group == c('age7')) %>%
#         ggplot()+    # use long data, with proportions as Freq
#         geom_tile(                    # visualize it in tiles
#                 aes(x = d_since_vac,   # x-axis is case age
#                     y = age_cat7,     # y-axis is infector age
#                     fill = per_10k),
#                 color = 'white')+     # color of the tile is the Freq column in the data
#         coord_fixed()+ # 保持为正方形
#         scale_fill_material("red"
#         )+
#         
#         scale_y_discrete(
#                 expand = c(0,0)
#         )+
#         scale_x_discrete(
#                 expand = c(0,0)
#         )+
#         
#         theme_minimal()+
#         theme(
#                 axis.text.y = element_text(vjust=0.5, size = 10, colour = 'black',
#                                            face = 'bold'),            # axis text alignment
#                 axis.ticks = element_line(size=0.5),
#                 axis.text.x = element_blank(),
#                 # axis.text.x =element_text(angle = 30, vjust = 0.5),
#                 axis.text  = element_text(size = 8),
#                 axis.title = element_blank(),
#                 plot.title = element_text(hjust = 0.5)
#                 # legend.justification = c(0.8, 0.8)
#         )+
#         labs(                         
#                 x = "Time since full vaccination (days)",
#                 y = "Age groups",
#                 title = 'Close school',
#                 fill = NULL     
#         )
# s_pop %>% 
#         filter(age_group != c('age7')) %>%
#         ggplot()+    # use long data, with proportions as Freq
#         geom_tile(                    # visualize it in tiles
#                 aes(x = d_since_vac,   # x-axis is case age
#                     y = age_cat7,     # y-axis is infector age
#                     fill = per_10k),
#                 color = 'white')+     # color of the tile is the Freq column in the data
#         coord_fixed()+ # 保持为正方形
#         scale_fill_material("light-blue"
#         )+
#         
#         scale_y_discrete(
#                 expand = c(0,0)
#         )+
#         scale_x_discrete(
#                 expand = c(0,0),
#                 labels=c("at_most_partially" = "At most\n partially")
#         )+
#         
#         theme_minimal()+
#         theme(
#                 axis.text.y = element_text(vjust=0.5, size = 10, colour = 'black',
#                                            face = 'bold'),            # axis text alignment
#                 axis.ticks = element_line(size=0.5),
#                 # axis.text.x = element_blank(),
#                 axis.text.x =element_text(angle = 0, vjust = 0.5, size = 10, colour = 'black',
#                                           face = 'bold'),
#                 axis.text  = element_text(size = 8),
#                 # axis.title = element_blank(),
#                 plot.title = element_text(hjust = 0.5)
#                 # legend.position = 'bottom'
#         )+
#         labs(                         
#                 x = "Time since full vaccination (days)",
#                 y = "Age groups",
#                 title = NULL,
#                 fill = NULL 
#         )
# 
# patchwork <- fig1 / 
#         fig2
# patchwork
# 
#         