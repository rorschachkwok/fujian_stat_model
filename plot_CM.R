pacman::p_load(
        tidyverse,
        openxlsx,
        ggsci,
        patchwork
        
)

cnt_df <- read.xlsx('contactMatrix.xlsx', sheet = 2, colNames = T)
cnt_long <- cnt_df %>% 
        pivot_longer(
                cols = 2:8,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1)



p_w <- ggplot(data = cnt_long)+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                aes(x = index_case,   # x-axis is case age
                    y = contact,     # y-axis is infector age
                    fill = freq),
                color = 'white')+     # color of the tile is the Freq column in the data
        coord_fixed()+ # 保持为正方形
        scale_fill_material("indigo",
                            limits=c(0, 4.2),
                            breaks=seq(0,4,by=1))+
        scale_y_discrete(
                expand = c(0,0)
        )+
        scale_x_discrete(
                expand = c(0,0)
        )+
        
        theme_minimal()+
        theme(
                axis.text.y = element_text(vjust=0.2),            # axis text alignment
                axis.ticks = element_line(size=0.5),
                axis.text.x =element_text(angle = 90, vjust = 0.5),
                axis.text  = element_text(size = 8),
                plot.title = element_text(hjust = 0.5)
        )+
        labs(                         
                x = "Age of index case",
                y = "Age of close contact",
                title = NULL,
                fill = NULL     
        )


# school matrix -----------------------------------------------------------
cnt_s <- read.xlsx('cntm_school.xlsx', sheet = 2, colNames = T)
cnt_s_long <- cnt_s %>% 
        pivot_longer(
                cols = 2:8,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1)



p_s <- ggplot(data = cnt_s_long)+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                aes(x = index_case,   # x-axis is case age
                    y = contact,     # y-axis is infector age
                    fill = freq),
                color = 'white')+     # color of the tile is the Freq column in the data
        coord_fixed()+ 
        scale_fill_material("indigo",
                            limits=c(0, 4.2),
                            breaks=seq(0,4,by=1))+
        
        scale_y_discrete(
                expand = c(0,0)
        )+
        scale_x_discrete(
                expand = c(0,0)
        )+
        
        theme_minimal()+
        theme(
                axis.text.y = element_text(vjust=0.2),            # axis text alignment
                axis.ticks = element_line(size=0.5),
                axis.text.x =element_text(angle = 90, vjust = 0.5),
                axis.text  = element_text(size = 8),
                plot.title = element_text(hjust = 0.5)
                
        )+
        labs(                         
                x = "Age of index case",
                y = "Age of close contact",
                title = NULL,
                fill = NULL     
        )


# factory matrix ----------------------------------------------------------
cnt_f <- read.xlsx('cntm_factory.xlsx', sheet = 2, colNames = T)
cnt_f_long <- cnt_f %>% 
        pivot_longer(
                cols = 2:8,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1)



p_f <- ggplot(data = cnt_f_long)+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                aes(x = index_case,   # x-axis is case age
                    y = contact,     # y-axis is infector age
                    fill = freq),
                color = 'white')+     # color of the tile is the Freq column in the data
        coord_fixed()+ 
        scale_fill_material("indigo",
                            limits=c(0, 4.2),
                            breaks=seq(0,4,by=1))+
        
        scale_y_discrete(
                expand = c(0,0)
        )+
        scale_x_discrete(
                expand = c(0,0)
        )+
        
        theme_minimal()+
        theme(
                axis.text.y = element_text(vjust=0.2),            # axis text alignment
                axis.ticks = element_line(size=0.5),
                axis.text.x =element_text(angle = 90, vjust = 0.5),
                axis.text  = element_text(size = 8),
                plot.title = element_text(hjust = 0.5)
        )+
        labs(                         
                x = "Age of index case",
                y = "Age of close contact",
                title = NULL,
                fill = NULL     
        )


# community and others matrix ---------------------------------------------
cnt_cno <- read.xlsx('cntm_cno.xlsx', sheet = 2, colNames = T)
cnt_cno_long <- cnt_cno %>% 
        pivot_longer(
                cols = 2:8,
                names_to = 'contact',
                values_to = 'freq'
        ) %>% 
        rename(index_case = 1)



p_cno <- ggplot(data = cnt_cno_long)+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                aes(x = index_case,   # x-axis is case age
                    y = contact,     # y-axis is infector age
                    fill = freq),
                color = 'white')+     # color of the tile is the Freq column in the data
        coord_fixed()+ 
        scale_fill_material("indigo",
                            limits=c(0, 4.2),
                            breaks=seq(0,4,by=1))+
        
        scale_y_discrete(
                expand = c(0,0)
        )+
        scale_x_discrete(
                expand = c(0,0)
        )+
        
        theme_minimal()+
        theme(
                axis.text.y = element_text(vjust=0.2),            # axis text alignment
                axis.ticks = element_line(size=0.5),
                axis.text.x =element_text(angle = 90, vjust = 0.5),
                axis.text  = element_text(size = 8),
                plot.title = element_text(hjust = 0.5)
        )+
        labs(                         
                x = "Age of index case",
                y = "Age of close contact",
                title = NULL,
                fill = NULL     
        )


patchwork <-  (p_w | p_s) /
              (p_f | p_cno)
patchwork + plot_annotation(tag_levels = 'a')      

ggsave('plot_cntm.pdf', height = 9, width = 10.5, dpi = 300)