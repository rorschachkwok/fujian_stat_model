pacman::p_load(
        tidyverse,
        openxlsx
)
df <- read.xlsx('timeVaryingVE.xlsx', sheet = 4)
df <- df %>% 
        mutate(time = as.factor(time))
levels(df$time)               
plot_ve <- ggplot(df, aes(time, ve, group = 1)) +
        geom_line(size = 1) +
        geom_point(size = 2) + 
        geom_errorbar(aes(ymin = lower, ymax = upper), 
                      width=0.05, size=1, 
                      color="#0f7ba2")+
        scale_y_continuous(
                expand = c(0, 0),
                limits = c(0, 1)
        )+ 
        theme_classic()+
        theme(
                axis.title = element_text(size = 10),
                axis.text = element_text(size = 10, colour = 'black'),
                plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm")
        )+
        labs(x = 'Time since full vaccination (days)',
             y = 'Adjusted estimated vaccine effectiveness')

patch_ve_si_density <- (plot_sensitivity_ve | plot_si_shift) /
        orf1ab_density /
        n_density
patch_ve_si_density + plot_annotation(tag_levels = 'a')        
         
        

ggsave('patch_ve_si_density.pdf', height = 15, width = 12, dpi = 300)

ggsave('plot_si_t.tiff', height = 4, width = 6, dpi = 300)
