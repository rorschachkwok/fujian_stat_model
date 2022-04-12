df <- read.xlsx('timeVaryingVE.xlsx', sheet = 2)
df <- df %>% 
        mutate(time = as.factor(time),
               VE = 1 - HR,
               VE.lower = 1 - upper,
               VE.upper = 1 - lower)
levels(df$time)               
ggplot(df, aes(time, VE, group = 1)) +
        geom_line(size = 1) +
        geom_point(size = 2) + 
        # coord_fixed()+
        # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3)
        geom_errorbar(aes(ymin = VE.lower, ymax = VE.upper), 
                      width=0.05, size=1, 
                      color="steelblue")+
        scale_y_continuous(
                expand = c(0, 0),
                limits = c(0, 1)
        )+ 
        theme_classic()+
        theme(
                axis.title = element_text(face = 'bold', size = 12),
                axis.text = element_text(size = 10, colour = 'black'),
                plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm")
        )+
        labs(x = 'Time since full vaccination (days)',
             y = 'Adjusted estimated vaccine effectiveness')
# pacman::p_load(ggpval)      
# add_pval(p, pairs = list(c(1,2),c(2,3)), annotation = c('0.05', '0.002'),
#          alternative='two.sided',textsize=2)
ggsave('time_varying_VE.pdf', height = 4, width = 5, dpi = 300)
ggsave('Fig2.tiff', p, height = 6, width = 6, dpi = 300)
