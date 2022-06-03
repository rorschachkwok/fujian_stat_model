pacman::p_load(
        tidyverse,
        openxlsx
)
df_sensitivity_ve <- read.xlsx('timeVaryingVE.xlsx', sheet = 5)
df_sensitivity_ve <- df_sensitivity_ve %>% 
        mutate(time = as.factor(time))

plot_sensitivity_ve <- ggplot(df_sensitivity_ve, aes(time, ve, color = group, group = group)) +
        geom_line(size = 1, position=position_dodge(width=0.1)) +
        geom_point(size = 2, position=position_dodge(width=0.1)) + 
        geom_errorbar(aes(ymin = lower, ymax = upper), 
                      width=0.1, size=0.5,
                      position=position_dodge(width=0.1))+
        scale_color_manual(values = c("#0f7ba2", "#dd5129"),
                           labels = c('To symptom onset of index case',
                                      'To isolation of index case'))+
        scale_y_continuous(
                expand = c(0, 0),
                limits = c(0, 1)
        )+ 
        theme_classic()+
        theme(
                axis.title = element_text(size = 10),
                axis.text = element_text(size = 10, colour = 'black'),
                plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
                legend.position = c(0.8, 0.9)
        )+
        labs(color = NULL,
             x = 'Time since full vaccination (days)',
             y = 'Adjusted estimated vaccine effectiveness')

ggsave('plot_sensitivity_ve.tiff', height = 4.5, width = 6, dpi = 300)
