library(patchwork)
# plot delta --------------------------------------------------------------

reduction_facet_label <- c('0-9 years old', '10-19 years old', 
                 '20-29 years old', '30-39 years old',
                 '40-49 years old', '50-59 years old', 
                 '60+ years old') 
reduction_facet_hide <- levels(reduce_incid$age_group)
names(reduction_facet_label) <- reduction_facet_hide

d_peak_reduction <- reduce_incid %>% filter(scenario != 'w') %>% 
        ggplot(mapping = aes(x = scenario, y = reduce_incid)) + 
        geom_col(fill = 'cornsilk3') +
        geom_text(aes(label = scales::percent(1 - prop_to_w, accuracy = 0.01)), 
                  vjust = -0.2, 
                  size = 3.5,
                  colour = "black")+ 
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1.1),
                breaks = seq(0, 1, 0.25),
                labels = scales::percent
        )+
        scale_x_discrete(labels = c('School \nclosure', 'Factory \nclosure', 
                                    'School + factory\nclosure'))+
        theme_classic()+
        theme(axis.text = element_text(colour = 'black', size = 10),
              axis.text.x =element_text(angle = 60, vjust = 0.5),
              strip.background = element_blank())+
        labs(x = 'Scenarios',
             y = "Reduction of\npeak daily incidence") +
        facet_wrap(~ age_group, nrow = 1,
                   labeller = labeller(age_group = reduction_facet_label))


# plot omicron ------------------------------------------------------------

o_peak_reduction <- reduce_incid %>% filter(scenario != 'w') %>% 
        ggplot(mapping = aes(x = scenario, y = reduce_incid)) + 
        geom_col(fill = 'cornsilk3') +
        geom_text(aes(label = scales::percent(1 - prop_to_w, accuracy = 0.01)), 
                  vjust = -0.2, 
                  size = 3.5,
                  colour = "black")+ 
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1.1),
                breaks = seq(0, 1, 0.25),
                labels = scales::percent
        )+
        scale_x_discrete(labels = c('School \nclosure', 'Factory \nclosure', 
                                    'School + factory\nclosure'))+
        theme_classic()+
        theme(axis.text = element_text(colour = 'black', size = 10),
              axis.text.x =element_text(angle = 60, vjust = 0.5),
              strip.background = element_blank())+
        labs(x = 'Scenarios',
             y = "Reduction of\npeak daily incidence") +
        facet_wrap(~ age_group, nrow = 1,
                   labeller = labeller(age_group = reduction_facet_label))

patch_reduction <- d_peak_reduction /
        o_peak_reduction
patch_reduction + plot_annotation(tag_levels = 'a')

ggsave('Fig6.pdf', height = 6, width = 13, dpi = 300)
