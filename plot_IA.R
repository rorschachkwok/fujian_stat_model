
# plot delta --------------------------------------------------------------

ag1 <- wsf %>% 
        filter(age_group == 'age1') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1600000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since second dose', 
                           '31-60 days since second dose', 
                           '60+ days since second dose', 
                           'At most partially vaccinated')) + 
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'top', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              strip.text = element_text(hjust = 0.5, face = 'bold', size = 12),
              strip.background = element_rect(color = 'white'))+
        
        labs(
                x = NULL,
                y = "0-9 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))+
        guides(color = guide_legend(nrow=2,byrow=TRUE))

ag2 <- wsf %>% 
        filter(age_group == 'age2') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1600000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = NULL,
                y = "10-19 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))

ag3 <- wsf %>% 
        filter(age_group == 'age3') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1600000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) +
        theme_delta()+
        labs(
                x = NULL,
                y = "20-29 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))

ag4 <- wsf %>% 
        filter(age_group == 'age4') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1600000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = NULL,
                y = "30-39 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag5 <- wsf %>% 
        filter(age_group == 'age5') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1600000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        
        theme_delta()+
        
        labs(
                x = NULL,
                y = "40-49 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag6 <- wsf %>% 
        filter(age_group == 'age6') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1600000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        
        theme_delta()+
        
        labs(
                x = NULL,
                y = "50-59 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag7 <- wsf %>% 
        filter(age_group == 'age7') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1600000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = 'Time (days)',
                y = "60+ years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed")

patchwork <- ag1/ 
        ag2/
        ag3/
        ag4/
        ag5/
        ag6/
        ag7


ggsave('IA_delta.pdf', height = 12, width = 8, dpi = 300)


# plot omicron ------------------------------------------------------------

ag1 <- wsf %>% 
        filter(age_group == 'age1') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1900000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since second dose', 
                           '31-60 days since second dose', 
                           '60+ days since second dose', 
                           'At most partially vaccinated')) + 
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'top', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              strip.text = element_text(hjust = 0.5, face = 'bold', size = 12),
              strip.background = element_rect(color = 'white'))+
        
        labs(
                x = NULL,
                y = "0-9 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))+
        guides(color = guide_legend(nrow=2,byrow=TRUE))

ag2 <- wsf %>% 
        filter(age_group == 'age2') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1900000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = NULL,
                y = "10-19 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))

ag3 <- wsf %>% 
        filter(age_group == 'age3') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1900000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) +
        theme_delta()+
        labs(
                x = NULL,
                y = "20-29 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))

ag4 <- wsf %>% 
        filter(age_group == 'age4') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1900000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = NULL,
                y = "30-39 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag5 <- wsf %>% 
        filter(age_group == 'age5') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1900000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        
        theme_delta()+
        
        labs(
                x = NULL,
                y = "40-49 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag6 <- wsf %>% 
        filter(age_group == 'age6') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1900000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        
        theme_delta()+
        
        labs(
                x = NULL,
                y = "50-59 years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag7 <- wsf %>% 
        filter(age_group == 'age7') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1900000),
                breaks = seq(0, 1600000, 400000),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 240, 40)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = 'Time (days)',
                y = "60+ years old",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed")

patchwork <- ag1/ 
        ag2/
        ag3/
        ag4/
        ag5/
        ag6/
        ag7


ggsave('IA_omicron.pdf', height = 12, width = 8, dpi = 300)
