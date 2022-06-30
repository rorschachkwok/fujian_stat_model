# plot delta--------------------------------------------------------------------
# 使用facet_时候，给不同分组添加分组标题
facet_label <- c('Whole population', 'School closure', 
                 'Factory closure', 'School + factory closure') 
facet_hide <- levels(wsf$scenario)
names(facet_label) <- facet_hide
# 可以共用的theme
theme_delta <- function(){
        
        theme_classic()+
                theme(axis.title = element_text(color = 'black', size = 10),
                      legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
                      legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
                      legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
                      legend.background=element_blank(), # 设置背景为空
                      legend.box.margin = margin(1,1,1,1),
                      legend.position = 'none', 
                      legend.text = element_text(size = 10),
                      legend.justification = c(0, 1),
                      strip.text = element_blank(),
                      strip.background = element_rect(color = 'white'))
}

ag1 <- wsf %>% 
        filter(age_group == 'age1') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since second dose', 
                           '31-60 days since second dose', 
                           '60+ days since second dose', 
                           'At most partially vaccinated')) + 
        theme_classic()+
        theme(axis.title = element_text(color = 'black', size = 10),
              legend.spacing.x=unit(0.1,'cm'), 
              legend.key.width=unit(1.5,'cm'), 
              legend.key.height=unit(0.5,'cm'), 
              legend.background=element_blank(), 
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'top', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              strip.text = element_text(hjust = 0.5, face = 'bold', size = 10),
              strip.background = element_rect(color = 'white'))+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(0-9 years old)",
                color = NULL)+
        # 按场景分组画图
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))+ #使用上面创建的分组标题
        guides(color = guide_legend(nrow=2,byrow=TRUE))

ag2 <- wsf %>% 
        filter(age_group == 'age2') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(10-19 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag3 <- wsf %>% 
        filter(age_group == 'age3') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) +
        theme_delta()+
        labs(
                x = NULL,
                y = "New cases per 10,000\n(20-29 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag4 <- wsf %>% 
        filter(age_group == 'age4') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(30-39 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag5 <- wsf %>% 
        filter(age_group == 'age5') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        
        theme_delta()+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(40-49 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag6 <- wsf %>% 
        filter(age_group == 'age6') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        
        theme_delta()+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(50-59 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag7 <- wsf %>% 
        filter(age_group == 'age7') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
                
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = 'Time (days)',
                y = "New cases per 10,000\n(60+ years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed")

patchwork <- ag1/ 
        ag2/
        ag3/
        ag4/
        ag5/
        ag6/
        ag7


ggsave('Fig3.pdf', height = 12, width = 8, dpi = 300)

# plot omicron ------------------------------------------------------------

theme_delta <- function(){      # same theme used in 'plot delta'
        
        theme_classic()+
                theme(axis.title = element_text(color = 'black', size = 10),
                      legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
                      legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
                      legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
                      legend.background=element_blank(), # 设置背景为空
                      legend.box.margin = margin(1,1,1,1),
                      legend.position = 'none', 
                      legend.text = element_text(size = 10),
                      legend.justification = c(0, 1),
                      strip.text = element_blank(),
                      strip.background = element_rect(color = 'white'))
}

ag1 <- wsf %>% 
        filter(age_group == 'age1') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1200),
                breaks = seq(0, 1200, 300)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since second dose', 
                           '31-60 days since second dose', 
                           '60+ days since second dose', 
                           'At most partially vaccinated')) + 
        theme_classic()+
        theme(axis.title = element_text(color = 'black', size = 10),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'top', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              strip.text = element_text(hjust = 0.5, face = 'bold', size = 10),
              strip.background = element_rect(color = 'white'))+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(0-9 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))+
        guides(color = guide_legend(nrow=2,byrow=TRUE))

ag2 <- wsf %>% 
        filter(age_group == 'age2') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1200),
                breaks = seq(0, 1200, 300)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(10-19 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))

ag3 <- wsf %>% 
        filter(age_group == 'age3') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1200),
                breaks = seq(0, 1200, 300)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) +
        theme_delta()+
        labs(
                x = NULL,
                y = "New cases per 10,000\n(20-29 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "free_y",
                   labeller = labeller(scenario = facet_label))

ag4 <- wsf %>% 
        filter(age_group == 'age4') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1200),
                breaks = seq(0, 1200, 300)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(30-39 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag5 <- wsf %>% 
        filter(age_group == 'age5') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1200),
                breaks = seq(0, 1200, 300)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        
        theme_delta()+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(40-49 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag6 <- wsf %>% 
        filter(age_group == 'age6') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1200),
                breaks = seq(0, 1200, 300)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        
        theme_delta()+
        
        labs(
                x = NULL,
                y = "New cases per 10,000\n(50-59 years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed",
                   labeller = labeller(scenario = facet_label))

ag7 <- wsf %>% 
        filter(age_group == 'age7') %>% 
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 1200),
                breaks = seq(0, 1200, 300)
                
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 60)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129")) + 
        theme_delta()+
        
        labs(
                x = 'Time (days)',
                y = "New cases per 10,000\n(60+ years old)",
                color = NULL)+
        facet_grid(cols = vars(scenario), scales =  "fixed")

patchwork <- ag1/ 
        ag2/
        ag3/
        ag4/
        ag5/
        ag6/
        ag7


ggsave('Fig4.pdf', height = 12, width = 8, dpi = 300)
