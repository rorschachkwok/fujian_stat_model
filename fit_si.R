pacman::p_load(
        openxlsx,
        MASS,
        fitdistrplus,
        tidyverse,
        patchwork
)
df <- read.xlsx('cleaned_data.xlsx')

si_dist <- df %>% 
        dplyr::select(date_onset, index_onset_date) %>% 
        mutate(across(.cols = everything(),
                      .fns = convertToDate)) %>% 
        drop_na(date_onset) %>% 
        mutate(si = as.numeric(date_onset - index_onset_date)) %>% 
        mutate(si_positive = si + 7)
si_shift <- si_dist$si_positive
si_raw <- si_dist$si
si_truncated <- si_dist %>% filter(si > 0)
si_truncated <- si_truncated$si

set.seed(10)
# normMLE  <- fitdist(si_shift, "norm", method = 'mle')
gammaMLE <- fitdist(si_shift, "gamma", method = 'mle')
weiMLE <- fitdist(si_shift, "weibull", method = 'mle')
lnormMLE <- fitdist(si_shift, "lnorm", method = 'mle')
# plot(normMLE)
plot(gammaMLE)
plot(weiMLE)
plot(lnormMLE)
# summary(normMLE)
summary(gammaMLE)
summary(weiMLE)
summary(lnormMLE)

si_data <- as.data.frame(si_shift) %>% 
        group_by(si_shift) %>% 
        count() %>% 
        ungroup() %>% 
        mutate(freq = n/sum(n))

plot_si_shift <- ggplot(data = data.frame(x = c(0, 30)), aes(x))+
        stat_function(fun = dgamma, 
                      args = list(shape = gammaMLE$estimate[[1]], rate = gammaMLE$estimate[[2]]),
                      mapping = aes(color = 'Gamma'),
                      size = 1)+
        stat_function(fun = dweibull, 
                      args = list(shape = weiMLE$estimate[[1]], scale = weiMLE$estimate[[2]]),
                      mapping = aes(color = 'Weibull'),
                      size = 1)+
        stat_function(fun = dlnorm,
                      args = list(mean = lnormMLE$estimate[[1]], sd = lnormMLE$estimate[[2]]),
                      mapping = aes(color = 'Lognormal'),
                      size = 1)+
        
        geom_bar(data = si_data,
                 mapping = aes(x = si_shift, 
                               y = freq,
                               fill = 'Measured serial interval'),
                 alpha = 0.3,
                 stat="identity",
                 position=position_dodge())+
        geom_vline(xintercept=8.64, linetype="dashed", color = "black")+
        geom_vline(xintercept=5,    linetype="dotted", color = "black")+
        geom_text(x=8.64, y=0.15, label="mean = 3.64", hjust = -0.1)+
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 0.16),
                breaks = seq(0, 0.15, 0.05)
        )+
        scale_x_continuous(
                breaks = seq(0, 20, 5),
                labels = seq(-5, 15, 5)
        )+
        scale_color_manual(values = c("#0f7ba2","#fab255","#dd5129"),
                           labels = c('Gamma', 'Lognormal', 'Weibull'))+
        scale_fill_manual(values = 'azure4')+
        theme_classic()+
        theme(axis.title = element_text(size = 10),
              axis.text = element_text(size = 10, colour = 'black'),
              legend.spacing.x=unit(0.1,'cm'), 
              legend.key.width=unit(1.5,'cm'), 
              legend.key.height=unit(0.7,'cm'), 
              legend.background=element_blank(), 
              legend.box.margin = margin(1,1,1,1),
              legend.position = c(0.8, 0.8),
              legend.text = element_text(size = 10))+
        labs(colour = NULL,
             fill = NULL,
             x = 'Serial interval (days)',
             y = 'Probability')

# truncated si distribution
t_gammaMLE <- fitdist(si_truncated, "gamma", method = 'mle')
t_weiMLE <- fitdist(si_truncated, "weibull", method = 'mle')
t_lnormMLE <- fitdist(si_truncated, "lnorm", method = 'mle')
plot(t_gammaMLE)
plot(t_weiMLE)
plot(t_lnormMLE)
summary(t_gammaMLE)
summary(t_weiMLE)
summary(t_lnormMLE)

set.seed(100)
x_gamma = round(rgamma(n = 100, shape = 2.1527108, rate = 0.4717341), 3)
summary(x_gamma)

si_trunc_data <- as.data.frame(si_truncated) %>% 
        group_by(si_truncated) %>% 
        count() %>% 
        ungroup() %>% 
        mutate(freq = n/sum(n))

plot_si_t <- ggplot(data = data.frame(x = c(0, 25)), aes(x))+
        stat_function(fun = dgamma, 
                      args = list(shape = t_gammaMLE$estimate[[1]], rate = t_gammaMLE$estimate[[2]]),
                      mapping = aes(color = 'Gamma'),
                      size = 1)+
        stat_function(fun = dweibull, 
                      args = list(shape = t_weiMLE$estimate[[1]], scale = t_weiMLE$estimate[[2]]),
                      mapping = aes(color = 'Weibull'),
                      size = 1)+
        stat_function(fun = dlnorm,
                      args = list(mean = t_lnormMLE$estimate[[1]], sd = t_lnormMLE$estimate[[2]]),
                      mapping = aes(color = 'Lognormal'),
                      size = 1)+
        
        geom_bar(data = si_trunc_data,
                 mapping = aes(x = si_truncated, 
                               y = freq,
                               fill = 'Truncated serial interval'),
                 alpha = 0.3,
                 stat="identity",
                 position=position_dodge())+
        geom_vline(xintercept=4.56, linetype="dashed", color = "black")+
        geom_text(x=4.56, y=0.2, label="mean = 4.56", hjust = -0.1)+
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 0.22),
                breaks = seq(0, 0.2, 0.05)
        )+
        scale_x_continuous(
                breaks = seq(0, 20, 5),
                labels = seq(0, 20, 5)
        )+
        scale_color_manual(values = c("#0f7ba2","#fab255","#dd5129"),
                           labels = c('Gamma', 'Lognormal', 'Weibull'))+
        scale_fill_manual(values = 'azure4')+
        theme_classic()+
        theme(axis.title = element_text(face = 'bold', size = 10),
              axis.text = element_text(size = 10, colour = 'black'),
              legend.spacing.x=unit(0.1,'cm'), 
              legend.key.width=unit(1.5,'cm'), 
              legend.key.height=unit(0.7,'cm'), 
              legend.background=element_blank(), 
              legend.box.margin = margin(1,1,1,1),
              legend.position = c(0.8, 0.8),
              legend.text = element_text(size = 10))+
        labs(colour = NULL,
             fill = NULL,
             x = 'Serial interval (days)',
             y = 'Probability')


# ct value violin plot ----------------------------------------------------

ct_data <- df %>% 
        filter(outcome == 1) %>% 
        dplyr::select(days_onset_pcr:vac_history) %>% 
        mutate(days_onset_pcr = as.factor(days_onset_pcr),
               vac_history = replace(vac_history, vac_history == 1, 0),
               vac_history = as.factor(vac_history)) %>% 
        drop_na()

plot_orf1ab <- 
        ggplot(ct_data, aes(x = days_onset_pcr, y = orf1ab, fill = vac_history)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter()+
        scale_y_continuous(
                limits = c(0, 45),
                breaks = seq(0, 40, 10)
        )+
        scale_fill_manual(values = c(
                "#dd5129", "#0f7ba2"),
                labels = c('At most partially vaccinated',
                           'Fully vaccinated'))+
        theme_classic()+
        theme(
                legend.position = 'top',
                strip.text = element_blank(),
                strip.background = element_rect(color = 'white')
        )+
        labs(
                x = 'Days from symptom onset to PCR test',
                y = "ORF1ab gene",
                fill = NULL)+
        facet_wrap(~ vac_history)

plot_n <- ggplot(ct_data, aes(x = days_onset_pcr, y = n_gene, fill = vac_history)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter()+
        scale_y_continuous(
                limits = c(0, 45),
                breaks = seq(0, 40, 10)
        )+
        scale_fill_manual(values = c(
                "#dd5129", "#0f7ba2"),
                labels = c('At most partially vaccinated',
                           'Fully vaccinated'))+
        theme_classic()+
        theme(
                legend.position = 'top',
                strip.text = element_blank(),
                strip.background = element_rect(color = 'white')
        )+
        labs(
                x = 'Days from symptom onset to PCR test',
                y = "N gene",
                fill = NULL)+
        facet_wrap(~ vac_history)

patch_ct <- plot_orf1ab/
        plot_n
patch_ct + plot_annotation(tag_levels = 'a')

ggsave('plot_ct.pdf', height = 12, width = 8, dpi = 300)




# ct kernel density plot ---------------------------------------------
ct_data <- df %>% 
        filter(outcome == 1) %>% 
        dplyr::select(days_onset_pcr:vac_history) %>% 
        mutate(days_onset_pcr = as.factor(days_onset_pcr),
               vac_history = replace(vac_history, vac_history == 1, 0),
               vac_history = as.factor(vac_history)) %>% 
        drop_na()

# ct_density <- ct_data %>% filter(days_onset_pcr %in% -1:4)
        
kd_facet_label <- c('At most partially vaccinated',
                 'Fully vaccinated')
kd_facet_hide <- levels(ct_data$vac_history)
names(kd_facet_label) <- kd_facet_hide

orf1ab_density <- ct_data %>% 
        ggplot(aes(x = as.numeric(as.character(days_onset_pcr)), y = orf1ab))+
        stat_density_2d(aes(fill = ..level..), geom = "polygon") +
        
        scale_fill_distiller(palette = 4, direction = 1,
                             limits=c(0, 0.04),
                             breaks=seq(0, 0.04, by = 0.01)) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, 45),
                           breaks = seq(0, 40, 10)) +
        scale_x_continuous(
                limits = c(-1, 3)
        )+
        
        theme_classic()+
        theme(
                text = element_text(colour = 'black'),
                panel.grid = element_blank(),
                strip.background = element_rect(color = 'white')
        )+
        
        labs(
                x = 'Time since symptom onset (days)',
                y = "Ct value",
                fill = 'Density')+
        
        facet_wrap(~ vac_history,
                   labeller = labeller(vac_history = kd_facet_label))

n_density <- ct_data %>% 
        ggplot(aes(x = as.numeric(as.character(days_onset_pcr)), y = n_gene))+
        stat_density_2d(aes(fill = ..level..), geom = "polygon") +
        
        scale_fill_distiller(palette = 4, direction = 1,
                             limits=c(0, 0.04),
                             breaks=seq(0, 0.04, by = 0.01)) +
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, 45),
                           breaks = seq(0, 40, 10)) +
        scale_x_continuous(
                limits = c(-1, 3)
        )+
        
        theme_classic()+
        theme(
                text = element_text(colour = 'black'),
                panel.grid = element_blank(),
                strip.background = element_rect(color = 'white')
        )+
        
        labs(
                x = 'Time since symptom onset (days)',
                y = "Ct value",
                fill = 'Density')+
        
        facet_wrap(~ vac_history,
                   labeller = labeller(vac_history = kd_facet_label))

patch_density <- orf1ab_density/
        n_density
patch_density + plot_annotation(tag_levels = 'a')

ggsave('plot_ct_density.pdf', height = 12, width = 8, dpi = 300)
