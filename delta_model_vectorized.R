#Loading all necessary libraries 
pacman::p_load(
        deSolve,
        tidyverse,
        openxlsx,
        MetBrewer,
        ggsci,
        scales,
        cowplot,
        patchwork,
        slider,
        lubridate
)

rm(list = ls())

# initial values
V0 <- read.xlsx('pop_of_S_28.xlsx', sheet = 1)
V0 <- V0$n_fujian # inital value for number of susceptible stratified by vaccination status
V0 <- matrix(V0, nrow = 7, byrow = T)
E0 <- matrix(rep(0, 28), nrow = 7, byrow = T) # inital value for number of exposed
P0 <- matrix(rep(0, 28), nrow = 7, byrow = T)
A0 <- matrix(rep(0, 28), nrow = 7, byrow = T)
I0 <- matrix(rep(0, 28), nrow = 7, byrow = T) # inital value for number of infectious
I0[4, 1] <- 1
R0 <- matrix(rep(0, 28), nrow = 7, byrow = T) # inital value for number of recovered
C0 <- matrix(rep(0, 28), nrow = 7, byrow = T) 

# parameters
beta <- read.xlsx('Beta_28.xlsx', sheet = 2, colNames = F)
beta <- as.matrix(beta) # matrix of transmission rates
kappa1 <- matrix(rep(0.63, 28), nrow = 7, byrow = T)
kappa2 <- matrix(rep(0.35, 28), nrow = 7, byrow = T)
HR <- matrix(rep(c(1, 0.54036, 0.44003, 0.36506), 7), nrow = 7, byrow = T)
mu <- matrix(rep(0.33, 28), nrow = 7, byrow = T)
omega <- matrix(rep(1/3, 28), nrow = 7, byrow = T)
omegap <- matrix(rep(1/3, 28), nrow = 7, byrow = T) # rate at which individuals move from the exposed to the pre-symptom classes
omegapp <- matrix(rep(1/2, 28), nrow = 7, byrow = T)
gamma <- matrix(rep(1/5, 28), nrow = 7, byrow = T) # recovery rate
gammap <- matrix(rep(1/7, 28), nrow = 7, byrow = T)

f_raw <- read.xlsx('death_rate.xlsx', sheet = 1, colNames = F)
f <- as.matrix(f_raw)

# combining parameter and initial values
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)
INPUT <- c(t(V0), t(E0), t(P0), t(A0), t(I0), t(R0), t(C0)) # same order as ode return list

t_range <- seq(from = 0, to = 270, by = 1) # vector with time steps


# differential equations --------------------------------------------------
diff_eqs <- function(times, INPUT, parms){
        
        with(as.list(c(INPUT, parms)), {
                V <- matrix(INPUT[1:28], nrow = 7, byrow = T)
                E <- matrix(INPUT[29:56], nrow = 7, byrow = T)
                P <- matrix(INPUT[57:84], nrow = 7, byrow = T)
                A <- matrix(INPUT[85:112], nrow = 7, byrow = T)
                I <- matrix(INPUT[113:140], nrow = 7, byrow = T)
                R <- matrix(INPUT[141:168], nrow = 7, byrow = T)
                C <- matrix(INPUT[169:196], nrow = 7, byrow = T)
                
                lambda <- t(beta) %*% rowSums(I + kappa1*P + kappa2*A)
                dV <- -c(lambda) * HR * V  
                dE <- c(lambda) * HR * V - mu*omega*E - (1-mu)*omegap*E
                dP <- (1-mu)*omegap*E - omegapp*P
                dA <- mu*omega*E - gammap*A
                dI <- omegapp*P - gamma*I - f*I
                dR <- gamma*I + gammap*A
                dC <- omegapp*P 
                list(c(t(dV), t(dE), t(dP), t(dA), t(dI), t(dR), t(dC))) # same order as the INPUT
        })
}

out <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
out <- as.data.frame(out)

# with(out, {
#         plot(x = time, y = 1- (`28` / 597466))})
# min(out$`1`)


# change param to close factory
beta <- read.xlsx('Beta_28_cut_f.xlsx', sheet = 2, colNames = F)
beta <- as.matrix(beta) 
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate close factory
cut_f <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cut_f <- as.data.frame(cut_f)

# change param to close school
beta <- read.xlsx('Beta_28_cut_s.xlsx', sheet = 2, colNames = F)
beta <- as.matrix(beta) 
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate close school
cut_s <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cut_s <- as.data.frame(cut_s)



# manage whole population -----------------------------------------------------------
m <- 28
age_id <- rep(1:7, each = 4)
vac_id <- rep(1:4, 7)
pop <- read.xlsx('pop_of_S_28.xlsx', sheet = 1, colNames = T)
pop <- pop[, c("group", 'age_group', 'vac_group', "n_fujian")]

df <- out %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(status = as.numeric(group)) %>% 
        
        mutate(status = case_when(status %in% seq(    1,   m) ~ 'Susceptible',
                                  status %in% seq(  m+1, 2*m) ~ 'Exposed',
                                  status %in% seq(2*m+1, 3*m) ~ 'Pre-symptomatic',
                                  status %in% seq(3*m+1, 4*m) ~ 'Asymptomatic',
                                  status %in% seq(4*m+1, 5*m) ~ 'Infectious',
                                  status %in% seq(5*m+1, 6*m) ~ 'Recovered',
                                  status %in% seq(6*m+1, 7*m) ~ 'Cumulative')) %>% 
        mutate(status = fct_relevel(status, "Susceptible", 'Exposed', 'Pre-symptomatic',
                                    'Asymptomatic', 'Infectious', 'Recovered', 'Cumulative')) 
for (i in 1:28) {
        df[df$group %in% c(seq(i, 6*m+i, m)), 'group'] <- c(str_glue('age{age_id[i]}_vac{vac_id[i]}')) 
}

sele_w <- df %>% 
        # filter(status %in% c('Susceptible', 'Asymptomatic', 'Infectious', 'Pre-symptomatic', 'Cumulative')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
        mutate(daily_new = Exposed * mu[1,1] * omega[1,1] + `Pre-symptomatic` * omegapp[1,1]) %>% 
        left_join(pop, by = c('group' = 'group')) %>%
        arrange(group, time) %>% 
        group_by(group) %>% 
        mutate(
                mean_7day_new = slide_dbl(
                        .x = daily_new,
                        .i = time,
                        .f = mean,
                        .before = 6
                )) %>% 
        mutate(rolling_week_incid = mean_7day_new / n_fujian * 10000) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        mutate(scenario = 'w')

# manage school
cut_s <- cut_s %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(status = as.numeric(group)) %>% 
        
        mutate(status = case_when(status %in% seq(    1,   m) ~ 'Susceptible',
                                  status %in% seq(  m+1, 2*m) ~ 'Exposed',
                                  status %in% seq(2*m+1, 3*m) ~ 'Pre-symptomatic',
                                  status %in% seq(3*m+1, 4*m) ~ 'Asymptomatic',
                                  status %in% seq(4*m+1, 5*m) ~ 'Infectious',
                                  status %in% seq(5*m+1, 6*m) ~ 'Recovered',
                                  status %in% seq(6*m+1, 7*m) ~ 'Cumulative')) %>% 
        mutate(status = fct_relevel(status, "Susceptible", 'Exposed', 'Pre-symptomatic',
                                    'Asymptomatic', 'Infectious', 'Recovered', 'Cumulative')) 
for (i in 1:28) {
        cut_s[cut_s$group %in% c(seq(i, 6*m+i, m)), 'group'] <- c(str_glue('age{age_id[i]}_vac{vac_id[i]}')) 
}


sele_s <- cut_s %>% 
        # filter(status %in% c('Susceptible', 'Asymptomatic', 'Infectious', 'Pre-symptomatic', 'Cumulative')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
        mutate(daily_new = Exposed * mu[1,1] * omega[1,1] + `Pre-symptomatic` * omegapp[1,1]) %>% 
        left_join(pop, by = c('group' = 'group')) %>%
        arrange(group, time) %>% 
        group_by(group) %>% 
        mutate(
                mean_7day_new = slide_dbl(
                        .x = daily_new,
                        .i = time,
                        .f = mean,
                        .before = 6
                )) %>% 
        mutate(rolling_week_incid = mean_7day_new / n_fujian * 10000) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        mutate(scenario = 'c_s')

# manage factory
cut_f <- cut_f %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(status = as.numeric(group)) %>% 
        
        mutate(status = case_when(status %in% seq(    1,   m) ~ 'Susceptible',
                                  status %in% seq(  m+1, 2*m) ~ 'Exposed',
                                  status %in% seq(2*m+1, 3*m) ~ 'Pre-symptomatic',
                                  status %in% seq(3*m+1, 4*m) ~ 'Asymptomatic',
                                  status %in% seq(4*m+1, 5*m) ~ 'Infectious',
                                  status %in% seq(5*m+1, 6*m) ~ 'Recovered',
                                  status %in% seq(6*m+1, 7*m) ~ 'Cumulative')) %>% 
        mutate(status = fct_relevel(status, "Susceptible", 'Exposed', 'Pre-symptomatic',
                                    'Asymptomatic', 'Infectious', 'Recovered', 'Cumulative')) 
for (i in 1:28) {
        cut_f[cut_f$group %in% c(seq(i, 6*m+i, m)), 'group'] <- c(str_glue('age{age_id[i]}_vac{vac_id[i]}')) 
}


sele_f <- cut_f %>% 
        # filter(status %in% c('Susceptible', 'Asymptomatic', 'Infectious', 'Pre-symptomatic', 'Cumulative')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
        mutate(daily_new = Exposed * mu[1,1] * omega[1,1] + `Pre-symptomatic` * omegapp[1,1]) %>% 
        left_join(pop, by = c('group' = 'group')) %>%
        arrange(group, time) %>% 
        group_by(group) %>% 
        mutate(
                mean_7day_new = slide_dbl(
                        .x = daily_new,
                        .i = time,
                        .f = mean,
                        .before = 6
        )) %>% 
        mutate(rolling_week_incid = mean_7day_new / n_fujian * 10000) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        mutate(scenario = 'c_f')


wsf <- bind_rows(sele_w, sele_s, sele_f)
wsf <- wsf %>% 
        mutate(scenario = as.factor(scenario)) %>% 
        mutate(scenario = fct_relevel(scenario, 'w', 'c_s', 'c_f'))
# final_size <- wsf %>% 
#         group_by(scenario, age_group, vac_group) %>% 
#         summarise(f_s = min(Susceptible),
#                   .groups = 'drop') %>% 
#         arrange(scenario, age_group, desc(vac_group)) %>% 
#         left_join(pop, by = c('age_group', 'vac_group')) %>% 
#         mutate(f_s_p = (1 - f_s / n_fujian) * 100) %>% 
#         mutate(cumu = 100 - f_s_p)
# 
# final_size %>% 
#         select(f_s, n_fujian) %>% 
#         colSums()

# plot
facet_label <- c('Whole population', 'Close school', 'Close factory')
facet_hide <- levels(wsf$scenario)
names(facet_label) <- facet_hide
theme_delta <- function(){
                
                theme_classic()+
                theme(axis.title = element_text(face = 'bold'),
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
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
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
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
       
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
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
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
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
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
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
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
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
        ggplot(aes(x = time, y= rolling_week_incid, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                limits = c(0, 600),
                breaks = seq(0, 600, 150)
                
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


ggsave('rolling_week_delta2.pdf', height = 12, width = 8, dpi = 300)

