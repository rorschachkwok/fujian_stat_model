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
                dC <- omegapp*P + mu*omega*E
                list(c(t(dV), t(dE), t(dP), t(dA), t(dI), t(dR), t(dC))) # same order as the INPUT
        })
}

out <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
out <- as.data.frame(out)


# run delta ---------------------------------------------------------------

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

# run omicron -------------------------------------------------------------

# change param to cut factory
beta <- read.xlsx('Beta_28_cut_f.xlsx', sheet = 4, colNames = F)
beta <- as.matrix(beta) 
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate cut factory
cut_f <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cut_f <- as.data.frame(cut_f)

# change param to cut school
beta <- read.xlsx('Beta_28_cut_s.xlsx', sheet = 4, colNames = F)
beta <- as.matrix(beta) 
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate cut school
cut_s <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cut_s <- as.data.frame(cut_s)



# df management -----------------------------------------------------------
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
        mutate(final_size = Cumulative / n_fujian) %>% 
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
        mutate(final_size = Cumulative / n_fujian) %>% 
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
        mutate(final_size = Cumulative / n_fujian) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        mutate(scenario = 'c_f')


wsf <- bind_rows(sele_w, sele_s, sele_f)
wsf <- wsf %>% 
        mutate(scenario = as.factor(scenario)) %>% 
        mutate(scenario = fct_relevel(scenario, 'w', 'c_s', 'c_f'))

# plot
facet_label <- c('Whole population', 'School closure', 'Factory closure')
facet_hide <- levels(wsf$scenario)
names(facet_label) <- facet_hide

hfinal <- wsf %>% 
        group_by(age_group, vac_group, scenario) %>% 
        summarise(
                final_size = max(final_size),
                .groups = 'drop'
        )


# plot delta --------------------------------------------------------------

d_final <- ggplot(data = hfinal, aes(x = vac_group,   # x-axis is case age
                                y = age_group,     # y-axis is infector age
                                fill = final_size))+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                color = 'grey')+     # color of the tile is the Freq column in the data
        geom_text(aes(label = format(round(final_size, 3), nsmall = 3)), color = "black", size = 4) +
        coord_fixed()+ # 保持为正方形
        
        scale_fill_gradient(low = 'white', high = 'coral3',
                            # na.value = 'azure4',
                             limits=c(0, 1),
                             breaks=seq(0,1,by=0.2))+
        
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
                axis.title = element_text(face = 'bold', size = 10),
                plot.title = element_text(hjust = 0, size = 14),
                strip.text = element_text(face = 'bold', size = 10)
        )+
        facet_grid(cols = vars(scenario), 
                   labeller = labeller(scenario = facet_label))+
        labs(                         
                x = "Vaccination status",
                y = "Age category",
                title = 'a',
                fill = NULL     
        )

# plot omicron ------------------------------------------------------------
o_final <- ggplot(data = hfinal, aes(x = vac_group,   # x-axis is case age
                               y = age_group,     # y-axis is infector age
                               fill = final_size))+    # use long data, with proportions as Freq
        geom_tile(                    # visualize it in tiles
                color = 'grey')+     # color of the tile is the Freq column in the data
        geom_text(aes(label = format(round(final_size, 3), nsmall = 3)), color = "black", size = 4) +
        coord_fixed()+ # 保持为正方形
        
        scale_fill_gradient(low = 'white', high = 'coral3',
                            limits=c(0, 1),
                            breaks=seq(0,1,by=0.2))+
        
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
                axis.title = element_text(face = 'bold', size = 10),
                plot.title = element_text(hjust = 0, size = 14),
                strip.text = element_text(face = 'bold', size = 10)
        )+
        facet_grid(cols = vars(scenario), 
                   labeller = labeller(scenario = facet_label))+
        labs(                         
                x = "Vaccination status",
                y = "Age category",
                title = 'b',
                fill = NULL     
        )

final_size_plot <- plot_grid(d_final, o_final, nrow = 2)
ggsave('final_size_plot.pdf', height = 10, width = 8, dpi = 300)
