V0 <- read.xlsx('pop_of_S_28.xlsx', sheet = 1)
V0 <- V0$n_fujian # inital value for number of susceptible stratified by vaccination status
V0 <- matrix(V0, nrow = 7, byrow = T)
E0 <- matrix(rep(0, 28), nrow = 7, byrow = T) # inital value for number of exposed
P0 <- matrix(rep(0, 28), nrow = 7, byrow = T)
A0 <- matrix(rep(0, 28), nrow = 7, byrow = T)
I0 <- matrix(rep(0, 28), nrow = 7, byrow = T) # inital value for number of infectious
I0[4, 1] <- 1
R0 <- matrix(rep(0, 28), nrow = 7, byrow = T) # inital value for number of recovered

# parameters
beta <- read.xlsx('Beta_28.xlsx', sheet = 2, colNames = F)
beta <- as.matrix(beta) # matrix of transmission rates
kappa1 <- matrix(rep(0.63, 28), nrow = 7, byrow = T)
kappa2 <- matrix(rep(0.35, 28), nrow = 7, byrow = T)
HR <- matrix(rep(c(1, 0.6992, 0.5531, 0.3665), 7), nrow = 7, byrow = T)
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
INPUT <- c(V0, E0, P0, A0, I0, R0)
# INPUT <- list(V = V0, E = E0, P = P0, A = A0, I = I0, R = R0)

ND <- 365 # time to simulate
TS <- 1 # time step to simualte is days
# constructing time vector
t_start <- 0 # starting time
t_end <- ND - 1 # ending time
t_inc <- TS #time increment
t_range <- seq(from= t_start, to=t_end+t_inc, by=t_inc) # vector with time steps


# differential equations --------------------------------------------------

diff_eqs <- function(times, INPUT, parms){
        # V <- matrix(INPUT[[1]][1:28], nrow = 7)
        # E <- matrix(INPUT[[1]][29:56], nrow = 7)
        # P <- matrix(INPUT[[1]][57:84], nrow = 7)
        # A <- matrix(INPUT[[1]][85:112], nrow = 7)
        # I <- matrix(INPUT[[1]][113:140], nrow = 7)
        # R <- matrix(INPUT[[1]][141:168], nrow = 7)
        a <- list(V = V0, E = E0, P = P0, A = A0, I = I0, R = R0, kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
                  omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)
        with(a, {
                        
                        lambda <- t(beta) %*% rowSums(I + kappa1*P + kappa2*A)
                        dV <- -matrix(rep(as.numeric(t(lambda)), each = 4), nrow = nrow(lambda), byrow = TRUE) * HR * V  
                        dE <- matrix(rep(as.numeric(t(lambda)), each = 4), nrow = nrow(lambda), byrow = TRUE) - mu*omega*E - (1-mu)*omegap*E
                        dP <- (1-mu)*omegap*E - omegapp*P
                        dA <- mu*omega*E - gammap*A
                        dI <- omegapp*P - gamma*I - f*I
                        dR <- gamma*I + gammap*A
                        return(list(c(dV, dE, dP, dA, dI, dR)))
                
                # list(dY) 
        })
}

out <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
out <- as.data.frame(out)
m <- 28
# change param to cut factory
beta <- read.xlsx('Beta_28_cut_f.xlsx', sheet = 2, colNames = F)
beta <- as.matrix(beta) 
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate cut factory
cut_f <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cut_f <- as.data.frame(cut_f)

# change param to cut school
beta <- read.xlsx('Beta_28_cut_s.xlsx', sheet = 2, colNames = F)
beta <- as.matrix(beta) 
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate cut school
cut_s <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cut_s <- as.data.frame(cut_s)





df <- out %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(group = as.numeric(group),
               status = group) %>% 
        
        mutate(status = case_when(status %in% 1:28 ~ 'Susceptible',
                                  status %in% 29:56 ~ 'Exposed',
                                  status %in% 57:84 ~ 'Pre-symptomatic',
                                  status %in% 85:112 ~ 'Asymptomatic',
                                  status %in% 113:140 ~ 'Infectious',
                                  status %in% 141:168 ~ 'Recovered')) %>% 
        mutate(status = fct_relevel(status, "Susceptible", 'Exposed', 'Pre-symptomatic',
                                    'Asymptomatic', 'Infectious', 'Recovered')) %>% 
        mutate(group = case_when(group %in% seq(1, 141, 28) ~ 'age1_vac1',
                                 group %in% seq(2, 142, 28) ~ 'age1_vac2',
                                 group %in% seq(3, 143, 28) ~ 'age1_vac3',
                                 group %in% seq(4, 144, 28) ~ 'age1_vac4',
                                 group %in% seq(5, 145, 28) ~ 'age2_vac1',
                                 group %in% seq(6, 146, 28) ~ 'age2_vac2',
                                 group %in% seq(7, 147, 28) ~ 'age2_vac3',
                                 group %in% seq(8, 148, 28) ~ 'age2_vac4',
                                 group %in% seq(9, 149, 28) ~ 'age3_vac1',
                                 group %in% seq(10, 150, 28) ~ 'age3_vac2',
                                 group %in% seq(11, 151, 28) ~ 'age3_vac3',
                                 group %in% seq(12, 152, 28) ~ 'age3_vac4',
                                 group %in% seq(13, 153, 28) ~ 'age4_vac1',
                                 group %in% seq(14, 154, 28) ~ 'age4_vac2',
                                 group %in% seq(15, 155, 28) ~ 'age4_vac3',
                                 group %in% seq(16, 156, 28) ~ 'age4_vac4',
                                 group %in% seq(17, 157, 28) ~ 'age5_vac1',
                                 group %in% seq(18, 158, 28) ~ 'age5_vac2',
                                 group %in% seq(19, 159, 28) ~ 'age5_vac3',
                                 group %in% seq(20, 160, 28) ~ 'age5_vac4',
                                 group %in% seq(21, 161, 28) ~ 'age6_vac1',
                                 group %in% seq(22, 162, 28) ~ 'age6_vac2',
                                 group %in% seq(23, 163, 28) ~ 'age6_vac3',
                                 group %in% seq(24, 164, 28) ~ 'age6_vac4',
                                 group %in% seq(25, 165, 28) ~ 'age7_vac1',
                                 group %in% seq(26, 166, 28) ~ 'age7_vac2',
                                 group %in% seq(27, 167, 28) ~ 'age7_vac3',
                                 group %in% seq(28, 168, 28) ~ 'age7_vac4'))


sele_w <- df %>% 
        filter(status %in% c('Asymptomatic', 'Infectious')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
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
        mutate(group = as.numeric(group),
               status = group) %>% 
        
        mutate(status = case_when(status %in% 1:28 ~ 'Susceptible',
                                  status %in% 29:56 ~ 'Exposed',
                                  status %in% 57:84 ~ 'Pre-symptomatic',
                                  status %in% 85:112 ~ 'Asymptomatic',
                                  status %in% 113:140 ~ 'Infectious',
                                  status %in% 141:168 ~ 'Recovered')) %>% 
        mutate(status = fct_relevel(status, "Susceptible", 'Exposed', 'Pre-symptomatic',
                                    'Asymptomatic', 'Infectious', 'Recovered')) %>% 
        mutate(group = case_when(group %in% seq(1, 141, 28) ~ 'age1_vac1',
                                 group %in% seq(2, 142, 28) ~ 'age1_vac2',
                                 group %in% seq(3, 143, 28) ~ 'age1_vac3',
                                 group %in% seq(4, 144, 28) ~ 'age1_vac4',
                                 group %in% seq(5, 145, 28) ~ 'age2_vac1',
                                 group %in% seq(6, 146, 28) ~ 'age2_vac2',
                                 group %in% seq(7, 147, 28) ~ 'age2_vac3',
                                 group %in% seq(8, 148, 28) ~ 'age2_vac4',
                                 group %in% seq(9, 149, 28) ~ 'age3_vac1',
                                 group %in% seq(10, 150, 28) ~ 'age3_vac2',
                                 group %in% seq(11, 151, 28) ~ 'age3_vac3',
                                 group %in% seq(12, 152, 28) ~ 'age3_vac4',
                                 group %in% seq(13, 153, 28) ~ 'age4_vac1',
                                 group %in% seq(14, 154, 28) ~ 'age4_vac2',
                                 group %in% seq(15, 155, 28) ~ 'age4_vac3',
                                 group %in% seq(16, 156, 28) ~ 'age4_vac4',
                                 group %in% seq(17, 157, 28) ~ 'age5_vac1',
                                 group %in% seq(18, 158, 28) ~ 'age5_vac2',
                                 group %in% seq(19, 159, 28) ~ 'age5_vac3',
                                 group %in% seq(20, 160, 28) ~ 'age5_vac4',
                                 group %in% seq(21, 161, 28) ~ 'age6_vac1',
                                 group %in% seq(22, 162, 28) ~ 'age6_vac2',
                                 group %in% seq(23, 163, 28) ~ 'age6_vac3',
                                 group %in% seq(24, 164, 28) ~ 'age6_vac4',
                                 group %in% seq(25, 165, 28) ~ 'age7_vac1',
                                 group %in% seq(26, 166, 28) ~ 'age7_vac2',
                                 group %in% seq(27, 167, 28) ~ 'age7_vac3',
                                 group %in% seq(28, 168, 28) ~ 'age7_vac4'))


sele_s <- cut_s %>% 
        filter(status %in% c('Asymptomatic', 'Infectious')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
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
        mutate(group = as.numeric(group),
               status = group) %>% 
        
        mutate(status = case_when(status %in% 1:28 ~ 'Susceptible',
                                  status %in% 29:56 ~ 'Exposed',
                                  status %in% 57:84 ~ 'Pre-symptomatic',
                                  status %in% 85:112 ~ 'Asymptomatic',
                                  status %in% 113:140 ~ 'Infectious',
                                  status %in% 141:168 ~ 'Recovered')) %>% 
        mutate(status = fct_relevel(status, "Susceptible", 'Exposed', 'Pre-symptomatic',
                                    'Asymptomatic', 'Infectious', 'Recovered')) %>% 
        mutate(group = case_when(group %in% seq(1, 141, 28) ~ 'age1_vac1',
                                 group %in% seq(2, 142, 28) ~ 'age1_vac2',
                                 group %in% seq(3, 143, 28) ~ 'age1_vac3',
                                 group %in% seq(4, 144, 28) ~ 'age1_vac4',
                                 group %in% seq(5, 145, 28) ~ 'age2_vac1',
                                 group %in% seq(6, 146, 28) ~ 'age2_vac2',
                                 group %in% seq(7, 147, 28) ~ 'age2_vac3',
                                 group %in% seq(8, 148, 28) ~ 'age2_vac4',
                                 group %in% seq(9, 149, 28) ~ 'age3_vac1',
                                 group %in% seq(10, 150, 28) ~ 'age3_vac2',
                                 group %in% seq(11, 151, 28) ~ 'age3_vac3',
                                 group %in% seq(12, 152, 28) ~ 'age3_vac4',
                                 group %in% seq(13, 153, 28) ~ 'age4_vac1',
                                 group %in% seq(14, 154, 28) ~ 'age4_vac2',
                                 group %in% seq(15, 155, 28) ~ 'age4_vac3',
                                 group %in% seq(16, 156, 28) ~ 'age4_vac4',
                                 group %in% seq(17, 157, 28) ~ 'age5_vac1',
                                 group %in% seq(18, 158, 28) ~ 'age5_vac2',
                                 group %in% seq(19, 159, 28) ~ 'age5_vac3',
                                 group %in% seq(20, 160, 28) ~ 'age5_vac4',
                                 group %in% seq(21, 161, 28) ~ 'age6_vac1',
                                 group %in% seq(22, 162, 28) ~ 'age6_vac2',
                                 group %in% seq(23, 163, 28) ~ 'age6_vac3',
                                 group %in% seq(24, 164, 28) ~ 'age6_vac4',
                                 group %in% seq(25, 165, 28) ~ 'age7_vac1',
                                 group %in% seq(26, 166, 28) ~ 'age7_vac2',
                                 group %in% seq(27, 167, 28) ~ 'age7_vac3',
                                 group %in% seq(28, 168, 28) ~ 'age7_vac4'))


sele_f <- cut_f %>% 
        filter(status %in% c('Asymptomatic', 'Infectious')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        mutate(scenario = 'c_f')
wsf <- bind_rows(sele_w, sele_s)
wsf <- bind_rows(wsf, sele_f)
wsf <- wsf %>% 
        mutate(scenario = as.factor(scenario)) %>% 
        mutate(scenario = fct_relevel(scenario, 'w', 'c_s', 'c_f'))
# plot
facet_label <- c('Whole population', 'Close school', 'Close factory')
facet_hide <- levels(wsf$scenario)
names(facet_label) <- facet_hide
ag1 <- wsf %>% 
        filter(age_group == 'age1') %>% 
        ggplot(aes(x = time, y= I_A, color = vac_group)) +
        
        geom_line() +
        
        scale_y_continuous(
                expand = expansion(0),
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 50),
                labels = seq(0, 300, 50)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since 2nd dose', 
                           '31-60 days since 2nd dose', 
                           '60+ days since 2nd dose', 
                           'At most partilly vaccinated')) + 
        
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              # legend.position = 'top',
              # legend.justification = 'left',
              # legend.key.width = unit(1, 'cm'),
              # legend.text = element_text(size = 12),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              # legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'top', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              # plot.title = element_text(face = 'bold', hjust = 0.5, size = 5),
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
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 50),
                labels = seq(0, 300, 50)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since 2nd dose', 
                           '31-60 days since 2nd dose', 
                           '60+ days since 2nd dose', 
                           'At most partilly vaccinated')) + 
        
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              # legend.position = 'top',
              # legend.justification = 'left',
              # legend.key.width = unit(1, 'cm'),
              # legend.text = element_text(size = 12),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              # legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'none', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              # plot.title = element_text(face = 'bold', hjust = 0.5, size = 5),
              strip.text = element_blank(),
              strip.background = element_rect(color = 'white'))+
        
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
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 50),
                labels = seq(0, 300, 50)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since 2nd dose', 
                           '31-60 days since 2nd dose', 
                           '60+ days since 2nd dose', 
                           'At most partilly vaccinated')) + 
        
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              # legend.position = 'top',
              # legend.justification = 'left',
              # legend.key.width = unit(1, 'cm'),
              # legend.text = element_text(size = 12),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              # legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'none', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              # plot.title = element_text(face = 'bold', hjust = 0.5, size = 5),
              strip.text = element_blank(),
              strip.background = element_rect(color = 'white'))+
        
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
                labels = comma
                # limits=c(0, 1260000),
                # breaks=seq(0,1250000,by=250000)
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 50),
                labels = seq(0, 300, 50)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since 2nd dose', 
                           '31-60 days since 2nd dose', 
                           '60+ days since 2nd dose', 
                           'At most partilly vaccinated')) + 
        
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              # legend.position = 'top',
              # legend.justification = 'left',
              # legend.key.width = unit(1, 'cm'),
              # legend.text = element_text(size = 12),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              # legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'none', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              # plot.title = element_text(face = 'bold', hjust = 0.5, size = 5),
              strip.text = element_blank(),
              strip.background = element_rect(color = 'white'))+
        
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
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 50),
                labels = seq(0, 300, 50)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since 2nd dose', 
                           '31-60 days since 2nd dose', 
                           '60+ days since 2nd dose', 
                           'At most partilly vaccinated')) + 
        
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              # legend.position = 'top',
              # legend.justification = 'left',
              # legend.key.width = unit(1, 'cm'),
              # legend.text = element_text(size = 12),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              # legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'none', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              # plot.title = element_text(face = 'bold', hjust = 0.5, size = 5),
              strip.text = element_blank(),
              strip.background = element_rect(color = 'white'))+
        
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
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 50),
                labels = seq(0, 300, 50)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since 2nd dose', 
                           '31-60 days since 2nd dose', 
                           '60+ days since 2nd dose', 
                           'At most partilly vaccinated')) + 
        
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              # legend.position = 'top',
              # legend.justification = 'left',
              # legend.key.width = unit(1, 'cm'),
              # legend.text = element_text(size = 12),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              # legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'none', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              # plot.title = element_text(face = 'bold', hjust = 0.5, size = 5),
              strip.text = element_blank(),
              strip.background = element_rect(color = 'white'))+
        
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
                labels = comma
        )+
        scale_x_continuous(
                expand = expansion(0),
                breaks = seq(0, 300, 50),
                labels = seq(0, 300, 50)
        )+
        scale_color_manual(values = c(
                "#43b284","#0f7ba2","#fab255","#dd5129"),
                labels = c('14-30 days since 2nd dose', 
                           '31-60 days since 2nd dose', 
                           '60+ days since 2nd dose', 
                           'At most partilly vaccinated')) + 
        
        theme_classic()+
        theme(axis.title = element_text(face = 'bold'),
              # legend.position = 'top',
              # legend.justification = 'left',
              # legend.key.width = unit(1, 'cm'),
              # legend.text = element_text(size = 12),
              legend.spacing.x=unit(0.1,'cm'), # 定义文本水平距离
              legend.key.width=unit(1.5,'cm'), # 定义图例水平大小
              legend.key.height=unit(0.5,'cm'), # 定义图例垂直大小
              legend.background=element_blank(), # 设置背景为空
              # legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = 'none', 
              legend.text = element_text(size = 10),
              legend.justification = c(0, 1),
              # plot.title = element_text(face = 'bold', hjust = 0.5, size = 5),
              strip.text = element_blank(),
              strip.background = element_rect(color = 'white'))+
        
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


ggsave('vec_test_delta.pdf', height = 12, width = 8, dpi = 300)
