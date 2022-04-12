#Loading all necessary libraries 
pacman::p_load(
        deSolve,
        tidyverse,
        openxlsx,
        MetBrewer
)


# PARAMETERS --------------------------------------------------------------

m <- 28  #number of age and vac classes

# inital value FOR six compartment
V0 <- read.xlsx('pop_of_S_28.xlsx', sheet = 1)
V0 <- V0$n_fujian # inital value for number of susceptible stratified by vaccination status
E0 <- rep(0, 28) # inital value for number of exposed
P0 <- rep(0, 28)
A0 <- rep(0, 28)
I0 <- rep(0, 28) # inital value for number of infectious
I0[13] <- 1
R0 <- rep(0, 28) # inital value for number of recovered

# parameters
beta <- read.xlsx('Beta_28.xlsx', colNames = F)
beta <- as.matrix(beta) # matrix of transmission rates
kappa1 <- 0.63
kappa2 <- 0.35
HR <- rep(c(1, 0.6992, 0.5531, 0.3665), 7)
mu <- rep(c(0.33, 0.33, 0.33, 0.33), 7)
omega <- 1/4
omegap <- 1/4 # rate at which individuals move from the exposed to the pre-symptom classes
omegapp <- 1/1.8
gamma <- 1/14 # recovery rate
gammap <- 1/16

f <- rep(c(0.00035, 0.0024, 0.0121,
           0.03375, 0.07665, 0.1501, 0.21765), each = 4)

# combining parameter and initial values
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)
INPUT <- c(V0, E0, P0, A0, I0, R0)

ND <- 365 # time to simulate
TS <- 1 # time step to simualte is days
# constructing time vector
t_start <- 0 # starting time
t_end <- ND - 1 # ending time
t_inc <- TS #time increment
t_range <- seq(from= t_start, to=t_end+t_inc, by=t_inc) # vector with time steps


# differential equations --------------------------------------------------

diff_eqs <- function(times, Y, parms){
        dY <- numeric(length(Y))
        
        with(parms,{
                               
                for(i in 1:m){
                        # V_i
                        dY[i] <- - beta[, i] %*% (Y[4*m + seq(1:m)] + kappa1*Y[2*m + seq(1:m)] + kappa2*Y[3*m + seq(1:m)]) * HR[i] * Y[i] 
                        # E_i
                        dY[m+i] <- beta[, i] %*% (Y[4*m + seq(1:m)] + kappa1*Y[2*m + seq(1:m)] + kappa2*Y[3*m + seq(1:m)]) * HR[i] * Y[i] - mu[i]*omega*Y[m+i] - (1-mu[i])*omegap*Y[m+i]
                        # P_i
                        dY[2*m+i] <- (1-mu[i])*omegap*Y[m+i] - omegapp*Y[2*m+i]
                        # A_i
                        dY[3*m+i] <- mu[i]*omega*Y[m+i] - gammap*Y[3*m+i]
                        # I_i
                        dY[4*m+i] <- omegapp*Y[2*m+i] - gamma*Y[4*m+i] - f[i]*Y[4*m+i]
                        # R_i
                        dY[5*m+i] <- gamma*Y[4*m+i] + gammap*Y[3*m+i]
                }
                list(dY) 
        })
}

out <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
out <- as.data.frame(out)


# PLOTTING ----------------------------------------------------------------
df <- out %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(group = as.numeric(group),
               status = group) %>% 
        # mutate(status = replace(status, status < 29 ,"Susceptible")) %>% 
        # mutate(status = replace(status, status < 57, 'Exposed')) %>% 
        # mutate(status = replace(status, status < 85, 'Pre-symptomatic')) %>% 
        # mutate(status = replace(status, status < 113, 'Asymptomatic')) %>% 
        # mutate(status = replace(status, status < 141, 'Infectious')) %>% 
        # mutate(status = replace(status, status < 169, 'Recovered')) %>% 
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


selected_df <- df %>% 
        filter(status %in% c('Asymptomatic', 'Infectious')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1'))

# plot
library(scales)
facet_label <- c('0-9 years old', '10-19 years old', '20-29 years old',
                 '30-39 years old', '40-49 years old', '50-59 years old',
                 '60+ years old')
facet_hide <- levels(selected_df$age_group)
names(facet_label) <- facet_hide

selected_df %>% 
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
        scale_color_manual(values = rev(met.brewer('Egypt', 4)),
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
              legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = c(0.55, 0.17), 
              legend.justification = c(0, 1),
              plot.title = element_text(face = 'bold'),
              strip.text = element_text(hjust = 0, face = 'bold'),
              strip.background = element_rect(color = 'white'))+
        
        labs(
                x = "Time (days)",
                y = "Daily cases",
                color = NULL)+
        facet_wrap(~ age_group, ncol=2, scales =  "free_y",
                   labeller = labeller(age_group = facet_label))+
        # facet_grid(rows = vars(age_group), scales =  "free_y",
        #            labeller = labeller(age_group = facet_label))+
        guides(color = guide_legend(nrow=4,byrow=TRUE))

ggsave('Fig1_28.pdf', height = 9, width = 6, dpi = 300)
ggsave('Fig1.tiff', plot = p1, height = 9, width = 6, dpi = 300)

# CUT FACTORY -------------------------------------------------------------

#Loading all necessary libraries 
pacman::p_load(
        deSolve,
        tidyverse,
        openxlsx,
        MetBrewer
)


# PARAMETERS --------------------------------------------------------------

m <- 28  #number of age and vac classes

# inital value FOR six compartment
V0 <- read.xlsx('pop_of_S_28.xlsx', sheet = 1)
V0 <- V0$n_fujian # inital value for number of susceptible stratified by vaccination status
E0 <- rep(0, 28) # inital value for number of exposed
P0 <- rep(0, 28)
A0 <- rep(0, 28)
I0 <- rep(0, 28) # inital value for number of infectious
I0[13] <- 1
R0 <- rep(0, 28) # inital value for number of recovered

# parameters
beta <- read.xlsx('Beta_28_cut_f.xlsx', colNames = F)
beta <- as.matrix(beta) # matrix of transmission rates
kappa1 <- 0.63
kappa2 <- 0.35
HR <- rep(c(1, 0.6992, 0.5531, 0.3665), 7)
mu <- rep(c(0.33, 0.33, 0.33, 0.33), 7)
omega <- 1/4
omegap <- 1/4 # rate at which individuals move from the exposed to the pre-symptom classes
omegapp <- 1/1.8
gamma <- 1/14 # recovery rate
gammap <- 1/16

f <- rep(c(0.00035, 0.0024, 0.0121,
           0.03375, 0.07665, 0.1501, 0.21765), each = 4)

# combining parameter and initial values
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)
INPUT <- c(V0, E0, P0, A0, I0, R0)

ND <- 365 # time to simulate
TS <- 1 # time step to simualte is days
# constructing time vector
t_start <- 0 # starting time
t_end <- ND - 1 # ending time
t_inc <- TS #time increment
t_range <- seq(from= t_start, to=t_end+t_inc, by=t_inc) # vector with time steps


# differential equations --------------------------------------------------

diff_eqs <- function(times, Y, parms){
        dY <- numeric(length(Y))
        
        with(parms,{
                
                for(i in 1:m){
                        # V_i
                        dY[i] <- - beta[, i] %*% (Y[4*m + seq(1:m)] + kappa1*Y[2*m + seq(1:m)] + kappa2*Y[3*m + seq(1:m)]) * HR[i] * Y[i] 
                        # E_i
                        dY[m+i] <- beta[, i] %*% (Y[4*m + seq(1:m)] + kappa1*Y[2*m + seq(1:m)] + kappa2*Y[3*m + seq(1:m)]) * HR[i] * Y[i] - mu[i]*omega*Y[m+i] - (1-mu[i])*omegap*Y[m+i]
                        # P_i
                        dY[2*m+i] <- (1-mu[i])*omegap*Y[m+i] - omegapp*Y[2*m+i]
                        # A_i
                        dY[3*m+i] <- mu[i]*omega*Y[m+i] - gammap*Y[3*m+i]
                        # I_i
                        dY[4*m+i] <- omegapp*Y[2*m+i] - gamma*Y[4*m+i] - f[i]*Y[4*m+i]
                        # R_i
                        dY[5*m+i] <- gamma*Y[4*m+i] + gammap*Y[3*m+i]
                }
                list(dY) 
        })
}

out <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
out <- as.data.frame(out)


# PLOTTING ----------------------------------------------------------------
df <- out %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(group = as.numeric(group),
               status = group) %>% 
        # mutate(status = replace(status, status < 29 ,"Susceptible")) %>% 
        # mutate(status = replace(status, status < 57, 'Exposed')) %>% 
        # mutate(status = replace(status, status < 85, 'Pre-symptomatic')) %>% 
        # mutate(status = replace(status, status < 113, 'Asymptomatic')) %>% 
        # mutate(status = replace(status, status < 141, 'Infectious')) %>% 
        # mutate(status = replace(status, status < 169, 'Recovered')) %>% 
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


selected_df <- df %>% 
        filter(status %in% c('Asymptomatic', 'Infectious')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1'))

# plot
library(scales)
facet_label <- c('0-9 years old', '10-19 years old', '20-29 years old',
                 '30-39 years old', '40-49 years old', '50-59 years old',
                 '60+ years old')
facet_hide <- levels(selected_df$age_group)
names(facet_label) <- facet_hide

selected_df %>% 
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
        scale_color_manual(values = rev(met.brewer('Egypt', 4)),
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
              legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = c(0.55, 0.17), 
              legend.justification = c(0, 1),
              plot.title = element_text(face = 'bold'),
              strip.text = element_text(hjust = 0, face = 'bold'),
              strip.background = element_rect(color = 'white'))+
        
        labs(
                x = "Time (days)",
                y = "Daily cases",
                color = NULL)+
        facet_wrap(~ age_group, ncol=2, scales =  "free_y",
                   labeller = labeller(age_group = facet_label))+
        # facet_grid(rows = vars(age_group), scales =  "free_y",
        #            labeller = labeller(age_group = facet_label))+
        guides(color = guide_legend(nrow=4,byrow=TRUE))

ggsave('Fig1_28_cut_f.pdf', height = 9, width = 6, dpi = 300)
ggsave('Fig1.tiff', plot = p1, height = 9, width = 6, dpi = 300)


# CUT SCHOOL --------------------------------------------------------------

#Loading all necessary libraries 
pacman::p_load(
        deSolve,
        tidyverse,
        openxlsx,
        MetBrewer
)


# PARAMETERS --------------------------------------------------------------

m <- 28  #number of age and vac classes

# inital value FOR six compartment
V0 <- read.xlsx('pop_of_S_28.xlsx', sheet = 1)
V0 <- V0$n_fujian # inital value for number of susceptible stratified by vaccination status
E0 <- rep(0, 28) # inital value for number of exposed
P0 <- rep(0, 28)
A0 <- rep(0, 28)
I0 <- rep(0, 28) # inital value for number of infectious
I0[13] <- 1
R0 <- rep(0, 28) # inital value for number of recovered

# parameters
beta <- read.xlsx('Beta_28_cut_s.xlsx', colNames = F)
beta <- as.matrix(beta) # matrix of transmission rates
kappa1 <- 0.63
kappa2 <- 0.35
HR <- rep(c(1, 0.6992, 0.5531, 0.3665), 7)
mu <- rep(c(0.33, 0.33, 0.33, 0.33), 7)
omega <- 1/4
omegap <- 1/4 # rate at which individuals move from the exposed to the pre-symptom classes
omegapp <- 1/1.8
gamma <- 1/14 # recovery rate
gammap <- 1/16

f <- rep(c(0.00035, 0.0024, 0.0121,
           0.03375, 0.07665, 0.1501, 0.21765), each = 4)

# combining parameter and initial values
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)
INPUT <- c(V0, E0, P0, A0, I0, R0)

ND <- 365 # time to simulate
TS <- 1 # time step to simualte is days
# constructing time vector
t_start <- 0 # starting time
t_end <- ND - 1 # ending time
t_inc <- TS #time increment
t_range <- seq(from= t_start, to=t_end+t_inc, by=t_inc) # vector with time steps


# differential equations --------------------------------------------------

diff_eqs <- function(times, Y, parms){
        dY <- numeric(length(Y))
        
        with(parms,{
                
                for(i in 1:m){
                        # V_i
                        dY[i] <- - beta[, i] %*% (Y[4*m + seq(1:m)] + kappa1*Y[2*m + seq(1:m)] + kappa2*Y[3*m + seq(1:m)]) * HR[i] * Y[i] 
                        # E_i
                        dY[m+i] <- beta[, i] %*% (Y[4*m + seq(1:m)] + kappa1*Y[2*m + seq(1:m)] + kappa2*Y[3*m + seq(1:m)]) * HR[i] * Y[i] - mu[i]*omega*Y[m+i] - (1-mu[i])*omegap*Y[m+i]
                        # P_i
                        dY[2*m+i] <- (1-mu[i])*omegap*Y[m+i] - omegapp*Y[2*m+i]
                        # A_i
                        dY[3*m+i] <- mu[i]*omega*Y[m+i] - gammap*Y[3*m+i]
                        # I_i
                        dY[4*m+i] <- omegapp*Y[2*m+i] - gamma*Y[4*m+i] - f[i]*Y[4*m+i]
                        # R_i
                        dY[5*m+i] <- gamma*Y[4*m+i] + gammap*Y[3*m+i]
                }
                list(dY) 
        })
}

out <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
out <- as.data.frame(out)


# PLOTTING ----------------------------------------------------------------
df <- out %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(group = as.numeric(group),
               status = group) %>% 
        # mutate(status = replace(status, status < 29 ,"Susceptible")) %>% 
        # mutate(status = replace(status, status < 57, 'Exposed')) %>% 
        # mutate(status = replace(status, status < 85, 'Pre-symptomatic')) %>% 
        # mutate(status = replace(status, status < 113, 'Asymptomatic')) %>% 
        # mutate(status = replace(status, status < 141, 'Infectious')) %>% 
        # mutate(status = replace(status, status < 169, 'Recovered')) %>% 
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


selected_df <- df %>% 
        filter(status %in% c('Asymptomatic', 'Infectious')) %>% 
        pivot_wider(names_from = 'status',
                    values_from = 'value') %>% 
        mutate(I_A = Asymptomatic + Infectious) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1'))

# plot
library(scales)
facet_label <- c('0-9 years old', '10-19 years old', '20-29 years old',
                 '30-39 years old', '40-49 years old', '50-59 years old',
                 '60+ years old')
facet_hide <- levels(selected_df$age_group)
names(facet_label) <- facet_hide

selected_df %>% 
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
        scale_color_manual(values = rev(met.brewer('Egypt', 4)),
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
              legend.box.background=element_rect(colour = "black",linetype = "solid"), # 图例绘制边框
              legend.box.margin = margin(1,1,1,1),
              legend.position = c(0.55, 0.17), 
              legend.justification = c(0, 1),
              plot.title = element_text(face = 'bold'),
              strip.text = element_text(hjust = 0, face = 'bold'),
              strip.background = element_rect(color = 'white'))+
        
        labs(
                x = "Time (days)",
                y = "Daily cases",
                color = NULL)+
        facet_wrap(~ age_group, ncol=2, scales =  "free_y",
                   labeller = labeller(age_group = facet_label))+
        # facet_grid(rows = vars(age_group), scales =  "free_y",
        #            labeller = labeller(age_group = facet_label))+
        guides(color = guide_legend(nrow=4,byrow=TRUE))

ggsave('Fig1_28_cut_s.pdf', height = 9, width = 6, dpi = 300)
ggsave('Fig1.tiff', plot = p1, height = 9, width = 6, dpi = 300)

devtools::install_github("zeehio/facetscales")
library(facetscales)
detach(facetscales)
