#Loading all necessary libraries 
pacman::p_load(
        deSolve,
        tidyverse,
        openxlsx,
        MetBrewer,
        ggsci,
        scales,
        patchwork
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
D0 <- rep(0, 28)

# parameters
beta <- read.xlsx('Beta_28.xlsx', sheet = 1, colNames = F)
beta <- as.matrix(beta) # matrix of transmission rates
kappa1 <- 0.63
kappa2 <- 0.35
HR <- rep(c(1, 0.6992, 0.5531, 0.3665), 7)
mu <- rep(c(0.33, 0.33, 0.33, 0.33), 7)
omega <- 1/3
omegap <- 1/3 # rate at which individuals move from the exposed to the pre-symptom classes
omegapp <- 1/2
gamma <- 1/5 # recovery rate
gammap <- 1/7

f_raw <- read.xlsx('death_rate.xlsx', sheet = 1, colNames = F)
f_raw <- as.matrix(f_raw)
f <- as.vector(t(f_raw))

# combining parameter and initial values
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)
INPUT <- c(V0, E0, P0, A0, I0, R0, D0)

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
                        # D_i
                        dY[6*m+i] <- omegapp*Y[2*m+i]
                }
                list(dY) 
        })
}

out <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
out <- as.data.frame(out)

# change param to cut factory
beta <- read.xlsx('Beta_28_cut_f.xlsx', sheet = 1, colNames = F)
beta <- as.matrix(beta) 
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate cut factory
cut_f <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cut_f <- as.data.frame(cut_f)

# change param to cut school
beta <- read.xlsx('Beta_28_cut_s.xlsx', sheet = 1, colNames = F)
beta <- as.matrix(beta) 
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate cut school
cut_s <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cut_s <- as.data.frame(cut_s)

# df management -----------------------------------------------------------

sele_w <- out %>% 
        select(1, 170:197) %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(group = as.numeric(group)) %>% 
        
        mutate(group = case_when(group == 169 ~ 'age1_vac1',
                                 group == 170 ~ 'age1_vac2',
                                 group == 171 ~ 'age1_vac3',
                                 group == 172 ~ 'age1_vac4',
                                 group == 173 ~ 'age2_vac1',
                                 group == 174 ~ 'age2_vac2',
                                 group == 175 ~ 'age2_vac3',
                                 group == 176 ~ 'age2_vac4',
                                 group == 177 ~ 'age3_vac1',
                                 group == 178 ~ 'age3_vac2',
                                 group == 179 ~ 'age3_vac3',
                                 group == 180 ~ 'age3_vac4',
                                 group == 181 ~ 'age4_vac1',
                                 group == 182 ~ 'age4_vac2',
                                 group == 183 ~ 'age4_vac3',
                                 group == 184 ~ 'age4_vac4',
                                 group == 185 ~ 'age5_vac1',
                                 group == 186 ~ 'age5_vac2',
                                 group == 187 ~ 'age5_vac3',
                                 group == 188 ~ 'age5_vac4',
                                 group == 189 ~ 'age6_vac1',
                                 group == 190 ~ 'age6_vac2',
                                 group == 191 ~ 'age6_vac3',
                                 group == 192 ~ 'age6_vac4',
                                 group == 193 ~ 'age7_vac1',
                                 group == 194 ~ 'age7_vac2',
                                 group == 195 ~ 'age7_vac3',
                                 group == 196 ~ 'age7_vac4')) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        mutate(scenario = 'w')
# sele_w %>% 
#         filter(age_group == 'age7') %>% 
#         filter(vac_group == 'vac1') %>% 
#         summarise(max = max(value))
sele_s <- cut_s %>% 
        select(1, 170:197) %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(group = as.numeric(group)) %>% 
        
        mutate(group = case_when(group == 169 ~ 'age1_vac1',
                                 group == 170 ~ 'age1_vac2',
                                 group == 171 ~ 'age1_vac3',
                                 group == 172 ~ 'age1_vac4',
                                 group == 173 ~ 'age2_vac1',
                                 group == 174 ~ 'age2_vac2',
                                 group == 175 ~ 'age2_vac3',
                                 group == 176 ~ 'age2_vac4',
                                 group == 177 ~ 'age3_vac1',
                                 group == 178 ~ 'age3_vac2',
                                 group == 179 ~ 'age3_vac3',
                                 group == 180 ~ 'age3_vac4',
                                 group == 181 ~ 'age4_vac1',
                                 group == 182 ~ 'age4_vac2',
                                 group == 183 ~ 'age4_vac3',
                                 group == 184 ~ 'age4_vac4',
                                 group == 185 ~ 'age5_vac1',
                                 group == 186 ~ 'age5_vac2',
                                 group == 187 ~ 'age5_vac3',
                                 group == 188 ~ 'age5_vac4',
                                 group == 189 ~ 'age6_vac1',
                                 group == 190 ~ 'age6_vac2',
                                 group == 191 ~ 'age6_vac3',
                                 group == 192 ~ 'age6_vac4',
                                 group == 193 ~ 'age7_vac1',
                                 group == 194 ~ 'age7_vac2',
                                 group == 195 ~ 'age7_vac3',
                                 group == 196 ~ 'age7_vac4')) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        mutate(scenario = 'c_s')

sele_f <- cut_f %>% 
        select(1, 170:197) %>% 
        pivot_longer(
                cols = -1,
                names_to = 'group',
                values_to = 'value'
        ) %>% 
        mutate(group = as.numeric(group)) %>% 
        
        mutate(group = case_when(group == 169 ~ 'age1_vac1',
                                 group == 170 ~ 'age1_vac2',
                                 group == 171 ~ 'age1_vac3',
                                 group == 172 ~ 'age1_vac4',
                                 group == 173 ~ 'age2_vac1',
                                 group == 174 ~ 'age2_vac2',
                                 group == 175 ~ 'age2_vac3',
                                 group == 176 ~ 'age2_vac4',
                                 group == 177 ~ 'age3_vac1',
                                 group == 178 ~ 'age3_vac2',
                                 group == 179 ~ 'age3_vac3',
                                 group == 180 ~ 'age3_vac4',
                                 group == 181 ~ 'age4_vac1',
                                 group == 182 ~ 'age4_vac2',
                                 group == 183 ~ 'age4_vac3',
                                 group == 184 ~ 'age4_vac4',
                                 group == 185 ~ 'age5_vac1',
                                 group == 186 ~ 'age5_vac2',
                                 group == 187 ~ 'age5_vac3',
                                 group == 188 ~ 'age5_vac4',
                                 group == 189 ~ 'age6_vac1',
                                 group == 190 ~ 'age6_vac2',
                                 group == 191 ~ 'age6_vac3',
                                 group == 192 ~ 'age6_vac4',
                                 group == 193 ~ 'age7_vac1',
                                 group == 194 ~ 'age7_vac2',
                                 group == 195 ~ 'age7_vac3',
                                 group == 196 ~ 'age7_vac4')) %>% 
        separate(group, into = c('age_group', 'vac_group'), sep = '_') %>% 
        mutate(across(.cols = contains('group'),
                      .fns  = as.factor)) %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac4', 'vac3', 'vac2', 'vac1')) %>% 
        mutate(scenario = 'c_f')
# sele_w %>% 
#         filter(age_group == 'age7') %>% 
#         arrange(desc(value))
# sele_f %>% 
#         filter(age_group == 'age7') %>% 
#         arrange(desc(value))
wsf <- bind_rows(sele_w, sele_s)
wsf <- bind_rows(wsf, sele_f)
wsf <- wsf %>% 
        mutate(scenario = as.factor(scenario)) %>% 
        mutate(scenario = fct_relevel(scenario, 'w', 'c_s', 'c_f'))

# produce heatmap data frame
f_rate <- matrix(f, ncol = 1, 
                 dimnames = list(
                         NULL,
                         c('f_rate')
                 ))
f_rate <- as.data.frame(f_rate)

hmap_w <- sele_w %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac1', 'vac2', 'vac3', 'vac4')) %>% 
        group_by(age_group, vac_group) %>% 
        summarise(
                max       = max(value),
                .groups = 'drop'
        ) %>% 
        bind_cols(f_rate) %>%
        mutate(
                f_toll    = max * f_rate,
                f_per_10k = round(f_toll / 41540086 * 10000, digits = 3),
        )
        
hmap_f <- sele_f %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac1', 'vac2', 'vac3', 'vac4')) %>% 
        group_by(age_group, vac_group) %>% 
        summarise(
                max       = max(value),
                .groups = 'drop'
        ) %>% 
        bind_cols(f_rate) %>%
        mutate(
                f_toll    = max * f_rate,
                f_per_10k = round(f_toll / 41540086 * 10000, digits = 3),
        )

hmap_s <- sele_s %>% 
        mutate(vac_group = fct_relevel(vac_group, 'vac1', 'vac2', 'vac3', 'vac4')) %>% 
        group_by(age_group, vac_group) %>% 
        summarise(
                max       = max(value),
                .groups = 'drop'
        ) %>% 
        bind_cols(f_rate) %>%
        mutate(
                f_toll    = max * f_rate,
                f_per_10k = round(f_toll / 41540086 * 10000, digits = 3),
        )
