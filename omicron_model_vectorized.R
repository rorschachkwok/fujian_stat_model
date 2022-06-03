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
beta <- read.xlsx('Beta_28.xlsx', sheet = 4, colNames = F)
beta <- as.matrix(beta) # matrix of transmission rates
kappa1 <- matrix(rep(0.63, 28), nrow = 7, byrow = T)
kappa2 <- matrix(rep(0.35, 28), nrow = 7, byrow = T)
HR <- matrix(rep(c(1, 0.99, 0.97, 0.97), 7), nrow = 7, byrow = T)
mu <- matrix(rep(0.55, 28), nrow = 7, byrow = T)
omega <- matrix(rep(1/3, 28), nrow = 7, byrow = T)
omegap <- matrix(rep(1/2, 28), nrow = 7, byrow = T) # rate at which individuals move from the exposed to the pre-symptom classes
omegapp <- matrix(rep(1, 28), nrow = 7, byrow = T)
gamma <- matrix(rep(1/3, 28), nrow = 7, byrow = T) # recovery rate
gammap <- matrix(rep(1/3, 28), nrow = 7, byrow = T)

f_raw <- read.xlsx('death_rate.xlsx', sheet = 2, colNames = F)
f <- as.matrix(f_raw)

# combining parameter and initial values
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)
INPUT <- c(t(V0), t(E0), t(P0), t(A0), t(I0), t(R0), t(C0)) # same order as ode return list

t_range <- seq(from = 0, to = 365, by = 1) # vector with time steps


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

# change param to only community and others
beta <- read.xlsx('Beta_28_cno.xlsx', sheet = 4, colNames = F)
beta <- as.matrix(beta)
parms <- list(kappa1 = kappa1, kappa2 = kappa2, HR = HR, beta=beta, mu=mu, omega=omega, omegap = omegap,
              omegapp = omegapp, gamma=gamma, gammap = gammap, f = f)

# simulate only community and others
cno <- ode(INPUT, t_range, diff_eqs, parms, method = 'rk4')
cno <- as.data.frame(cno)


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

# manage community and others
cno <- cno %>% 
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
        cno[cno$group %in% c(seq(i, 6*m+i, m)), 'group'] <- c(str_glue('age{age_id[i]}_vac{vac_id[i]}')) 
}


sele_cno <- cno %>% 
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
        mutate(scenario = 'cno')


wsf <- bind_rows(sele_w, sele_s, sele_f, sele_cno)
wsf <- wsf %>% 
        mutate(scenario = as.factor(scenario)) %>% 
        mutate(scenario = fct_relevel(scenario, 'w', 'c_s', 'c_f', 'cno'))

# peak incidence among age groups and scenarios ---------------------------

peak_incid <- wsf %>% group_by(time, age_group, scenario) %>% 
        summarise(
                sum_vac_incid = sum(rolling_week_incid, na.rm = T),
                .groups = 'drop') %>%  
        group_by(age_group, scenario) %>% 
        summarise(peak_incid = max(sum_vac_incid, na.rm = T),
                  .groups = 'drop') 
w_peak_incid <- peak_incid %>% 
        filter(scenario == 'w')
w_peak_incid <- w_peak_incid$peak_incid

reduce_incid <- peak_incid %>% 
        mutate(w_peak_incid = rep(w_peak_incid, each = 4),
               prop_to_w = peak_incid / w_peak_incid,
               reduce_incid = 1 - prop_to_w)