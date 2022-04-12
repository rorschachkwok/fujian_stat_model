pacman::p_load(
        deSolve,
        tidyverse
)

# defining parameters for this function
vepair <- function(lambda, VE, mu, omega, omegap, omegapp, gamma, gammap, f, 
                     v_ij, E_ij, P_ij, A_ij, I_ij, R_ij, 
                     times) {
        
        # the differential equations:
        vepair_equations <- function(time, variables, parameters) {
                with(as.list(c(variables, parameters)), {
                        dV <- -lambda*(1-VE)*V
                        dE <- lambda*(1-VE)*V - mu*omega*E - (1-mu)*omegap*E
                        dP <- (1-mu)*omegap*E - omegapp*P
                        dA <- mu*omega*E - gammap*A
                        dI <- omegapp*P - gamma*I - f*I
                        dR <- gamma*I + gammap*A
                        return(list(c(dV, dE, dP, dA, dI, dR)))
                })
        }
        
        # the parameters values:
        parameters_values <- c(
                beta = beta,
                VE   = VE,
                omega = omega,
                p = p,
                q_1 = q_1,
                q_2 = q_2,
                gamma = gamma,
                sigma = sigma,
                epsilon = epsilon,
                zeta = zeta,
                theta = theta,
                mu = mu,
                f = f,
        )
        
        # the initial values of variables:
        initial_values <- c(
                V = V0,
                E = E0,
                P = P0,
                A = A0,
                I = I0,
                R = R0
        )
        
        # solving
        out <- ode(initial_values, times, vepair_equations, parameters_values)
        
        # returning the output:
        as.data.frame(out)
}

vepair(lambda, VE_ij = matrix(nrow = 3, ncol = 1), q_ij, omega_1, omega_2, omega_3, gamma_1, gamma_2, f, 
       v_ij, E_ij, P_ij, I_ij, A_ij, R_ij, 
       times = seq(0, 60))
VE_ij = matrix(data = 1:12, nrow = 3, ncol = 4 , dimnames = list(c('0-17', '18-59', '60-'),
                                                                 c('unvaccinated', '<1 month', '1 month', '>1 month')))
