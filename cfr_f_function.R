f_raw <- gamma*f_raw / (1+f_raw)

myfunction <- function(t, y) {
        1/2 * (y[-length(y)]+y[-1]) %*% (t[-1] - t[-length(t)])
}

y <- as.vector(out$`57`) 
t <- t_range

i <- as.vector(out$`113`)

cfr <- 0.000322581
temp <- 0.2*cfr / (1+cfr)
cfr * omegapp[1,1] * myfunction(t, y)[1,1]
temp * myfunction(t, i)[1,1]


