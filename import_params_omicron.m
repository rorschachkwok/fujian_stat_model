n = 7;  % number of subgroups 

params.kappa1  = 0.63 * ones(n,4);
params.kappa2  = 0.35 * ones(n,4);
params.mu     = 0.55 .* ones(n,4);
params.omega  = 1/3 * ones(n,4);
params.omegap = 1/2 * ones(n,4);
params.omegapp = 1/1 * ones(n,4);
params.gamma  = 1/3 * ones(n,4);
params.gammap = 1/3 * ones(n,4);

params.f      = readmatrix('death_rate.xlsx',Sheet='omicron')

kappa1 = params.kappa1;
kappa2 = params.kappa2;
mu = params.mu;
omega = params.omega;
omegap = params.omegap;
omegapp = params.omegapp;
gamma = params.gamma;
gammap = params.gammap;
f = params.f;
