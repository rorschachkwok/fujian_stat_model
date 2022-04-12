clear all; close all; clc;

import_params_delta;   % import parameters omega, gamma, p, kappa,...
N = readmatrix('agePopulationVector.xlsx'); % the population vector (stratified by age)
C = readmatrix('cntm_cut_factory.xlsx', Sheet='Sheet 1');   % the contact matrix


params.Beta_cut_f   = C * 0.1589 ./ (N'); 
Beta_28_cut_f = kron(params.Beta_cut_f, ones(4));
% params.Beta_cut_s   = C * 0.1589 ./ (N');
% Beta_28_cut_s = kron(params.Beta_cut_s, ones(4));

% a series R0 for simulation
%R0 = [0.2, 0.5:6.5];     R0 = R0'; 
R0 = [5.08]';


%% probability vector q (DBM and NGM are adopted respectively), for infection by a one-time contact
% q_DBM = zeros(numel(R0),1);
q_NGM = zeros(numel(R0),1);

for i = 1:n
    for j = 1:n
        R(i,j) = C(j,i) / (mu(i)*omega(i) + (1-mu(i))*omegap(i)) * (kappa2(i)*mu(i)*omega(i)/(gammap(i)) + (1-mu(i))*omegap(i)/(gamma(i)+f(i)) + (kappa1(i)*(1-mu(i))*omegap(i)/omegapp(i)));
    end
end


for i = 1:numel(R0)
      
    temp1 = 1 / (mu(i)*omega(i) + (1-mu(i))*omegap(i)) * (kappa2(i)*mu(i)*omega(i)/(gammap(i)) + (1-mu(i))*omegap(i)/(gamma(i)+f(i)) + (kappa1(i)*(1-mu(i))*omegap(i)/omegapp(i)));
    q_NGM(i) = R0(i) / (max(eig(C)) * temp1);
end
fprintf('[R0, q_NGM] = \n');
disp([R0, q_NGM]);

for i = 1:numel(R0)
%     params.Beta   = C * q_DBM(i) ./ (N');
    params.Beta   = C * q_NGM(i) ./ (N');
end
%% 
Beta_28 = kron(params.Beta, ones(4))


