functions {
// this function approximates a cut function that rises 
// from 0 to 1 at -delta to delta with slope 1/(2*delta) 
// and a "sharpness" given by beta.  1 (day) is a totally 
// fine beta in our case
real squishycut(real x, real delta, real beta){
  real x_squashed;
  x_squashed = (1 /(2 * delta*beta)) * 
    log( (1 + exp(beta*(x + delta))) / 
    (1 + exp(beta*(x - delta)))); 
  return x_squashed;
}

// this function steps up and down from y1 to y2 at x1 
// and back to y1 at x2 at (normalized) rate beta.
// Again - for our purposes, beta = 1 is fine. 
real squishytwostep(real x, real y1, real y2, real x1, real x2, real beta){
  real y_stepped;
  y_stepped = y1 + 2*(y2 - y1)/(x2 - x1) * (1 / (1 + exp(beta * (x - x2))) - 1 / (1 + exp(beta * (x - x1)))); 
  return y_stepped;
}
}

data {
int<lower=0> n;               // number of total observations
int k;                        // n. of individuals
real y[n];                    // x-coordinate
real x[n];                    // y-coordinate
real yday[n];                 // day of year
int<lower=1,upper=k> id[n];   // vector of id's 
real squish; 
} 

transformed data {
real deltat[n];
deltat[1] = 0; 
for(i in 2:n){
  if(id[i] == id[i-1]){
    deltat[i] = yday[i] - yday[i-1];
  } 
  else deltat[i] = 0;
}
}

parameters {
vector<lower = min(x), upper = max(x)>[2] mux_mean;
vector<lower = min(y), upper = max(y)>[2] muy_mean;
real<lower = min(x), upper = max(x)> mux1[k]; 
real<lower = min(y), upper = max(y)> muy1[k]; 
real<lower = min(x), upper = max(x)> mux2[k]; 
real<lower = min(y), upper = max(y)> muy2[k];

// migration timing parameters
real<lower = min(yday), upper = max(yday)> t_mean;
real<lower = 0> t_sd;
real<lower = min(yday), upper = max(yday)> t[k];  

real<lower = 0, upper = 100> dt_mean;
real<lower = 0> dt_sd;
real<lower = 0, upper = 100> dt[k];   

// components of variance-covariance matrix
real<lower = 0> mux1_sigma;
real<lower = 0> muy1_sigma;
real<lower=-1, upper=1> rho1;

real<lower = 0> mux2_sigma;
real<lower = 0> muy2_sigma;
real<lower=-1, upper=1> rho2;

real<lower = 0> sigma_ranging; 
real<lower = 0> sigma_migration; 
}

transformed parameters {
// population level covariance
cholesky_factor_cov[2] mu1_Sigma_chol;
cholesky_factor_cov[2] mu2_Sigma_chol;

real A;

// Cholesky decomposition of the bivariate normal ranging normals

mu1_Sigma_chol[1,1] = mux1_sigma;
mu1_Sigma_chol[1,2] = 0;
mu1_Sigma_chol[2,1] = muy1_sigma * rho1;
mu1_Sigma_chol[2,2] = muy1_sigma * sqrt(1- square(rho1));

mu2_Sigma_chol[1,1] = mux2_sigma;
mu2_Sigma_chol[1,2] = 0;
mu2_Sigma_chol[2,1] = muy2_sigma * rho2;
mu2_Sigma_chol[2,2] = muy2_sigma * sqrt(1- square(rho2));

A = -2 * sigma_ranging^2 * log(0.05)*pi();
}  

model {
// ancillary variables
vector[n] x_hat;
vector[n] y_hat;
vector[n] x_res;
vector[n] y_res;
vector[2] mu1_mean;
vector[2] mu2_mean;
// mean locations of each individual
vector[2] mu1[k];  
vector[2] mu2[k];  
real sigma_res;

t_mean ~ normal(mean(yday), 20.0)  T[min(yday),max(yday)];
t_sd ~ exponential(1/50.0);
dt_mean ~ normal(20.0, 20.0)  T[0,max(yday)];
dt_sd ~ exponential(1/50.0);

mu1_mean[1] = mux_mean[1];
mu1_mean[2] = muy_mean[1];
mu2_mean[1] = mux_mean[2];
mu2_mean[2] = muy_mean[2];

t ~ normal(t_mean, t_sd);
dt ~ normal(dt_mean, dt_sd);

for (i in 1:k){
  mu1[i][1] = mux1[i];
  mu1[i][2] = muy1[i];
  mu2[i][1] = mux2[i];
  mu2[i][2] = muy2[i];
  mu1[i] ~ multi_normal_cholesky(mu1_mean, mu1_Sigma_chol);
  mu2[i] ~ multi_normal_cholesky(mu2_mean, mu2_Sigma_chol);
}

x_res[1] = 0;
y_res[1] = 0;

for (i in 2:n){
  x_hat[i]  = mu1[id[i]][1] +
    (mu2[id[i]][1] - mu1[id[i]][1]) *
    squishycut(yday[i] - t[id[i]] - dt[id[i]]/2,
               dt[id[i]]/2, squish);
  
  y_hat[i]  = mu1[id[i]][2] +
    (mu2[id[i]][2] - mu1[id[i]][2]) *
    squishycut(yday[i] - t[id[i]] - dt[id[i]]/2,
               dt[id[i]]/2, squish);
  
  if(id[i] == id[i-1]){
    x_res[i] = x[i] - x_hat[i];
    y_res[i] = y[i] - y_hat[i];
    sigma_res = squishytwostep(yday[i], sigma_ranging, sigma_migration, t[id[i]], t[id[i]] + dt[id[i]], squish);
    x_res[i] ~ normal(0, sigma_res);
    y_res[i] ~ normal(0, sigma_res);
  }
  else {
    x_res[i] = 0;
    y_res[i] = 0;
  }
}
}	
