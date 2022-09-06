data {
  int P; // number of provinces
  int TT; // number of times (not including the prediction)
  int N; // number of data = P*T
  int K; // number of national-level coeffs
  int L; // number of province-level coeffs
  matrix[N,K] X; // data matrix of national effects
  matrix[N,L] Z; // data matrix of province effects
  matrix[P,K] X_pred;
  matrix[P,L] Z_pred;
  vector<lower=0, upper=1>[N] Y;
  vector[P] Alpha_init;
  matrix[P,TT+1] Pop_weight; // weight of each province (turnout rate in each election)
}

parameters {
  matrix[P-1,TT+1] alpha0;
  vector[K] beta;
  vector[L] gamma;
  vector[TT+1] delta;
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_gamma;
  real<lower=0> sigma_delta;
  real<lower=0> sigma_epsilon[TT];
  real<lower=0> sigma;
  vector[P] y_pred;
}

transformed parameters {
  // weighted mean to .5 constraint
  matrix[P,TT+1] alpha;
  alpha[1:(P-1),] = alpha0;
  for(t in 1:(TT+1)){
    alpha[P,t] = (0.5-sum(Pop_weight[1:(P-1),t] .* alpha0[,t]))/Pop_weight[P,t];
  }
}

model {
  for(t in 1:(TT+1)){
    if(t==1){
      alpha[,1] ~ normal(Alpha_init,sigma_alpha);
    }else{
      alpha[,t] ~ normal(alpha[,t-1],sigma_alpha);
    } 
  }
  for(t in 1:TT){
    Y[(1+(t-1)*P):(t*P)] ~ normal(alpha[,t] + 
      (X * beta)[(1+(t-1)*P):(t*P)] + 
      (Z * gamma)[(1+(t-1)*P):(t*P)] + 
      delta[t], 
      sigma_epsilon[t]);
  }
  y_pred ~ normal(alpha[,TT+1] + 
    X_pred * beta + 
    Z_pred * gamma + 
    delta[TT+1], 
    sigma_epsilon[TT]);

  delta ~ normal(0, sigma_delta);
  // beta ~ normal(0, sigma_beta);
  // gamma ~ normal(0, sigma_gamma);
  beta ~ cauchy(0, sigma_beta);
  gamma ~ cauchy(0, sigma_gamma);

  sigma_alpha ~ normal(0, sigma);
  sigma_delta ~ normal(0, sigma);
  sigma_beta ~ normal(0, sigma);
  sigma_gamma ~ normal(0, sigma);
  sigma_epsilon ~ normal(0, sigma);
}

