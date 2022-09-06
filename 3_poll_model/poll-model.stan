functions {
  vector ilr(vector x) { // x: length 4 simplex vector
    vector[3] z;
    z[1] = sqrt(3.0/4.0)*log(x[1]/cbrt(x[2]*x[3]*x[4]));
    z[2] = sqrt(2.0/3.0)*log(x[2]/sqrt(x[3]*x[4]));
    z[3] = sqrt(1.0/2.0)*log(x[3]/x[4]);
    return z;
  }
  
  vector inv_ilr(vector z) { // z: length 3 real vector
    vector[4] x;
    x[1] = exp(sqrt(3.0/4.0)*z[1]);
    x[2] = exp(-z[1]/sqrt(12) + sqrt(2.0/3.0)*z[2]);
    x[3] = exp(-z[1]/sqrt(12) - z[2]/sqrt(6) + sqrt(1.0/2.0)*z[3]);
    x[4] = exp(-z[1]/sqrt(12) - z[2]/sqrt(6) - z[3]/sqrt(2));
    return x/sum(x);
  }
  
  vector simplex_add2(vector a, vector b) {
    return (a .* b)/sum(a .* b);
  }
  
  vector simplex_add3(vector a, vector b, vector c) {
    return (a .* b .* c)/sum(a .* b .* c);
  }
  
  vector simplex_add4(vector a, vector b, vector c, vector d) {
    return (a .* b .* c .* d)/sum(a .* b .* c .* d);
  }
  
  vector simplex_inv(vector x){ // length 4 simplex vector
    vector[4] x_inv;
    x_inv[1] = 1/x[1];
    x_inv[2] = 1/x[2];
    x_inv[3] = 1/x[3];
    x_inv[4] = 1/x[4];
    return x_inv;
  }
  
  vector simplex_closure(vector x){
    return x/sum(x);
  }
}

data {
  int I;    // Number of province cluster (=7)
  int J;    // Number of days
  int K;    // Number of polls
  int i[K]; // Province cluster index
  int j[K]; // Day index
  int w[J];
  int w_size;
  int wording[K];
  int method[K];

  int response[K,4];
  
  // prior distn using the fundamentals model
  vector[I] mean_prior;
  vector[I] sd_prior;
  
  vector[3] zero_vec;
}

parameters {
  vector[3] beta_star_temp[I,(w_size-1)];
  vector[3] delta_star[J-1];
  simplex[4] d_wording_temp[2];
  simplex[4] d_method_temp;
  
  vector<lower=0, upper=0.05>[I] sim;
  vector<lower=0, upper=0.15>[I] ahn;
  vector<lower=0, upper=1>[I] demo_pred;
  
  vector<lower=0>[3] sigma_beta_vec;
  vector<lower=0>[3] sigma_delta_vec;
  vector<lower=0>[3] sigma_wording;
  vector<lower=0>[3] sigma_method;
  
  real<lower=0> sigma;

}

transformed parameters {
  vector<lower=0, upper=1>[I] lee;
  vector<lower=0, upper=1>[I] yoon;
  simplex[4] prediction[I];
  vector[3] beta_star[I,w_size];
  simplex[4] d_wording[3];
  simplex[4] d_method[2];
  
  d_wording[1:2] = d_wording_temp;
  d_method[1] = d_method_temp;
  d_wording[3] = simplex_closure(simplex_inv(d_wording[1] .* d_wording[2]));
  d_method[2] = simplex_closure(simplex_inv(d_method[1]));

  for(ii in 1:I){
    lee[ii] = (1 - sim[ii] - ahn[ii]) * demo_pred[ii];
    yoon[ii] = (1 - sim[ii] - ahn[ii]) * (1-demo_pred[ii]);
    prediction[ii][1] = lee[ii];
    prediction[ii][2] = yoon[ii];
    prediction[ii][3] = sim[ii];
    prediction[ii][4] = ahn[ii];
  }
  
  beta_star[1:I,1:(w_size-1)] = beta_star_temp;
  for(ii in 1:I){
    beta_star[ii,w_size] = ilr(prediction[ii]);
  }
}

model {
  demo_pred ~ normal(mean_prior, sd_prior);

  sigma_beta_vec ~ normal(0, sigma);
  sigma_delta_vec ~ normal(0, sigma);
  
  for(ii in 1:I){
    beta_star[ii,w_size-1] ~ normal(ilr(prediction[ii]), sigma_beta_vec);
    for(jj in 1:(w_size-2)){
      beta_star[ii,w_size-jj-1] ~ normal(beta_star[ii,w_size-jj], sigma_beta_vec);
    }
  }
  
  ilr(d_wording[1]) ~ normal(0, sigma_wording);
  ilr(d_wording[2]) ~ normal(0, sigma_wording);
  ilr(d_wording[3]) ~ normal(0, sigma_wording);
  ilr(d_method[1]) ~ normal(0, sigma_method);
  ilr(d_method[2]) ~ normal(0, sigma_method);
  
  delta_star[J-1] ~ normal(zero_vec, sigma_delta_vec);
  for(jj in 1:(J-2)){
    delta_star[jj] ~ normal(delta_star[jj+1], sigma_delta_vec);
  }
  
  for(k in 1:K){
    response[k,] ~ multinomial( simplex_add3(inv_ilr(beta_star[i[k],w[j[k]]] + delta_star[j[k]]), d_wording[wording[k]], d_method[method[k]]) );
  }
}

generated quantities {
  simplex[4] pi[I,J];
  simplex[4] beta[I,w_size];
  simplex[4] delta[J-1];
  
  for(ii in 1:I){
    pi[ii,J] = inv_ilr(beta_star[ii,w[J]]);
    for(jj in 1:(J-1)){
      pi[ii,jj] = inv_ilr(beta_star[ii,w[jj]] + delta_star[jj]);
    }
    for(ww in 1:w_size){
      beta[ii,ww] = inv_ilr(beta_star[ii,ww]);
    }
  }
  for(jj in 1:(J-1)){
    delta[jj] = inv_ilr(delta_star[jj]);
  }
}
