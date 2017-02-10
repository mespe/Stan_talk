/* Demo model of ag data
 * Matt Espe
 * UCD Data Science Initiative
 */

functions{
  //Simple scale function
  vector my_scale(vector y){
	vector[dims(y)[1]] ans;
	
	for(i in 1:dims(y)[1])
	  ans[i] = (y[i] - 9000) / 1000;

	return(ans);
  }
}

data{
  int N;
  int n_cult;
  int n_site;
  int n_year;
  int n_yr_site;
  int n_var;
  int<lower = 1, upper = n_cult> cult[N];
  int<lower = 1, upper = n_site> site[N];
  int<lower = 1, upper = n_year> year[N];
  int<lower = 1, upper = n_yr_site> siteyr[N];
  vector[N] yield;    

}
transformed data{
  vector[N] yield_scaled = my_scale(yield);
}

parameters{
  real alpha;
  vector[n_cult] b_cult_raw;
  vector[n_site] b_site_raw;
  vector[n_year] b_year_raw;
  vector[n_yr_site] b_siteyr_raw;
  vector<lower = 0>[n_var] tau;
  real<lower = 0> sigma;
}

transformed parameters{
  vector[n_cult] b_cult = b_cult_raw * tau[1];
  vector[n_site] b_site = b_site_raw * tau[2];
  vector[n_year] b_year = b_year_raw * tau[3];
  vector[n_yr_site] b_siteyr = b_siteyr_raw * tau[4];
  vector[N] mu = alpha +
	b_cult[cult] +
	b_site[site] +
	b_year[year] +
	b_siteyr[siteyr];

}

model{
  
  //Priors
  target += normal_lpdf(alpha | 0, 1);
  target += normal_lpdf(b_cult_raw | 0, 1);
  target += normal_lpdf(b_site_raw | 0, 1);
  target += normal_lpdf(b_year_raw | 0, 1);
  target += normal_lpdf(b_siteyr_raw | 0, 1);
  target += normal_lpdf(tau | 0, 1);
  target += normal_lpdf(sigma | 0, 1);
  
  // Likelihood
  target += student_t_lpdf(yield_scaled | 4, mu, sigma);

}

generated quantities{
  vector[N] log_lik;
  for(n in 1:N)
	log_lik[n] = student_t_lpdf(yield_scaled[n] | 5, mu[n], sigma);

}
