// Risk assessment model
//
// In this code, variables in capital letters are input variables
// a subscript '_O' indicates observed, a subscript '_T' indicates total

data{
  // Dimensions
  int<lower=1>  N_SPECIES;
  int<lower=1>  N_ROW_O;
  int<lower=1>  N_ROW_T;
  int<lower=1>  N_SPECIES_VUL_GROUP;
  int<lower=1>  N_INTERACTION_CAT;
  int<lower=1>  N_FISHERY_GROUP;
  int<lower=1>  SPECIES_O [N_ROW_O];
  int<lower=1>  SPECIES_T [N_ROW_T];
  int<lower=1>  SPECIES_GROUP_O [N_ROW_O];
  int<lower=1>  SPECIES_GROUP_T [N_ROW_T];
  real<lower=0> OVERLAP_T [N_ROW_T];
  int <lower=0> LIVE_CAPTURES_O [N_ROW_O];
  int <lower=0> DEAD_CAPTURES_O [N_ROW_O];
  // Prior for the catchabilities
  real          BETA0;
  // Prior for the probability a capture is observed
  real          POBS_BETA_ALPHA;
  real          POBS_BETA_BETA;
  vector<lower=0> [N_ROW_O] OVERLAP_O;
  

  // There is some index arithmetic going on here
  // the 'DUMMY' variables, would like to refactor to reduce
  // the need for these transformations
  int<lower=1> LEVEL_G [N_SPECIES_VUL_GROUP];
  int<lower=1> LEVEL_F [N_FISHERY_GROUP];
    
  matrix [N_ROW_O, N_SPECIES_VUL_GROUP] DUMMYVAR_SPECIES_GROUP_O;
  matrix [N_ROW_O, N_FISHERY_GROUP]     DUMMYVAR_FISHERY_GROUP_O;
  matrix [N_ROW_O, N_INTERACTION_CAT]   DUMMYVAR_INTERACTION_O;
  int<lower=1>  FISHERY_GROUP_O [N_ROW_O];
  int<lower=1>  FISHERY_GROUP_T [N_ROW_T];
  
  // Total population
  real NTOT_LOG_MEAN [N_SPECIES];
  real NTOT_LOG_SD   [N_SPECIES];

  // Unidentifed captures
  int<lower=0>  DEAD_UNIDENT_O [N_FISHERY_GROUP];
  int<lower=0>  LIVE_UNIDENT_O [N_FISHERY_GROUP];
  matrix [N_ROW_T, N_SPECIES_VUL_GROUP] DUMMYVAR_SPECIES_GROUP_T;
  matrix [N_ROW_T, N_FISHERY_GROUP]     DUMMYVAR_FISHERY_GROUP_T;
  matrix [N_ROW_T, N_INTERACTION_CAT]   DUMMYVAR_INTERACTION_T;
  int<lower=1>  START_F_O [N_FISHERY_GROUP];
  int<lower=1>  END_F_O   [N_FISHERY_GROUP];
}


parameters{
  real<lower=0, upper=1>                          p_observable;
  vector<lower=0, upper=1>[N_FISHERY_GROUP]       p_live_cap;
  vector<lower=0, upper=1>[N_SPECIES_VUL_GROUP]   p_survive_cap;
  vector<lower=0>  [N_SPECIES]                    ntot;
  real<lower=0>                                   q0;
  vector<lower=0>         [N_SPECIES_VUL_GROUP-1] q_g0_s;
  vector<lower=0>         [N_FISHERY_GROUP-1]     q_f0_s;
  real<lower=0>                                   sigma;
  vector<lower=0>         [N_INTERACTION_CAT]     q_gf0;
  real<lower=0, upper=1>                          p_identified;
}


transformed parameters{
  vector<lower=0> [N_SPECIES_VUL_GROUP] q_g0;
  vector<lower=0> [N_FISHERY_GROUP] q_f0;
  vector [N_ROW_O] q_g_o;
  vector [N_ROW_O] q_f_o;
  vector [N_ROW_O] q_gf_o;
  vector [N_ROW_O] mu_observable_live_incidents_o;
  vector [N_ROW_O] mu_observable_dead_incidents_o;
  vector [N_ROW_O] q_o;
  vector [N_ROW_O] mu_observable_incidents_o;
  real mu_unident_o[N_FISHERY_GROUP];
  vector<lower=1E-12> [N_ROW_O] overlap_o_bounded;

  q_g0[LEVEL_G[1]] = 1;
  q_g0[LEVEL_G[2:N_SPECIES_VUL_GROUP]] = q_g0_s;

  q_f0[LEVEL_F[1]] = 1;
  q_f0[LEVEL_F[2:N_FISHERY_GROUP]] = q_f0_s;

  // Arithmetic to pick base levels of the catchability
  q_g_o = DUMMYVAR_SPECIES_GROUP_O * q_g0;
  q_f_o = DUMMYVAR_FISHERY_GROUP_O * q_f0;
  q_gf_o = DUMMYVAR_INTERACTION_O * q_gf0;

  q_o = q0 * (q_g_o .* q_f_o .* q_gf_o);  //catchability

  // Keep the overlap away from zero. At the moment this is
  // set to an infinitesimal level
  for (i in 1:N_ROW_O){
    overlap_o_bounded[i] = fmax(1E-12, OVERLAP_O[i]);  
  }

  // Means
  mu_observable_incidents_o = (q_o .* overlap_o_bounded .* ntot[SPECIES_O]) * p_observable;
  mu_observable_live_incidents_o = (mu_observable_incidents_o .* p_live_cap[FISHERY_GROUP_O]) * p_identified;
  mu_observable_dead_incidents_o = (mu_observable_incidents_o .* (1 - p_live_cap[FISHERY_GROUP_O])) * p_identified;

  for (f in 1:N_FISHERY_GROUP) {
    mu_unident_o[f] = (1 - p_identified) * sum(mu_observable_incidents_o[START_F_O[f]:END_F_O[f]]);
  }

}

model{
  // Total population for each species
  for (s in 1:N_SPECIES) {
    ntot[s] ~ lognormal(NTOT_LOG_MEAN[s], NTOT_LOG_SD[s]);
  }
 
  p_observable  ~ beta(POBS_BETA_ALPHA, POBS_BETA_BETA);
  p_live_cap    ~ beta(1,1);
  p_survive_cap ~ beta(1,1);
  p_identified  ~ beta(1,1);

  // Vulnerability 
  // Intercept
  q0 ~ lognormal(0, BETA0);
  // Species group
  q_g0_s ~ lognormal(0, BETA0);
  // Fishery group
  q_f0_s ~ lognormal(0, BETA0);
  // Species group * fishery group (as random effect)
  sigma ~ student_t(4, 0, 10);
  sigma_prior ~ student_t(4, 0, 10);
  /* sigma ~ gamma(100, 100); */
  q_gf0 ~ lognormal(0, sigma);

  q_prior ~ lognormal(0, BETA0);
  q_gf_prior ~ lognormal(0, sigma_prior);
  
  
  for(i in 1:N_ROW_O){
    LIVE_CAPTURES_O[i] ~ poisson(mu_observable_live_incidents_o[i]);
    DEAD_CAPTURES_O[i] ~ poisson(mu_observable_dead_incidents_o[i]);
  }
    
  // Unidentified captures
  for (f in 1:N_FISHERY_GROUP) {
    DEAD_UNIDENT_O[f] ~ poisson((1 - p_live_cap[f]) * mu_unident_o[f]);
    LIVE_UNIDENT_O[f] ~ poisson(p_live_cap[f]  * mu_unident_o[f]);
  }



}

generated quantities{
  real  q_t[N_ROW_T];
  vector [N_ROW_T] q_g_t;
  vector [N_ROW_T] q_f_t;
  vector [N_ROW_T] q_gf_t;
  int   apf_t[N_ROW_T];
  int   incidents_t[N_ROW_T];
  int   observable_captures_t[N_ROW_T];
  int   observable_captures_ident_t[N_ROW_T];
  
  // Observed strata
  int <lower=0> live_captures_o [N_ROW_O];
  int <lower=0> dead_captures_o [N_ROW_O];
  int <lower=0> live_unident_o [N_FISHERY_GROUP];
  int <lower=0> dead_unident_o [N_FISHERY_GROUP];

  for(i in 1:N_ROW_O){
    live_captures_o[i] = poisson_rng(mu_observable_live_incidents_o[i]);
    dead_captures_o[i] = poisson_rng(mu_observable_dead_incidents_o[i]);
  }

  for (f in 1:N_FISHERY_GROUP) {
    live_unident_o[f] = poisson_rng(p_live_cap[f]  * mu_unident_o[f]);
    dead_unident_o[f] = poisson_rng((1 - p_live_cap[f]) * mu_unident_o[f]);
  }


  // All strata
  q_g_t  = DUMMYVAR_SPECIES_GROUP_T * q_g0;
  q_f_t  = DUMMYVAR_FISHERY_GROUP_T * q_f0;
  q_gf_t = DUMMYVAR_INTERACTION_T   * q_gf0;
  
  for(j in 1:N_ROW_T){
    q_t[j] = q0 * q_g_t[j] * q_f_t[j] * q_gf_t[j] * OVERLAP_T[j] * ntot[SPECIES_T[j]];  
    //Total incidents
    incidents_t[j] = poisson_rng(q_t[j]);
    // Total number of fatalities (observed + unobserved)
    apf_t[j] = binomial_rng(incidents_t[j], 1 - p_live_cap[FISHERY_GROUP_T[j]] * p_survive_cap[SPECIES_GROUP_T[j]]);

    // Observable captures
    observable_captures_t[j] = binomial_rng(incidents_t[j], p_observable);
    observable_captures_ident_t[j] = binomial_rng(observable_captures_t[j], p_identified);
    
  }

}
