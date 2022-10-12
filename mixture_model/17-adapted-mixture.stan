// generated with brms 2.17.4
functions {
  /* compute correlated group-level effects
   * Args:
   *   z: matrix of unscaled group-level effects
   *   SD: vector of standard deviation parameters
   *   L: cholesky factor correlation matrix
   * Returns:
   *   matrix of scaled group-level effects
   */
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
  /* hurdle lognormal log-PDF of a single response
   * Args:
   *   y: the response value
   *   mu: mean parameter of the lognormal distribution
   *   sigma: sd parameter of the lognormal distribution
   *   hu: hurdle probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_lognormal_lpdf(real y, real mu, real sigma, real hu) {
    if (y == 0) {
      return bernoulli_lpmf(1 | hu);
    } else {
      return bernoulli_lpmf(0 | hu) + lognormal_lpdf(y | mu, sigma);
    }
  }
  /* hurdle lognormal log-PDF of a single response
   * logit parameterization of the hurdle part
   * Args:
   *   y: the response value
   *   mu: mean parameter of the lognormal distribution
   *   sigma: sd parameter of the lognormal distribution
   *   hu: linear predictor for the hurdle part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real hurdle_lognormal_logit_lpdf(real y, real mu, real sigma, real hu) {
    if (y == 0) {
      return bernoulli_logit_lpmf(1 | hu);
    } else {
      return bernoulli_logit_lpmf(0 | hu) + lognormal_lpdf(y | mu, sigma);
    }
  }
  // hurdle lognormal log-CCDF and log-CDF functions
  real hurdle_lognormal_lccdf(real y, real mu, real sigma, real hu) {
    return bernoulli_lpmf(0 | hu) + lognormal_lccdf(y | mu, sigma);
  }
  real hurdle_lognormal_lcdf(real y, real mu, real sigma, real hu) {
    return log1m_exp(hurdle_lognormal_lccdf(y | mu, sigma, hu));
  }
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  vector<lower=0>[N] weights; // model weights
  int<lower=1> K_mu1; // number of population-level effects
  matrix[N, K_mu1] X_mu1; // population-level design matrix
  int<lower=1> K_hu1; // number of population-level effects
  matrix[N, K_hu1] X_hu1; // population-level design matrix
  int<lower=1> K_mu2; // number of population-level effects
  matrix[N, K_mu2] X_mu2; // population-level design matrix
  int<lower=1> K_hu2; // number of population-level effects
  matrix[N, K_hu2] X_hu2; // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1; // number of grouping levels
  int<lower=1> M_1; // number of coefficients per level
  array[N] int<lower=1> J_1; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_mu1_1;
  vector[N] Z_1_mu1_2;
  int<lower=1> NC_1; // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2; // number of grouping levels
  int<lower=1> M_2; // number of coefficients per level
  array[N] int<lower=1> J_2; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_mu1_1;
  vector[N] Z_2_mu1_2;
  int<lower=1> NC_2; // number of group-level correlations
  // data for group-level effects of ID 3
  int<lower=1> N_3; // number of grouping levels
  int<lower=1> M_3; // number of coefficients per level
  array[N] int<lower=1> J_3; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_mu2_1;
  vector[N] Z_3_mu2_2;
  int<lower=1> NC_3; // number of group-level correlations
  // data for group-level effects of ID 4
  int<lower=1> N_4; // number of grouping levels
  int<lower=1> M_4; // number of coefficients per level
  array[N] int<lower=1> J_4; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_4_mu2_1;
  vector[N] Z_4_mu2_2;
  int<lower=1> NC_4; // number of group-level correlations
  // data for group-level effects of ID 5
  int<lower=1> N_5; // number of grouping levels
  int<lower=1> M_5; // number of coefficients per level
  array[N] int<lower=1> J_5; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_5_hu1_1;
  vector[N] Z_5_hu1_2;
  int<lower=1> NC_5; // number of group-level correlations
  // data for group-level effects of ID 6
  int<lower=1> N_6; // number of grouping levels
  int<lower=1> M_6; // number of coefficients per level
  array[N] int<lower=1> J_6; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_6_hu1_1;
  vector[N] Z_6_hu1_2;
  int<lower=1> NC_6; // number of group-level correlations
  // data for group-level effects of ID 7
  int<lower=1> N_7; // number of grouping levels
  int<lower=1> M_7; // number of coefficients per level
  array[N] int<lower=1> J_7; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_7_hu2_1;
  vector[N] Z_7_hu2_2;
  int<lower=1> NC_7; // number of group-level correlations
  // data for group-level effects of ID 8
  int<lower=1> N_8; // number of grouping levels
  int<lower=1> M_8; // number of coefficients per level
  array[N] int<lower=1> J_8; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_8_hu2_1;
  vector[N] Z_8_hu2_2;
  int<lower=1> NC_8; // number of group-level correlations
  // data for group-level effects of ID 9
  int<lower=1> N_9; // number of grouping levels
  int<lower=1> M_9; // number of coefficients per level
  array[N] int<lower=1> J_9; // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_9_theta1_1;
  int prior_only; // should the likelihood be ignored?
}
transformed data {
  int Kc_mu1 = K_mu1 - 1;
  matrix[N, Kc_mu1] Xc_mu1; // centered version of X_mu1 without an intercept
  vector[Kc_mu1] means_X_mu1; // column means of X_mu1 before centering
  int Kc_hu1 = K_hu1 - 1;
  matrix[N, Kc_hu1] Xc_hu1; // centered version of X_hu1 without an intercept
  vector[Kc_hu1] means_X_hu1; // column means of X_hu1 before centering
  int Kc_mu2 = K_mu2 - 1;
  matrix[N, Kc_mu2] Xc_mu2; // centered version of X_mu2 without an intercept
  vector[Kc_mu2] means_X_mu2; // column means of X_mu2 before centering
  int Kc_hu2 = K_hu2 - 1;
  matrix[N, Kc_hu2] Xc_hu2; // centered version of X_hu2 without an intercept
  vector[Kc_hu2] means_X_hu2; // column means of X_hu2 before centering
  for (i in 2 : K_mu1) {
    means_X_mu1[i - 1] = mean(X_mu1[ : , i]);
    Xc_mu1[ : , i - 1] = X_mu1[ : , i] - means_X_mu1[i - 1];
  }
  for (i in 2 : K_hu1) {
    means_X_hu1[i - 1] = mean(X_hu1[ : , i]);
    Xc_hu1[ : , i - 1] = X_hu1[ : , i] - means_X_hu1[i - 1];
  }
  for (i in 2 : K_mu2) {
    means_X_mu2[i - 1] = mean(X_mu2[ : , i]);
    Xc_mu2[ : , i - 1] = X_mu2[ : , i] - means_X_mu2[i - 1];
  }
  for (i in 2 : K_hu2) {
    means_X_hu2[i - 1] = mean(X_hu2[ : , i]);
    Xc_hu2[ : , i - 1] = X_hu2[ : , i] - means_X_hu2[i - 1];
  }
}
parameters {
  vector[Kc_mu1] b_mu1; // population-level effects
  real<lower=0> sigma1; // dispersion parameter
  vector[Kc_hu1] b_hu1; // population-level effects
  // real Intercept_hu1; // temporary intercept for centered predictors
  vector[Kc_mu2] b_mu2; // population-level effects
  real<lower=0> sigma2; // dispersion parameter
  vector[Kc_hu2] b_hu2; // population-level effects
  // real Intercept_hu2; // temporary intercept for centered predictors
  real Intercept_theta1; // temporary intercept for centered predictors
  ordered[2] ordered_Intercept; // to identify mixtures
  ordered[2] ordered_hu_Intercept; // to identify mixtures
  vector<lower=0>[M_1] sd_1; // group-level standard deviations
  matrix[M_1, N_1] z_1; // standardized group-level effects
  cholesky_factor_corr[M_1] L_1; // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2; // group-level standard deviations
  matrix[M_2, N_2] z_2; // standardized group-level effects
  cholesky_factor_corr[M_2] L_2; // cholesky factor of correlation matrix
  vector<lower=0>[M_3] sd_3; // group-level standard deviations
  matrix[M_3, N_3] z_3; // standardized group-level effects
  cholesky_factor_corr[M_3] L_3; // cholesky factor of correlation matrix
  vector<lower=0>[M_4] sd_4; // group-level standard deviations
  matrix[M_4, N_4] z_4; // standardized group-level effects
  cholesky_factor_corr[M_4] L_4; // cholesky factor of correlation matrix
  vector<lower=0>[M_5] sd_5; // group-level standard deviations
  matrix[M_5, N_5] z_5; // standardized group-level effects
  cholesky_factor_corr[M_5] L_5; // cholesky factor of correlation matrix
  vector<lower=0>[M_6] sd_6; // group-level standard deviations
  matrix[M_6, N_6] z_6; // standardized group-level effects
  cholesky_factor_corr[M_6] L_6; // cholesky factor of correlation matrix
  vector<lower=0>[M_7] sd_7; // group-level standard deviations
  matrix[M_7, N_7] z_7; // standardized group-level effects
  cholesky_factor_corr[M_7] L_7; // cholesky factor of correlation matrix
  vector<lower=0>[M_8] sd_8; // group-level standard deviations
  matrix[M_8, N_8] z_8; // standardized group-level effects
  cholesky_factor_corr[M_8] L_8; // cholesky factor of correlation matrix
  vector<lower=0>[M_9] sd_9; // group-level standard deviations
  array[M_9] vector[N_9] z_9; // standardized group-level effects
}
transformed parameters {
  // identify mixtures via ordering of the intercepts
  real Intercept_mu1 = ordered_Intercept[1];
  real Intercept_mu2 = ordered_Intercept[2];
  real Intercept_hu1 = ordered_hu_Intercept[1];
  real Intercept_hu2 = ordered_hu_Intercept[2];
  matrix[N_1, M_1] r_1; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_mu1_1;
  vector[N_1] r_1_mu1_2;
  matrix[N_2, M_2] r_2; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_2] r_2_mu1_1;
  vector[N_2] r_2_mu1_2;
  matrix[N_3, M_3] r_3; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_3] r_3_mu2_1;
  vector[N_3] r_3_mu2_2;
  matrix[N_4, M_4] r_4; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_4] r_4_mu2_1;
  vector[N_4] r_4_mu2_2;
  matrix[N_5, M_5] r_5; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_5] r_5_hu1_1;
  vector[N_5] r_5_hu1_2;
  matrix[N_6, M_6] r_6; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_6] r_6_hu1_1;
  vector[N_6] r_6_hu1_2;
  matrix[N_7, M_7] r_7; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_7] r_7_hu2_1;
  vector[N_7] r_7_hu2_2;
  matrix[N_8, M_8] r_8; // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_8] r_8_hu2_1;
  vector[N_8] r_8_hu2_2;
  vector[N_9] r_9_theta1_1; // actual group-level effects
  real lprior = 0; // prior contributions to the log posterior
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_mu1_1 = r_1[ : , 1];
  r_1_mu1_2 = r_1[ : , 2];
  // compute actual group-level effects
  r_2 = scale_r_cor(z_2, sd_2, L_2);
  r_2_mu1_1 = r_2[ : , 1];
  r_2_mu1_2 = r_2[ : , 2];
  // compute actual group-level effects
  r_3 = scale_r_cor(z_3, sd_3, L_3);
  r_3_mu2_1 = r_3[ : , 1];
  r_3_mu2_2 = r_3[ : , 2];
  // compute actual group-level effects
  r_4 = scale_r_cor(z_4, sd_4, L_4);
  r_4_mu2_1 = r_4[ : , 1];
  r_4_mu2_2 = r_4[ : , 2];
  // compute actual group-level effects
  r_5 = scale_r_cor(z_5, sd_5, L_5);
  r_5_hu1_1 = r_5[ : , 1];
  r_5_hu1_2 = r_5[ : , 2];
  // compute actual group-level effects
  r_6 = scale_r_cor(z_6, sd_6, L_6);
  r_6_hu1_1 = r_6[ : , 1];
  r_6_hu1_2 = r_6[ : , 2];
  // compute actual group-level effects
  r_7 = scale_r_cor(z_7, sd_7, L_7);
  r_7_hu2_1 = r_7[ : , 1];
  r_7_hu2_2 = r_7[ : , 2];
  // compute actual group-level effects
  r_8 = scale_r_cor(z_8, sd_8, L_8);
  r_8_hu2_1 = r_8[ : , 1];
  r_8_hu2_2 = r_8[ : , 2];
  r_9_theta1_1 = sd_9[1] * z_9[1];
  lprior += normal_lpdf(b_mu1 | 0, 1);
  lprior += normal_lpdf(Intercept_mu1 | 5, 0.5);
  lprior += normal_lpdf(sigma1 | 0, 1) - 1 * normal_lccdf(0 | 0, 1);
  lprior += normal_lpdf(b_hu1 | 0, 1);
  lprior += normal_lpdf(Intercept_hu1 | -0.5, 1);
  lprior += normal_lpdf(b_mu2 | 0, 1);
  lprior += normal_lpdf(Intercept_mu2 | 7.5, 0.2);
  lprior += normal_lpdf(sigma2 | 0, 1) - 1 * normal_lccdf(0 | 0, 1);
  lprior += normal_lpdf(b_hu2 | 0, 1);
  lprior += normal_lpdf(Intercept_hu2 | 0.5, 1);
  lprior += normal_lpdf(Intercept_theta1 | 0, 1);
  lprior += normal_lpdf(sd_1 | 0, 1) - 2 * normal_lccdf(0 | 0, 1);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 4);
  lprior += normal_lpdf(sd_2 | 0, 1) - 2 * normal_lccdf(0 | 0, 1);
  lprior += lkj_corr_cholesky_lpdf(L_2 | 4);
  lprior += normal_lpdf(sd_3 | 0, 1) - 2 * normal_lccdf(0 | 0, 1);
  lprior += lkj_corr_cholesky_lpdf(L_3 | 4);
  lprior += normal_lpdf(sd_4 | 0, 1) - 2 * normal_lccdf(0 | 0, 1);
  lprior += lkj_corr_cholesky_lpdf(L_4 | 4);
  lprior += normal_lpdf(sd_5 | 0, 1) - 2 * normal_lccdf(0 | 0, 1);
  lprior += lkj_corr_cholesky_lpdf(L_5 | 4);
  lprior += normal_lpdf(sd_6 | 0, 1) - 2 * normal_lccdf(0 | 0, 1);
  lprior += lkj_corr_cholesky_lpdf(L_6 | 4);
  lprior += normal_lpdf(sd_7 | 0, 1) - 2 * normal_lccdf(0 | 0, 1);
  lprior += lkj_corr_cholesky_lpdf(L_7 | 4);
  lprior += normal_lpdf(sd_8 | 0, 1) - 2 * normal_lccdf(0 | 0, 1);
  lprior += lkj_corr_cholesky_lpdf(L_8 | 4);
  lprior += normal_lpdf(sd_9 | 0, 1) - 1 * normal_lccdf(0 | 0, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu1 = Intercept_mu1 + Xc_mu1 * b_mu1;
    // initialize linear predictor term
    vector[N] hu1 = Intercept_hu1 + Xc_hu1 * b_hu1;
    // initialize linear predictor term
    vector[N] mu2 = Intercept_mu2 + Xc_mu2 * b_mu2;
    // initialize linear predictor term
    vector[N] hu2 = Intercept_hu2 + Xc_hu2 * b_hu2;
    // initialize linear predictor term
    vector[N] theta1 = Intercept_theta1 + rep_vector(0.0, N);
    vector[N] theta2 = rep_vector(0.0, N);
    real log_sum_exp_theta;
    for (n in 1 : N) {
      // add more terms to the linear predictor
      mu1[n] += r_1_mu1_1[J_1[n]] * Z_1_mu1_1[n]
                + r_1_mu1_2[J_1[n]] * Z_1_mu1_2[n]
                + r_2_mu1_1[J_2[n]] * Z_2_mu1_1[n]
                + r_2_mu1_2[J_2[n]] * Z_2_mu1_2[n];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      hu1[n] += r_5_hu1_1[J_5[n]] * Z_5_hu1_1[n]
                + r_5_hu1_2[J_5[n]] * Z_5_hu1_2[n]
                + r_6_hu1_1[J_6[n]] * Z_6_hu1_1[n]
                + r_6_hu1_2[J_6[n]] * Z_6_hu1_2[n];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      mu2[n] += r_3_mu2_1[J_3[n]] * Z_3_mu2_1[n]
                + r_3_mu2_2[J_3[n]] * Z_3_mu2_2[n]
                + r_4_mu2_1[J_4[n]] * Z_4_mu2_1[n]
                + r_4_mu2_2[J_4[n]] * Z_4_mu2_2[n];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      hu2[n] += r_7_hu2_1[J_7[n]] * Z_7_hu2_1[n]
                + r_7_hu2_2[J_7[n]] * Z_7_hu2_2[n]
                + r_8_hu2_1[J_8[n]] * Z_8_hu2_1[n]
                + r_8_hu2_2[J_8[n]] * Z_8_hu2_2[n];
    }
    for (n in 1 : N) {
      // add more terms to the linear predictor
      theta1[n] += r_9_theta1_1[J_9[n]] * Z_9_theta1_1[n];
    }
    for (n in 1 : N) {
      // apply the inverse link function
      hu1[n] = inv_logit(hu1[n]);
    }
    for (n in 1 : N) {
      // apply the inverse link function
      hu2[n] = inv_logit(hu2[n]);
    }
    for (n in 1 : N) {
      // scale theta to become a probability vector
      log_sum_exp_theta = log(exp(theta1[n]) + exp(theta2[n]));
      theta1[n] = theta1[n] - log_sum_exp_theta;
      theta2[n] = theta2[n] - log_sum_exp_theta;
    }
    // likelihood of the mixture model
    for (n in 1 : N) {
      array[2] real ps;
      ps[1] = theta1[n]
              + hurdle_lognormal_lpdf(Y[n] | mu1[n], sigma1, hu1[n]);
      ps[2] = theta2[n]
              + hurdle_lognormal_lpdf(Y[n] | mu2[n], sigma2, hu2[n]);
      target += weights[n] * log_sum_exp(ps);
    }
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
  target += std_normal_lpdf(to_vector(z_2));
  target += std_normal_lpdf(to_vector(z_3));
  target += std_normal_lpdf(to_vector(z_4));
  target += std_normal_lpdf(to_vector(z_5));
  target += std_normal_lpdf(to_vector(z_6));
  target += std_normal_lpdf(to_vector(z_7));
  target += std_normal_lpdf(to_vector(z_8));
  target += std_normal_lpdf(z_9[1]);
}
generated quantities {
  // actual population-level intercept
  real b_mu1_Intercept = Intercept_mu1 - dot_product(means_X_mu1, b_mu1);
  // actual population-level intercept
  real b_hu1_Intercept = Intercept_hu1 - dot_product(means_X_hu1, b_hu1);
  // actual population-level intercept
  real b_mu2_Intercept = Intercept_mu2 - dot_product(means_X_mu2, b_mu2);
  // actual population-level intercept
  real b_hu2_Intercept = Intercept_hu2 - dot_product(means_X_hu2, b_hu2);
  // actual population-level intercept
  real b_theta1_Intercept = Intercept_theta1;
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1, upper=1>[NC_1] cor_1;
  // compute group-level correlations
  corr_matrix[M_2] Cor_2 = multiply_lower_tri_self_transpose(L_2);
  vector<lower=-1, upper=1>[NC_2] cor_2;
  // compute group-level correlations
  corr_matrix[M_3] Cor_3 = multiply_lower_tri_self_transpose(L_3);
  vector<lower=-1, upper=1>[NC_3] cor_3;
  // compute group-level correlations
  corr_matrix[M_4] Cor_4 = multiply_lower_tri_self_transpose(L_4);
  vector<lower=-1, upper=1>[NC_4] cor_4;
  // compute group-level correlations
  corr_matrix[M_5] Cor_5 = multiply_lower_tri_self_transpose(L_5);
  vector<lower=-1, upper=1>[NC_5] cor_5;
  // compute group-level correlations
  corr_matrix[M_6] Cor_6 = multiply_lower_tri_self_transpose(L_6);
  vector<lower=-1, upper=1>[NC_6] cor_6;
  // compute group-level correlations
  corr_matrix[M_7] Cor_7 = multiply_lower_tri_self_transpose(L_7);
  vector<lower=-1, upper=1>[NC_7] cor_7;
  // compute group-level correlations
  corr_matrix[M_8] Cor_8 = multiply_lower_tri_self_transpose(L_8);
  vector<lower=-1, upper=1>[NC_8] cor_8;
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_1) {
    for (j in 1 : (k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_2) {
    for (j in 1 : (k - 1)) {
      cor_2[choose(k - 1, 2) + j] = Cor_2[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_3) {
    for (j in 1 : (k - 1)) {
      cor_3[choose(k - 1, 2) + j] = Cor_3[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_4) {
    for (j in 1 : (k - 1)) {
      cor_4[choose(k - 1, 2) + j] = Cor_4[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_5) {
    for (j in 1 : (k - 1)) {
      cor_5[choose(k - 1, 2) + j] = Cor_5[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_6) {
    for (j in 1 : (k - 1)) {
      cor_6[choose(k - 1, 2) + j] = Cor_6[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_7) {
    for (j in 1 : (k - 1)) {
      cor_7[choose(k - 1, 2) + j] = Cor_7[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1 : M_8) {
    for (j in 1 : (k - 1)) {
      cor_8[choose(k - 1, 2) + j] = Cor_8[j, k];
    }
  }
}
