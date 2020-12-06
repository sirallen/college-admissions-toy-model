add_p_acceptance_beliefs <- function(colleges, type, alpha) {
  colleges %>%
    mutate(
      p_acceptance_belief =
        pnorm(qnorm(1 - cum_share_capacity, mean = 0, sd = sqrt(sd_W^2 + sd_e^2)),
              mean = type, sd = sd_e, lower.tail = FALSE),
      p_acceptance_belief_shrink = (1 - alpha) * p_acceptance_belief + alpha * public_acceptance_rate
    )
}

compute_choice_set_greedily <- function(p, u, c = app_cost) {
  # p : vector of p_acceptance_beliefs
  # u : vector of utilities
  # c : app_cost
  in_choice_set <- rep(NA, length(u))
  exp_gain <- p * u - c
  m <- 1
  while (any(exp_gain > 0 & is.na(in_choice_set))) {
    kprime <- which.max(exp_gain)
    in_choice_set[kprime] <- m
    m <- m + 1
    exp_gain <- compute_exp_utility_gain(in_choice_set, p, u, c)
  }
  
  return(in_choice_set)
}

compute_exp_utility_gain <- function(in_choice_set, p, u, c) {
  # p : vector of p_acceptance_beliefs
  # u : vector of utilities
  # c : app_cost
  # compute the expected utility gain of applying to another college (for each college)
  exp_gain <- rowSums(sapply(which(!is.na(in_choice_set)), function(k) {
    options_better_than_k <- which(!is.na(in_choice_set) & u > u[k])
    p_k_is_best_option <- p[k] * (if (length(options_better_than_k) > 0) prod(1 - p[options_better_than_k]) else 1)
    p_k_is_best_option * pmax(0, u - u[k])
  }))
  p_no_acceptance <- prod(1 - p[!is.na(in_choice_set)])
  exp_gain <- p * (p_no_acceptance * u + exp_gain) - c
  exp_gain[!is.na(in_choice_set)] <- 0
  return(exp_gain)
}

compute_km_kmprime_matrix <- function(all_applicants_choice_sets, m = 1, mprime = 2) {
  # construct K x K matrix of applicants' mth addition to C_i vs m'-th addition
  all_applicants_num_choices <- apply(coalesce(all_applicants_choice_sets, 0), 2, max, na.rm = TRUE)
  in_sample <- which(all_applicants_num_choices >= max(m, mprime))
  in_sample_applicants_choice_sets <- all_applicants_choice_sets[, in_sample]
  km_kmprime_pairs <- apply(in_sample_applicants_choice_sets, 2,
                            function(C_i) {
                              km <- which(C_i == m)
                              kmprime <- which(C_i == mprime)
                              c('k_m' = km, 'k_mprime' = kmprime)
                            })
  return(t(km_kmprime_pairs))
}

compute_matches <- function(C, W) {
  # C : K x N matrix of application/choice sets
  # W : vector of student types
  oW <- order(W, decreasing = TRUE)
  #colnames(C) <- as.character(1:N)
  C <- C[, oW]
  admits <- matrix(rep(0, K, N), K, N)
  #colnames(admits) <- colnames(C)
  # assume most selective/desirable college gets first pick of applicants. It will admit applicants
  # with the highest type signals W and yield 100%. Then the second most selective college picks.
  # It will match with the highest-W unmatched applicants, but will admit anyone who ranks above
  # the lowest-W member of this set. And so on.
  for (k in seq_len(K)) {
    admits[k, head(which(C[k,] & colSums(admits) == 0), colleges$capacity[k])] <- 1
  }
  return(admits[, order(oW)])
}

compute_all_applicants_choice_sets_long_df <-
  function(all_applicants_choice_sets, students) {
    data_frame(student = rep(seq_len(N), each = K),
               ptype = rep(students$ptype, each = K),
               ptype_signal = rep(students$ptype_signal, each = K),
               college_rank = rep(seq_len(K), times = N),
               in_choice_set = as.vector(all_applicants_choice_sets),
               match = as.vector(compute_matches(all_applicants_choice_sets, students$ptype_signal))) %>%
      group_by(college_rank) %>%
      arrange(desc(ptype_signal)) %>%
      mutate(admit = in_choice_set & row_number() <= max(which(match == 1))) %>%
      ungroup() %>%
      mutate(type_percentile = floor(ptype * 100))
  }

compute_summaries <- function(all_applicants_choice_sets_long) {
  summary1 <- all_applicants_choice_sets_long %>%
    group_by(type_percentile, college_rank) %>%
    summarize(num_applications = sum(in_choice_set),
              num_admits = sum(admit)) %>%
    ungroup()
  
  summary2 <- all_applicants_choice_sets_long %>%
    group_by(type_percentile) %>%
    summarize(num_students = n_distinct(student),
              num_applications = sum(in_choice_set),
              avg_num_applications_per_student = num_applications / num_students)
  
  summary3 <- all_applicants_choice_sets_long %>%
    group_by(college_rank) %>%
    summarize(num_applications = sum(in_choice_set),
              num_admits = sum(admit),
              num_matches = sum(match),
              admit_rate = num_admits / num_applications,
              yield = num_matches / num_admits)
  
  list(summary1, summary2, summary3)
}

compute_W_cutoff <- function(G_inv, G_inv_next_best, u, u_next_best) {
  # find the W such that a prospect is indifferent between applying to k and k+1 (for first choice)
  # (assumes no shrinkage, alpha = 0)
  if (G_inv == -Inf) {
    return(list(minimum = -Inf, objective = NA))
  }
  lower_search_bound <- G_inv - 4 * sd_e
  upper_search_bound <- G_inv + 4 * sd_e
  optimize(f = function(W) {
    p_admit <- pnorm(G_inv - W, mean = 0, sd = sd_e, lower.tail = FALSE)
    p_admit_next_best <- pnorm(G_inv_next_best - W, mean = 0, sd = sd_e, lower.tail = FALSE)
    abs((p_admit / p_admit_next_best - u_next_best / u) ^ 2)
  },
  interval = c(lower_search_bound, upper_search_bound))
}

compute_W_cutoff_cost <- function(G_inv, u, c) {
  # find the W such that a prospect is indifferent between applying to k and not applying (for first choice)
  # (assumes no shrinkage, alpha = 0)
  if (u < c) {
    return(list(minimum = NA, objective = NA))
  } else if (G_inv == -Inf) {
    return(list(minimum = -Inf, objective = NA))
  }
  lower_search_bound <- G_inv - 4 * sd_e
  upper_search_bound <- G_inv + 4 * sd_e
  optimize(f = function(W) {
    p_admit <- pnorm(G_inv - W, mean = 0, sd = sd_e, lower.tail = FALSE)
    abs((p_admit * u - c) ^ 2)
  },
  interval = c(lower_search_bound, upper_search_bound))
}

compute_exp_number_of_unique_choice_sets <- function(all_applicants_choice_sets) {
  # !is.na(all_applicants_choice_sets) : K x N logical matrix, TRUE if college k is in C_i
  # map each C_i to a unique integer
  C_i_int <- apply(!is.na(all_applicants_choice_sets), 2, FUN = function(C_i) sum(2 ^ which(C_i)))
  # count fraction of times each C_i occurred
  C_i_int_tab <- table(C_i_int) / length(C_i_int)
  # compute expected number of distinct C_i for sample of size n = 1, ..., N
  # https://math.stackexchange.com/questions/3931475/
  data_frame(n = seq_along(C_i_int),
             exp_num_unique_C_i =
               sapply(seq_along(C_i_int), function(n) length(C_i_int_tab) - sum((1 - C_i_int_tab) ^ n)))
}

compute_sq_loss_obj_fn_on_2d_grid <- function(k, colleges) {
  # For a given k, compute the squared-loss objective function of the
  # equilibrium condition hat_I_k = I_k on a 2d grid of (w_k^-, w_k^+)
  stopifnot(k > 1 & k <= K)
  res <- crossing(pw_k_upper = seq(0, 1, length.out = 200),
                  pw_k_lower = seq(0, 1, length.out = 200)) %>%
    mutate(w_k_upper = qnorm(pw_k_upper, mean = 0, sd = sd_W),
           w_k_lower = qnorm(pw_k_lower, mean = 0, sd = sd_W)) %>%
    filter(w_k_upper > w_k_lower,
           w_k_upper > qnorm(1 - colleges$cum_share_capacity[k - 1] / colleges$public_acceptance_rate[k - 1],
                             mean = 0, sd = sd_W),
           pw_k_upper - pw_k_lower > colleges$share_capacity[k] / colleges$public_acceptance_rate[k],
           pw_k_upper - pw_k_lower < colleges$cum_share_capacity[k] / colleges$public_acceptance_rate[k]) %>%
    mutate(dat = map2(.$w_k_lower, .$w_k_upper,
                      function(l, u) {
                        a_k <- colleges$cum_share_capacity[k - 1]
                        Z_k <- qnorm(1 - a_k, mean = 0, sd = sqrt(sd_W^2 + sd_e^2))
                        norm_W_k <- diff(pnorm(c(l, u), mean = 0, sd = sd_W))
                        # normalization constant for integral
                        norm_integral <- integrate(f = function(wprime) dnorm(wprime, mean = 0, sd = sd_W) *
                                                     (1 - pnorm(Z_k - wprime, mean = 0, sd = sd_e)),
                                                   lower = -Inf, upper = Inf)$value
                        integral <- integrate(f = function(w) dnorm(w, mean = 0, sd = sd_W) ^ 2 *
                                                (1 - pnorm(Z_k - w, mean = 0, sd = sd_e)),
                                              lower = l, upper = u)$value
                        integral <- integral * a_k / norm_integral
                        hat_I_k <- (colleges$share_capacity[k] + integral) / norm_W_k
                        obj_fn <- (hat_I_k - colleges$public_acceptance_rate[k]) ^ 2
                        return(list(hat_I_k, obj_fn, integral, norm_W_k))
                      })) %>%
    mutate(hat_I_k = map_dbl(dat, `[[`, 1),
           log_obj_fn = log(map_dbl(dat, `[[`, 2)),
           already_matched = map_dbl(dat, `[[`, 3),
           norm_W_k = map_dbl(dat, `[[`, 4))
  
  return(res)
}
