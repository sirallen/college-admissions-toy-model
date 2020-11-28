add_p_acceptance_beliefs <- function(colleges, type, alpha) {
  colleges %>%
    mutate(
      p_acceptance_belief =
        pnorm(qnorm(1 - cumsum(capacity) / sum(capacity), mean = 0, sd = sqrt(sd_types^2 + sd_e^2)),
              mean = type, sd = sd_e, lower.tail = FALSE),
      p_acceptance_belief_shrink = (1 - alpha) * p_acceptance_belief + alpha * public_acceptance_rate
    )
}

compute_choice_set_greedily <- function(p, u, c = app_cost) {
  # p : vector of p_acceptance_beliefs
  # u : vector of utilities
  # c : app_cost
  in_choice_set <- rep(FALSE, length(u))
  exp_gain <- p * u - c
  
  while (any(exp_gain > 0 & !in_choice_set)) {
    kprime <- which.max(exp_gain)
    in_choice_set[kprime] <- TRUE
    exp_gain <- compute_exp_utility_gain(in_choice_set, p, u, c)
  }
  
  return(in_choice_set)
}

compute_exp_utility_gain <- function(in_choice_set, p, u, c) {
  # p : vector of p_acceptance_beliefs
  # u : vector of utilities
  # c : app_cost
  # compute the expected utility gain of applying to another college (for each college)
  exp_gain <- rowSums(sapply(which(in_choice_set), function(k) {
    options_better_than_k <- which(in_choice_set & u > u[k])
    p_k_is_best_option <- p[k] * (if (length(options_better_than_k) > 0) prod(1 - p[options_better_than_k]) else 1)
    p_k_is_best_option * pmax(0, u - u[k])
  }))
  p_no_acceptance <- prod(1 - p[in_choice_set])
  exp_gain <- p * (p_no_acceptance * u + exp_gain) - c
  exp_gain[in_choice_set] <- 0
  return(exp_gain)
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
