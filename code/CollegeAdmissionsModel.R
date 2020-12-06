library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

theme_set(theme_minimal() +
            theme(panel.border = element_rect(color = 'black', fill = NA)))

source('CollegeAdmissionsModelFunctions.R')
#load('CollegeAdmissionsModelData.Rda')

K <- 50 # num colleges
N <- 100 * K # num students
sd_W <- 1
sd_e <- .1
app_cost <- 3
i_1 <- .05
i_K <- 1

# average acceptance rates at 4-year colleges, 2010-2019,
# for K = 50 bins of the cumulative share of freshman enrollment (corresponding
# to sum_{j<=k} A_j / sum_j A_j)
ipeds_acceptance_rates <-
  c(0.0981346762033072, 0.177450494710972, 0.250561840114117, 
    0.301199206924257, 0.338119667577706, 0.375391626592303, 0.405817193907439, 
    0.43404759543858, 0.455276760451289, 0.475884696234033, 0.492744998573495, 
    0.511481393396734, 0.528188912461492, 0.542461394108174, 0.557511864500808, 
    0.571243462138944, 0.584131178732808, 0.595807267589745, 0.60873621121331, 
    0.618557475664224, 0.630310072121656, 0.641280376827458, 0.650511147734797, 
    0.660444639943762, 0.669527098777772, 0.67914584978513, 0.690081418549776, 
    0.699152561422523, 0.708158344717021, 0.715789475533667, 0.724065630922742, 
    0.733560945924568, 0.74102157544647, 0.750590256504852, 0.759392490401858, 
    0.76802738096699, 0.776608284203821, 0.7859198432771, 0.795013830895565, 
    0.80524390533013, 0.815342630244326, 0.825509872595499, 0.835762605195074, 
    0.847573305479231, 0.862586765172119, 0.878458337309925, 0.897310732995593, 
    0.918915938402087, 0.944267958191291, 0.979141499152769)

colleges <- data_frame(
  college_rank = seq_len(K),
  capacity = rep(100, K),
  share_capacity = capacity / sum(capacity),
  cum_share_capacity = cumsum(share_capacity),
  #public_acceptance_rate = seq(i_1, i_K, length.out = K),
  public_acceptance_rate = ipeds_acceptance_rates)

colleges %>%
  select(college_rank, public_acceptance_rate) %>%
  crossing(beta = c(.1, .5, 1, 2)) %>%
  mutate(u = 1 / public_acceptance_rate^beta) %>% # common utility function
  group_by(beta) %>%
  mutate(u = u * 1000 / sum(public_acceptance_rate * u)) %>% # normalization
  ungroup() %>%
  ggplot(aes(x = college_rank, y = log(u), col = factor(beta))) +
    geom_line() +
    ggsave('plots/UtilityFunctions.png', dev = 'png', width = 8, height = 4, scale = 1.4)

set.seed(99)
students <- data_frame(
  i = seq_len(N),
  type = rnorm(N, mean = 0, sd = sd_W),
  type_signal = type + rnorm(N, mean = 0, sd = sd_e),
  ptype = pnorm(type, mean = 0, sd = sd_W),
  ptype_signal = pnorm(type_signal, mean = 0, sd = sqrt(sd_W^2 + sd_e^2)))

students %>%
  ggplot(aes(x = ptype, y = ptype_signal)) +
  geom_point(size = .2)

crossing(ptype = c(.05, .3, .7, .95),
         alpha = c(0, .1, .2, 1)) %>%
  mutate(type = qnorm(ptype, mean = 0, sd = sd_W),
         ptype = paste0('type_%ile=', 100 * ptype),
         ptype = factor(ptype, levels = unique(ptype))) %>%
  mutate(data = map2(.$type, .$alpha,
                     function(w, a) {
                       colleges %>%
                         add_p_acceptance_beliefs(type = w, alpha = a) %>%
                         select(college_rank,
                                p_acceptance_belief,
                                p_acceptance_belief_shrink)
                     })) %>%
  unnest(data) %>%
  ggplot(aes(x = college_rank, y = p_acceptance_belief_shrink, col = factor(alpha))) +
    geom_line() +
    geom_line(data = colleges, aes(y = public_acceptance_rate), lty = 2, col = 'black') +
    facet_wrap(~ ptype) +
    theme(panel.border = element_rect(color = 'black', fill = NA)) +
    ggsave('plots/ProbAcceptanceBeliefsByType.png', dev = 'png', width = 10, height = 5, scale = 1)


parameter_grid <- crossing(app_cost = 1:4,
                           alpha = c(0, .1, .2, 1),
                           beta = c(.1, .5, 1, 2, 3))

parameter_grid$choice_sets_matrix <- rep(list(NULL), nrow(parameter_grid))

for (j in seq_len(nrow(parameter_grid))) {
  cat(j, '..')
  c <- parameter_grid$app_cost[j]
  alpha <- parameter_grid$alpha[j]
  beta <- parameter_grid$beta[j]
  
  # note: this modifieds colleges in .GlobalEnv
  colleges <- colleges %>%
    mutate(u = 1 / public_acceptance_rate^beta) %>% # common utility function
    mutate(u = u * 1000 / sum(public_acceptance_rate * u)) # normalization
  
  all_applicants_choice_sets <-
    # a K x N matrix of applicants' choice sets
    sapply(seq_len(N), function(i) {
      type <- students$type[i]
      colleges %>%
        add_p_acceptance_beliefs(type = type, alpha = alpha) %>%
        mutate(in_choice_set = compute_choice_set_greedily(.$p_acceptance_belief_shrink, .$u, c = c)) %>%
        pull(in_choice_set)
    })
  
  parameter_grid$choice_sets_matrix[j] <- list(all_applicants_choice_sets)
}

rm(c, alpha, beta, j)
rm(all_applicants_choice_sets)

i <- 1

parameter_grid <- parameter_grid %>%
  mutate(summary = map(choice_sets_matrix, function(all_applicants_choice_sets) {
    cat(i, '..'); i <<- i + 1
    all_applicants_choice_sets_long <-
      compute_all_applicants_choice_sets_long_df(!is.na(all_applicants_choice_sets), students)
    
    compute_summaries(all_applicants_choice_sets_long)
  }))

parameter_grid <- parameter_grid %>%
  mutate(summary1 = map(summary, `[[`, 1),
         summary2 = map(summary, `[[`, 2),
         summary3 = map(summary, `[[`, 3))
  #select(-summary)

parameter_grid$gamma <- 0

lapply(unique(parameter_grid$gamma), function(g) {
  lapply(unique(parameter_grid$beta), function(b) {
    parameter_grid %>%
      select(-choice_sets_matrix, -summary, -summary2, -summary3) %>%
      unnest(summary1) %>%
      filter(beta == b, gamma == g) %>%
      mutate_at('alpha', ~ paste0('alpha=', .)) %>%
      mutate_at('app_cost', ~ paste0('c=', .)) %>%
      ggplot(aes(x = college_rank, y = type_percentile, fill = num_applications)) +
      geom_tile() +
      facet_grid(alpha ~ app_cost) +
      ggsave(paste0('plots/NumApplicationsByCollegeRankAndTypePercentile_',
                    'gamma=', g,
                    '_beta=', sprintf(b, fmt = '%#.2f'), '.png'),
             dev = 'png', width = 10, height = 10, scale = 1)
  })
})

lapply(unique(parameter_grid$gamma), function(g) {
  lapply(unique(parameter_grid$beta), function(b) {
    parameter_grid %>%
      select(-choice_sets_matrix, -summary, -summary2, -summary3) %>%
      unnest(summary1) %>%
      filter(beta == b, gamma == g) %>%
      mutate_at('alpha', ~ paste0('alpha=', .)) %>%
      mutate_at('app_cost', ~ paste0('c=', .)) %>%
      ggplot(aes(x = college_rank, y = type_percentile, fill = num_admits)) +
      geom_tile() +
      facet_grid(alpha ~ app_cost) +
      ggsave(paste0('plots/NumAdmitsByCollegeRankAndTypePercentile_',
                    'gamma=', g,
                    '_beta=', sprintf(b, fmt = '%#.2f'), '.png'),
             dev = 'png', width = 10, height = 10, scale = 1)
  })
})

lapply(unique(parameter_grid$gamma), function(g) {
  parameter_grid %>%
    select(-choice_sets_matrix, -summary, -summary1, -summary3) %>%
    unnest(summary2) %>%
    filter(gamma == g) %>%
    mutate_at('beta', ~ paste0('beta=', .)) %>%
    mutate_at('app_cost', ~ paste0('c=', .)) %>%
    ggplot(aes(x = type_percentile, y = avg_num_applications_per_student)) +
    geom_line(aes(col = factor(alpha))) +
    facet_grid(app_cost ~ beta) +
    theme(panel.border = element_rect(color = 'black', fill = NA)) +
    ggsave(paste0('plots/AvgNumApplicationsPerStudentByTypePercentile_gamma=', g, '.png'),
           dev = 'png', width = 10, height = 10, scale = 1)
})

lapply(unique(parameter_grid$gamma), function(g) {
  parameter_grid %>%
    select(-choice_sets_matrix, -summary, -summary1, -summary2) %>%
    unnest(summary3) %>%
    filter(gamma == g) %>%
    mutate_at('beta', ~ paste0('beta=', .)) %>%
    mutate_at('app_cost', ~ paste0('c=', .)) %>%
    ggplot(aes(x = college_rank)) +
    geom_line(aes(y = admit_rate, lty = 'admit_rate', col = factor(alpha))) +
    geom_line(data = colleges, aes(y = public_acceptance_rate, lty = 'public_acceptance_rate'), col = 'red') +
    facet_grid(app_cost ~ beta) +
    guides(linetype = FALSE) +
    theme(panel.border = element_rect(color = 'black', fill = NA)) +
    ggsave(paste0('plots/AdmitRatesByCollegeRank_gamma=', g, '.png'),
           dev = 'png', width = 10, height = 10, scale = 1)
})

lapply(unique(parameter_grid$gamma), function(g) {
  parameter_grid %>%
    select(-choice_sets_matrix, -summary, -summary1, -summary2) %>%
    unnest(summary3) %>%
    filter(gamma == g) %>%
    mutate_at('beta', ~ paste0('beta=', .)) %>%
    mutate_at('app_cost', ~ paste0('c=', .)) %>%
    ggplot(aes(x = college_rank)) +
    geom_line(aes(y = yield, col = factor(alpha))) +
    facet_grid(app_cost ~ beta) +
    theme(panel.border = element_rect(color = 'black', fill = NA)) +
    ggsave(paste0('plots/YieldsByCollegeRank_gamma=', g, '.png'),
           dev = 'png', width = 10, height = 10, scale = 1)
})

lapply(unique(parameter_grid$gamma), function(g) {
  parameter_grid %>%
    select(-choice_sets_matrix, -summary, -summary1, -summary2) %>%
    unnest(summary3) %>%
    filter(gamma == g) %>%
    mutate_at('beta', ~ paste0('beta=', .)) %>%
    mutate_at('app_cost', ~ paste0('c=', .)) %>%
    ggplot(aes(x = college_rank, y = num_applications)) +
    geom_line(aes(col = factor(alpha))) +
    facet_grid(app_cost ~ beta) +
    theme(panel.border = element_rect(color = 'black', fill = NA)) +
    ggsave(paste0('plots/ApplicationVolumeByCollegeRank_gamma=', g, '.png'),
           dev = 'png', width = 10, height = 10, scale = 1)
})

# expected utility of adding first college to C_i
colleges %>%
  select(college_rank, capacity, cum_share_capacity, public_acceptance_rate) %>%
  crossing(beta = c(0, .1, .5, 1, 2)) %>%
  mutate(u = 1 / public_acceptance_rate^beta) %>%
  group_by(beta) %>%
  mutate(u = u * 1000 / sum(public_acceptance_rate * u)) %>%
  ungroup() %>%
  crossing(type = qnorm(seq(0, 1, length.out = 500), mean = 0, sd = sd_W)) %>%
  group_by(beta, type) %>%
  mutate(p_acceptance_belief =
           pnorm(qnorm(1 - cum_share_capacity, mean = 0, sd = sqrt(sd_W^2 + sd_e^2)),
                 mean = type, sd = sd_e, lower.tail = FALSE),
         exp_gain = pmax(0, p_acceptance_belief * u - app_cost)) %>%
  summarize(exp_gain = max(exp_gain)) %>%
  ggplot(aes(x = type, y = log(exp_gain + .01), col = factor(beta))) +
    geom_line()

as.data.frame(compute_km_kmprime_matrix(parameter_grid$choice_sets_matrix[[5]], 1, 2)) %>%
  count(k_m, k_mprime) %>%
  ggplot(aes(x = k_m, y = k_mprime, fill = n)) +
  geom_tile() +
  geom_abline(slope = 1, intercept = 0, col = 'red') +
  coord_flip() +
  xlim(c(0, K + 1)) +
  ylim(c(0, K + 1))


# crossing(type = qnorm(seq(0, 1, length.out = 1000), mean = 0, sd = sd_W),
#          alpha = c(0, .3, .6, .9)) %>%
#   mutate(first_addition_to_choice_set =
#            map2_int(.$type, .$alpha, function(W, alpha) {
#              utilities <- colleges$u
#              p_acceptance_beliefs <-
#                add_p_acceptance_beliefs(colleges, type = W, alpha = alpha)$p_acceptance_belief_shrink
#              exp_gain <- utilities * p_acceptance_beliefs - app_cost
#              kprime <- which.max(exp_gain)
#              kprime * (exp_gain[kprime] > 0)
#            })) %>%
#   group_by(alpha) %>%
#   mutate(i = cumsum(coalesce(first_addition_to_choice_set != lag(first_addition_to_choice_set), TRUE))) %>%
#   group_by(alpha, i, first_addition_to_choice_set) %>%
#   summarize(minW = first(type),
#             maxW = last(type),
#             n = n()) %>%
#   ggplot(aes(x = pnorm(minW, mean = 0, sd = sd_W),
#              xend = pnorm(maxW, mean = 0, sd = sd_W),
#              y = first_addition_to_choice_set - .8 * alpha,
#              yend = first_addition_to_choice_set - .8 * alpha,
#              group = interaction(alpha, first_addition_to_choice_set),
#              col = factor(alpha))) +
#     geom_segment() +
#     scale_y_reverse()

# same thing, but better computation method
colleges <- colleges %>%
  mutate(G_inv = qnorm(1 - cum_share_capacity, mean = 0, sd = sqrt(sd_W^2 + sd_e^2)),
         G_inv_next_best = lead(G_inv),
         u_next_best = lead(u)) %>%
  mutate(W_cutoff_next_best_optim = pmap(list(G_inv, G_inv_next_best, u, u_next_best), compute_W_cutoff),
         W_cutoff_c_optim = pmap(list(G_inv, u), compute_W_cutoff_cost, c = app_cost)) %>%
  mutate(W_cutoff_next_best_obj = map_dbl(.$W_cutoff_next_best_optim, `[[`, 'objective'),
         W_cutoff_next_best_val = map_dbl(.$W_cutoff_next_best_optim, `[[`, 'minimum'),
         W_cutoff_c_obj = map_dbl(.$W_cutoff_c_optim, `[[`, 'objective'),
         W_cutoff_c_val = map_dbl(.$W_cutoff_c_optim, `[[`, 'minimum')) %>%
  select(-W_cutoff_next_best_optim, -W_cutoff_c_optim)

colleges %>%
  mutate(i = W_cutoff_c_val < W_cutoff_next_best_val,
         W_cutoff_val = pmax(W_cutoff_next_best_val, W_cutoff_c_val),
         W_cutoff_val = if_else(W_cutoff_c_val > coalesce(lag(W_cutoff_next_best_val), Inf), NA_real_, W_cutoff_val),
         W_cutoff_val_lag = if_else(is.na(W_cutoff_val), NA_real_, coalesce(lag(W_cutoff_val), Inf))) %>%
  ggplot(aes(x = pnorm(W_cutoff_val_lag, mean = 0, sd = sd_W),
             xend = pnorm(W_cutoff_val, mean = 0, sd = sd_W),
             y = college_rank,
             yend = college_rank)) +
  geom_segment() +
  xlim(c(0, 1)) +
  scale_y_reverse()


parameter_grid <- parameter_grid %>%
  mutate(exp_num_unique_choice_sets =
           map(.$choice_sets_matrix, compute_exp_number_of_unique_choice_sets))

parameter_grid %>%
  select(app_cost, alpha, beta, exp_num_unique_choice_sets) %>%
  unnest() %>%
  filter(n == 1 | n %% 100 == 0) %>%
  mutate_at('beta', ~ paste0('beta=', .)) %>%
  mutate_at('app_cost', ~ paste0('c=', .)) %>%
  ggplot(aes(x = n, y = exp_num_unique_C_i, col = factor(alpha))) +
  geom_line() +
  facet_grid(app_cost ~ beta) +
  ggsave('plots/NumDistinctChoiceSets.png',
         dev = 'png', width = 10, height = 10, scale = 1)


# OPTIMIZATION

sq_loss_obj_fn_on_2d_grid_all_k <- lapply(2:K, function(k) {
  cat(k, '..')
  compute_sq_loss_obj_fn_on_2d_grid(k, colleges = colleges) %>%
    mutate(k = k)
}) %>%
  bind_rows()

lapply(2:K, function(k) {
  cat(k, '..')
  sq_loss_obj_fn_on_2d_grid_all_k %>%
    filter(k == !! k) %>%
    ggplot(aes(x = pw_k_upper, y = pw_k_lower, fill = log_obj_fn)) +
    geom_tile() +
    geom_point(data = sq_loss_obj_fn_on_2d_grid_all_k %>%
                 filter(k == !! k) %>%
                 filter(log_obj_fn == min(log_obj_fn)), col = 'red') +
    xlim(c(0, 1 + 5e-3)) +
    ylim(c(0, 1 + 5e-3)) +
    ggsave(paste0('plots/optimization/Optimization_k=', k, '.png'),
           dev = 'png', width = 7, height = 6, scale = 1)
})

# Note: the solution set is a function (w_k^-, w_k^+(w_k^-)).
# Need to figure out how to choose one of these points
# min_loss_W_k <- sq_loss_obj_fn_on_2d_grid_all_k %>%
#   group_by(k) %>%
#   arrange(log_obj_fn) %>%
#   slice(1) %>%
#   ungroup() %>%
#   bind_rows(data_frame(k = 1, pw_k_upper = 1,
#                        pw_k_lower = 1 - colleges$share_capacity[1] / colleges$public_acceptance_rate[1],
#                        w_k_upper = Inf, w_k_lower = qnorm(pw_k_lower, mean = 0, sd = sd_W),
#                        hat_I_k = colleges$public_acceptance_rate[1], log_obj_fn = NA_real_,
#                        already_matched = 0,
#                        norm_W_k = pw_k_upper - pw_k_lower)) %>%
#   arrange(k)
# 
# min_loss_W_k %>%
#   ggplot(aes(x = pw_k_lower, xend = pw_k_upper,
#              y = k, yend = k)) +
#   geom_segment() +
#   scale_y_reverse() +
#   labs(x = 'W_k', y = 'college_rank')
# 
# min_loss_W_k %>%
#   mutate(I_k = colleges$public_acceptance_rate[.$k]) %>%
#   ggplot(aes(x = I_k, y = hat_I_k)) +
#   geom_abline(slope = 1, intercept = 0, lty = 3) +
#   geom_point() +
#   xlim(c(0, 1)) +
#   ylim(c(0, 1))

# compute actual admit rates using simulation procedure
students_choice_sets <- min_loss_W_k %>%
  select(k, pw_k_upper, pw_k_lower) %>%
  crossing(students) %>%
  mutate(in_choice_set = ptype < pw_k_upper & ptype > pw_k_lower) %>%
  select(k, i, in_choice_set) %>%
  spread(i, in_choice_set) %>%
  select(-k) %>%
  as.matrix() %>%
  rbind(matrix(FALSE, K - nrow(.), ncol(.)))

colnames(students_choice_sets) <- NULL

students_choice_sets %>%
  compute_all_applicants_choice_sets_long_df(students) %>%
  compute_summaries() %>%
  `[[`(3) %>%
  select(college_rank, admit_rate) %>%
  View()

