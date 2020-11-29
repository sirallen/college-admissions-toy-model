library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

theme_set(theme_minimal())

source('CollegeAdmissionsModelFunctions.R')
#load('CollegeAdmissionsModelData.Rda')

K <- 50           # num colleges
N <- 100 * K      # num students
sd_types <- 1     # sd(W_i)
sd_e <- .1        # sd(eps_i)
app_cost <- 3     # c
i_1 <- .05        # I_1
i_K <- 1          # I_K

colleges <- data_frame(
  college_rank = seq_len(K),
  capacity = rep(100, K),
  public_acceptance_rate = seq(i_1, i_K, length.out = K))

colleges %>%
  select(college_rank, public_acceptance_rate) %>%
  crossing(beta = seq(1, 2, .5),
           gamma = c(0, 3)) %>%
  mutate(u = 1 / public_acceptance_rate^beta + gamma * (n() - college_rank)) %>% # common utility function
  group_by(beta, gamma) %>%
  mutate(u = u * 1000 / sum(public_acceptance_rate * u)) %>% # normalization
  ungroup() %>%
  ggplot(aes(x = college_rank, y = log(u), col = factor(beta), lty = factor(gamma),
             group = interaction(beta, gamma))) +
    geom_line() +
    ggsave('plots/UtilityFunctions.png', dev = 'png', width = 8, height = 4, scale = 1.4)

set.seed(99)
students <- data_frame(
  type = rnorm(N, mean = 0, sd = sd_types),
  type_signal = type + rnorm(N, mean = 0, sd = sd_e),
  ptype = pnorm(type, mean = 0, sd = sd_types),
  ptype_signal = pnorm(type_signal, mean = 0, sd = sqrt(sd_types^2 + sd_e^2)))

students %>%
  ggplot(aes(x = ptype, y = ptype_signal)) +
  geom_point(size = .2)

crossing(ptype = c(.05, .3, .7, .95),
         alpha = c(0, .1, .2, 1)) %>%
  mutate(type = qnorm(ptype, mean = 0, sd = sd_types),
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
                           beta = seq(1, 2, .5),
                           gamma = c(0, 3))

parameter_grid$data <- rep(list(NULL), nrow(parameter_grid))

for (j in seq_len(nrow(parameter_grid))) {
  cat(j, '..')
  c <- parameter_grid$app_cost[j]
  alpha <- parameter_grid$alpha[j]
  beta <- parameter_grid$beta[j]
  gamma <- parameter_grid$gamma[j]
  
  colleges <- colleges %>%
    mutate(u = 1 / public_acceptance_rate^beta + gamma * (n() - college_rank)) %>% # common utility function
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
  
  parameter_grid$data[j] <- list(all_applicants_choice_sets)
}

rm(c, alpha, beta, gamma, j)
rm(all_applicants_choice_sets)

i <- 1

parameter_grid <- parameter_grid %>%
  mutate(summary = map(data, function(all_applicants_choice_sets) {
    cat(i, '..'); i <<- i + 1
    all_applicants_choice_sets_long <-
      compute_all_applicants_choice_sets_long_df(all_applicants_choice_sets, students)
    
    compute_summaries(all_applicants_choice_sets_long)
  }))

parameter_grid <- parameter_grid %>%
  mutate(summary1 = map(summary, `[[`, 1),
         summary2 = map(summary, `[[`, 2),
         summary3 = map(summary, `[[`, 3))
  #select(-summary)

lapply(unique(parameter_grid$gamma), function(g) {
  lapply(unique(parameter_grid$beta), function(b) {
    parameter_grid %>%
      select(-data, -summary, -summary2, -summary3) %>%
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
      select(-data, -summary, -summary2, -summary3) %>%
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
    select(-data, -summary, -summary1, -summary3) %>%
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
    select(-data, -summary, -summary1, -summary2) %>%
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
    select(-data, -summary, -summary1, -summary2) %>%
    unnest(summary3) %>%
    filter(gamma == g) %>%
    mutate_at('beta', ~ paste0('beta=', .)) %>%
    mutate_at('app_cost', ~ paste0('c=', .)) %>%
    ggplot(aes(x = college_rank)) +
    geom_line(aes(y = yield, lty = 'yield', col = factor(alpha))) +
    geom_line(data = colleges, aes(y = public_acceptance_rate, lty = 'public_acceptance_rate'), col = 'red') +
    facet_grid(app_cost ~ beta) +
    scale_linetype_manual(values = c(yield = 1, public_acceptance_rate = 3)) +
    guides(linetype = FALSE) +
    theme(panel.border = element_rect(color = 'black', fill = NA)) +
    ggsave(paste0('plots/YieldsByCollegeRank_gamma=', g, '.png'),
           dev = 'png', width = 10, height = 10, scale = 1)
})

lapply(unique(parameter_grid$gamma), function(g) {
  parameter_grid %>%
    select(-data, -summary, -summary1, -summary2) %>%
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

