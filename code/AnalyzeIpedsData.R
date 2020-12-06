library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

theme_set(theme_minimal() +
            theme(panel.border = element_rect(color = 'black', fill = NA)))

# download from https://nces.ed.gov/ipeds/datacenter/InstitutionList.aspx?goToReportId=1
# sample: EZGroup -> 2019
# Select -> U.S. only
# Special characteristics -> Institutional category -> Degree-granting, primarily baccalaureate or above
ipeds <- fread('data/Data_12-4-2020.csv') %>%
  as_tibble()

ipedsReshape <- ipeds %>%
  gather(var, val, -`UnitID`, -`Institution Name`) %>%
  mutate(year = str_extract(var, '\\d{4}'),
         var = str_extract(var, '\\w+')) %>%
  spread(var, val) %>%
  mutate(admit_rate = ADMSSN / APPLCN,
         yield = ENRLT / ADMSSN) %>%
  filter(!is.na(admit_rate), !is.na(ENRLT)) %>%
  group_by(year) %>%
  arrange(admit_rate) %>%
  mutate(cum_share_freshman_enrollment = cumsum(ENRLT) / sum(ENRLT)) %>%
  ungroup()

ipedsReshape %>%
  ggplot(aes(x = cum_share_freshman_enrollment, y = admit_rate,
             col = factor(year))) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 1, .2)) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  ggsave('figure/IpedsAdmitRateByCumShareFreshmanEnrollment.png',
         dev = 'png', width = 8, height = 4, scale = 1.4)

ipedsReshape %>%
  mutate(z = ceiling(cum_share_freshman_enrollment * 50) / 50) %>%
  filter(ENRLT > 0) %>%
  group_by(cum_share_freshman_enrollment = z) %>%
  summarize(admit_rate = sum(admit_rate * ENRLT) / sum(ENRLT)) %>%
  bind_rows(tibble(cum_share_freshman_enrollment = 0, admit_rate = 0), .) %>%
  ggplot(aes(x = cum_share_freshman_enrollment, y = admit_rate)) +
  geom_line(lwd = 3) +
  scale_x_continuous(breaks = seq(0, 1, .2)) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  ggsave('figure/IpedsAdmitRateByCumShareFreshmanEnrollment_Average.png',
         dev = 'png', width = 8, height = 4, scale = 1.4)

ipedsReshape %>%
  filter(ENRLT > 100) %>%
  ggplot(aes(x = admit_rate, y = yield)) +
  geom_point(size = .4) +
  scale_x_continuous(breaks = seq(0, 1, .2)) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  ggsave('figure/IpedsYieldAndAdmitRate_2010-2019.png',
         dev = 'png', width = 8, height = 4, scale = 1.4)

ipedsReshape %>%
  group_by(year) %>%
  summarize_at(c('ADMSSN', 'APPLCN', 'ENRLT'), sum) %>%
  mutate(apps_per_enrollee = APPLCN / ENRLT)

