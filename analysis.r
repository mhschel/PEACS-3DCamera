ID_mean = obs %>% 
  group_by(ID) %>% 
  summarise(
  mean_QTc_lm = mean(QTc_lm),
  mean_QT_ARIc_lm = mean(QT_ARIc_lm),
  median_QTc_lm = median(QTc_lm),
  median_QT_ARIc_lm = median(QT_ARIc_lm)
  )

# Difference from the mean
obs_analysis = obs %>% left_join(ID_mean) %>%
  mutate(
    QTc_diff_ID_mean = QTc_lm - mean_QTc_lm,
    QT_ARIc_diff_ID_mean = QT_ARIc_lm - mean_QT_ARIc_lm,
    QTc_diff_ID_median = QTc_lm - median_QTc_lm,
    QT_ARIc_diff_ID_median = QT_ARIc_lm - median_QT_ARIc_lm
  ) 



# Get summary statistics for accuracy
obs_analysis %>% 
  group_by(Machine) %>%
  summarise(
    mean_diff = mean(QT_ARIc_diff_ID_mean),
    sd_diff = sd(QT_ARIc_diff_ID_mean),
    rms = mean(QT_ARIc_diff_ID_mean^2)^(1/2),
    mae = mean(abs(QT_ARIc_diff_ID_mean))
  )

# Test skewness and kurtosis
library(e1071)
obs_analysis %>%
  group_by(Machine) %>%
  summarise(skew = skewness(QTc_diff_ID_mean),
            kurt = kurtosis(QTc_diff_ID_mean),
            normality = shapiro.test(QTc_diff_ID_mean)$p.value)


ggplot(obs_analysis, aes(x = QTc_diff_ID_mean, group = Machine, fill = Machine)) + 
  geom_histogram(position = 'dodge', binwidth = 5) + theme_minimal()





# Calculate difference in standard deviations between approaches
sd_test = obs_analysis %>% group_by(ID, Machine) %>%
  summarize(measurement_sd = sd(QTc_lm))

ggplot(sd_test %>% filter(Machine == 'GE'), aes(x = reorder(factor(ID), measurement_sd), y = measurement_sd + 0.1)) + 
  geom_bar(position = 'dodge', stat = 'identity') + theme_minimal() + xlab('Patient ID') + 
  xlab('Patient ID') + ylab('Measurement SD (ms)') + scale_y_continuous(limits = c(0,20))
ggsave('without_camera.png', device = 'png')

ggplot(sd_test %>% filter(Machine == 'Mortara'), aes(x = reorder(factor(ID), measurement_sd), y = measurement_sd + 0.1)) + 
  geom_bar(position = 'dodge', stat = 'identity') + theme_minimal() + 
  xlab('Patient ID') + ylab('Measurement SD (ms)') + scale_y_continuous(limits = c(0,20))
ggsave('with_camera.png', device = 'png')


stat_function(fun = dnorm, args = list(mean = mean(df$PF), sd = sd(df$PF)))

sd_per_machine = sd_test %>%
  group_by(Machine) %>%
  summarise(mean_sd = mean(measurement_sd, na.rm = T))

sd_test_t_test = sd_test %>%
  group_by(ID) %>%
  mutate(machine_sd_diff = measurement_sd[1] - measurement_sd[2]) %>% 
  group_by(ID) %>%
  summarise(machine_sd_diff = mean(machine_sd_diff))

hist(sd_test_t_test$machine_sd_diff, xlab = 'SD Without Camera - SD With 3D Camera')

t.test(x = sd_test_t_test$machine_sd_diff, mu = 0)

# Measurement error vs 'true mean'
meas_error = obs_analysis %>% group_by(Machine) %>%
  summarize(measurement_sd = sd(QTc_diff_ID_mean),
            measurement_rms = mean(QTc_diff_ID_mean^2)^(1/2),
            measurement_mae = mean(abs(QTc_diff_ID_mean)))

obs_analysis %>% summarise(t_test = t.test(QTc_diff_ID_mean ~ Machine, data = .)$p.value)
  
# Calculate strongly deviant measurement from GE
GE_obs = obs_analysis %>%
  filter(Machine == 'GE')
mean(GE_obs$QTc_diff_ID_median)
median(GE_obs$QTc_diff_ID_median)
sum(GE_obs$QTc_diff_ID_mean < 0)  
sum(GE_obs$QTc_diff_ID_mean > 0)  



