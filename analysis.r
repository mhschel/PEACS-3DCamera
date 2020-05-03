ID_mean = obs %>% 
  group_by(ID) %>% 
  summarise(
  mean_QTc_lm = mean(QTc_lm),
  mean_QT_ARIc_lm = mean(QT_ARIc_lm),
  median_QTc_lm = median(QTc_lm),
  median_QT_ARIc_lm = median(QT_ARIc_lm)
  )

obs_analysis = obs %>% left_join(ID_mean) %>%
  mutate(
    QTc_diff_ID_mean = QTc_lm - mean_QTc_lm,
    QT_ARIc_diff_ID_mean = QT_ARIc_lm - mean_QT_ARIc_lm,
    QTc_diff_ID_median = QTc_lm - median_QTc_lm,
    QT_ARIc_diff_ID_median = QT_ARIc_lm - median_QT_ARIc_lm
  ) 

group_by(Machine) %>%
  summarise(
    mean_diff = mean(QT_ARIc_diff_ID_mean),
    sd_diff = sd(QT_ARIc_diff_ID_mean),
    rms = mean(QT_ARIc_diff_ID_mean^2)^(1/2),
    mae = mean(abs(QT_ARIc_diff_ID_mean))
  )

ggplot(obs_analysis, aes(x = QTc_diff_ID_median, group = Machine, fill = Machine)) + 
  geom_histogram(position = 'dodge', binwidth = 5) + theme_minimal()
