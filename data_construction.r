library(readr)
library(dplyr)
library(ggplot2)

obs = read_csv2('data_v2/clean_v2.csv')[1:19] %>%
  rename_all(make.names) %>%
  group_by(ID, seq.nr) %>%
  slice(1) %>%
  ungroup() %>%
  filter(interference == 0) %>%
  mutate(QT_ARI = ARI + 2 * dARI) %>%
  select(ID, Machine, seq.nr, RR = RR.interval, QT = external.QT, QTc = external.QTc, QT_ARI) %>%
  mutate(
    QT_ARIc_bazette = round( QT_ARI / (RR / 1000)^(1/2) ), 
    QT_ARIc_lm = round(predict(lm(data = ., QT_ARI ~ RR), .)),
    QTc_lm = round(predict(lm(data = ., QT ~ RR), .))
    )
  
  
# Quick viz to inspect data
ggplot(data = obs, aes(RR, QTc)) + 
  geom_point(aes(color = factor(Machine), shape = factor(ID)), size = 2) + 
  scale_shape_manual(values= LETTERS[1:19]) +
  geom_smooth(method = 'lm') + 
  theme_minimal()
