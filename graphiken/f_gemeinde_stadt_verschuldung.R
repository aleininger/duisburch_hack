library(ggplot2)
library(ggthemes)

f_gemeinde_stadt_verschuldung <-
ggplot(s, aes(x = jitter(as.numeric(kreisfrei)), y = totalPP)) +
  geom_point() +
  geom_segment(x = -.5, xend = .5,
               y = mean(s$totalPP[which(s$kreisfrei == F)], na.rm = T),
               yend = mean(s$totalPP[which(s$kreisfrei == F)], na.rm = T),
               linetype = 'dashed') +
  geom_segment(x = .5, xend = 1.5,
               y = mean(s$totalPP[which(s$kreisfrei == T)], na.rm = T),
               yend = mean(s$totalPP[which(s$kreisfrei == T)], na.rm = T),
               linetype = 'dashed') +
  scale_x_continuous(breaks = c(0, 1), labels = c('Gemeinden', 'Kreisfreie Städte'),
                     limits = c(-.5, 1.5)) +
  theme_tufte() + ylab('Verschuldung pro Kopf (Euro)') + xlab('')

png('graphiken/f_gemeinde_stadt_verschuldung.png')
f_gemeinde_stadt_verschuldung
dev.off()

max(s$totalPP[which(s$kreisfrei == F)], na.rm = T)

s$gebietsname[which(s$totalPP == max(s$totalPP[which(s$kreisfrei == F)], na.rm = T))]

png('graphiken/f_gemeinde_stadt_verschuldung.png')
f_gemeinde_stadt_verschuldung + annotate('text', x = -.1,
                                         y = max(s$totalPP[which(s$kreisfrei == F)], na.rm = T),
                                         label = s$gebietsname[which(s$totalPP == max(s$totalPP[which(s$kreisfrei == F)], na.rm = T))]
) +
  annotate('text', x = .7,
           y = max(s$totalPP[which(s$kreisfrei == T)], na.rm = T),
           label = s$gebietsname[which(s$totalPP == max(s$totalPP[which(s$kreisfrei == T)], na.rm = T))]
  )  +
  annotate('text', x = .7,
           y = min(s$totalPP[which(s$kreisfrei == T)], na.rm = T),
           label = s$gebietsname[which(s$totalPP == min(s$totalPP[which(s$kreisfrei == T)], na.rm = T))]
  )
dev.off()