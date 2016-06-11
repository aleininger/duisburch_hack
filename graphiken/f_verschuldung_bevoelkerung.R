splot <- s[which(!is.na(s$kreisfrei)), ]

f_verschuldung_bevoelkerung <-
ggplot(splot, aes(x = bevoelkerung, y = totalPP, color = kreisfrei,
                  name = gebietsname)) +
  geom_point() +
  ylab('Verschuldung pro Kopf (Euro)') + xlab('Log(Bevölkerung)') +
  scale_color_discrete(name = '', breaks = c(TRUE, FALSE),
                       labels = c('Kreisfrei', 'Gemeinde')) +
  theme_bw() + theme(legend.position = c(.9, .1))

p_verschuldung_bevoelkerung <- ggplotly(f_verschuldung_bevoelkerung,
                                        tooltip = c('name', 'y', 'x'))
