library(ggplot2)
library(sp)
library(stringr)
library(plotly)

setwd('/home/arndt/CorrelAidRepos/duisburch_hack')

# level0 <- readRDS('karten/DEU_adm0.rds')  # national border
# level1 <- readRDS('karten/DEU_adm1.rds')  # state borders
# level2 <- readRDS('karten/DEU_adm2.rds')  # Kreise
level3 <- readRDS('karten/DEU_adm3.rds')  # Gemeinden?
# level4 <- readRDS('karten/DEU_adm4.rds')

# map(level0)
# map(level1)
# map(level2)
# map(level3)

# level2_nrw <- subset(level2, NAME_1 == 'Nordrhein-Westfalen')
level3_nrw <- subset(level3, NAME_1 == 'Nordrhein-Westfalen')

# map(level2_nrw)
# map(level3_nrw)

level3_nrw@data$gkz <- level3_nrw@data$CCA_3

substr(level3_nrw@data$gkz, 6, 6) <- 'X'
level3_nrw@data$gkz <- str_replace(level3_nrw@data$gkz, 'X', '')

level3_nrw <- sp::merge(level3_nrw, d, by.x = 'gkz', by.y = 'gkz00')

level3_nrw_gg <- fortify(level3_nrw)

level3_nrw_gg <- base::merge(level3_nrw_gg,
                             level3_nrw@data[, c('OBJECTID', 'gkz',
                                                 'gebietsname', 'totalPP',
                                                 'gesamtinvestition',
                                                 'erhalten',
                                                 'anzahl')],
                             by.x = 'id', by.y = 'OBJECTID')

k_verschuldung <-
ggplot(level3_nrw_gg, aes(x = long, y = lat, group = group, fill = totalPP,
                          name = gebietsname)) +
  geom_polygon(color = 'black') + xlab('') + ylab('') +
  scale_fill_continuous(name = 'Verschuldung\n pro Kopf (Euro)',
                        low = 'firebrick1', high = 'darkred')

p_verschuldung <-
ggplotly(k_verschuldung, tooltip = c('name', 'fill'))


# ERFE Mittel ------------------------------------------------------------------

k_erfe <-
ggplot(level3_nrw_gg, aes(x = long, y = lat, group = group,
                          fill = gesamtinvestition, name = gebietsname)) +
  geom_polygon(color = 'black') + xlab('') + ylab('') +
  scale_fill_continuous(name = 'ERFE (Euro)',
                        low = 'lightblue', high = 'darkblue')

p_erfe <- ggplotly(k_erfe, tooltip = c('name', 'fill'))