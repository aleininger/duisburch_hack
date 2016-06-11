# ------------------------------------------------------------------------------
# CorrelAid Hackathon Dusiburg, 11. Juni 2016
# Kommunale Verschuldung in NRW
# Dieses Skript setzt kommunale Verschuldung und Empfang von EU Fördergeldern
# aus dem Programm EFRE NRW 2014-2020 in Kontext
# ------------------------------------------------------------------------------

library(xlsx)
library(dplyr)
library(stringr)

setwd('~/CorrelAidRepos/duisburch_hack/')

# ------------------------------------------------------------------------------
# Daten laden
# ------------------------------------------------------------------------------

# Verschuldung -----------------------------------------------------------------

s <- read.csv('daten/schulden_gemeinden.csv', stringsAsFactors = F,
              sep = ';', skip = 7, header = F, fileEncoding = 'latin1')
# Verschuldungsdaten lesen, Umlaute korrigieren

names(s) <- apply(s[1:3, ], 2, paste, collapse = '')  # Variablenname aus den ersten
# drei Zeilen erstellen

s <- s[-(1:3), ]  # die ersten drei Zeilen löschen

names(s)[1:27] <- c('gkz', 'gebietsname', 'bevoelkerung', 'total', 'totalPP', 'totalTraeger', 'totalTraegerPP', 'totalNetto', 'totalNettoPP', 'totalHaushalt', 'totalHaushaltPP', 'haushaltKredite', 'haushaltKreditePP', 'haushaltInvest', 'haushaltInvestPP', 'totalEigenbetriebe1', 'totalEigenbetriebe1PP', 'eigenbetriebe1Traeger', 'eigenbetriebe1TraegerPP', 'eigenbetriebe1Gemeinde', 'eigenbetriebe1GemeindePP', 'totalEigenbetriebe2', 'totalEigenbetriebe2PP', 'eigenbetriebe2Traeger', 'eigenbetriebe2TraegerPP', 'eigenbetriebe2Gemeinde', 'eigenbetriebe2GemeindePP')

s <- s[-which(s$gebietsname == ''), ]
s$gebietsname <- trimws(s$gebietsname)

# Numerische Variablen numerisch machen
s[, 3:(ncol(s))] <- apply(s[, 3:(ncol(s))], 2, str_replace, ',', '.')
s[, 3:(ncol(s))] <- apply(s[, 3:(ncol(s))], 2, as.numeric)

# Neue GKZ Codierung, die dem Projektedatensat entspricht
s$gkz00 <- s$gkz
s$gkz00[which(nchar(s$gkz00) == 2)] <- paste0(s$gkz00[which(nchar(s$gkz00) == 2)], '000000')
s$gkz00[which(nchar(s$gkz00) == 3)] <- paste0(s$gkz00[which(nchar(s$gkz00) == 3)], '00000')
s$gkz00[which(nchar(s$gkz00) == 5)] <- paste0(s$gkz00[which(nchar(s$gkz00) == 5)], '000')

# Kreisfreie Städte codieren
s$kreisfrei <- grepl('krfr. Stadt', s$gebietsname)
s$kreisfrei[which(nchar(s$gkz) <= 3)] <- NA

# OP ERFE ----------------------------------------------------------------------

e <- read.xlsx('daten/16-01-22_DN_Liste_der_Vorhaben_20151231.xlsx',
               sheetIndex = 1, rowIndex = 5:165, colIndex = 1:10)

names(e) <- tolower(names(e))

e$gebietskennziffer <- e$gebietskennziffer %>% as.character()

names(e)[6] <- 'gesamtinvestition'

e_agg <- e %>% group_by(gebietskennziffer) %>%
  summarise(gesamtinvestition = sum(gesamtinvestition),
           anzahl = n())

# merge ------------------------------------------------------------------------

d <- base::merge(s, e_agg, by.x = 'gkz00', by.y = 'gebietskennziffer', all.x = T)

d$erhalten <- !is.na(d$gesamtinvestition)

d <- d[which(!is.na(d$kreisfrei)), ]

save(list = c('s', 'e' ,'d'), file = 'daten/daten.RData')
