# ------------------------------------------------------------------------------
# CorrelAid Hackathon Dusiburg, 11. Juni 2016
# Kommunale Verschuldung in NRW
# Dieses Skript setzt kommunale Verschuldung und Empfang von EU Fördergeldern
# aus dem Programm EFRE NRW 2014-2020 in Kontext
# ------------------------------------------------------------------------------

library(xlsx)

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

# OP ERFE ----------------------------------------------------------------------

e <- read.xlsx('daten/16-01-22_DN_Liste_der_Vorhaben_20151231.xlsx',
               sheetIndex = 1, rowIndex = 5:165, colIndex = 1:10)
