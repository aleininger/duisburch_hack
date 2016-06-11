library(texreg)

summary(m1 <- glm(erhalten ~ totalPP, d,
                  family = binomial()))

summary(m2 <- glm(erhalten ~ totalPP + kreisfrei, d,
                  family = binomial()))

summary(m3 <- glm(erhalten ~ totalPP + kreisfrei + bevoelkerung, d,
                  family = binomial()))

# screenreg(list(m1, m2, m3))

# summary(lm(gesamtinvestition ~ totalPP + kreisfrei + bevoelkerung, d))

X_totalPP_f <- data.frame(
  totalPP = min(d$totalPP, na.rm = T):max(d$totalPP, na.rm = T),
  kreisfrei = FALSE,
  bevoelkerung = mean(d$bevoelkerung, na.rm = T)
)

X_totalPP_t <- X_totalPP_f
X_totalPP_t$kreisfrei <- TRUE

X_totalPP <- rbind(X_totalPP_f, X_totalPP_t)

X_totalPP$y <- predict(m3, newdata = X_totalPP, type = 'response')
X_totalPP$se <- predict(m3, newdata = X_totalPP,
                        se.fit = T, type = 'response')$se.fit

X_totalPP$upr <- X_totalPP$y + 1.96 * X_totalPP$se
X_totalPP$lwr <- X_totalPP$y - 1.96 * X_totalPP$se

f_pp_totalPP <-
ggplot(X_totalPP, aes(x = totalPP, y = y, ymin = lwr, ymax = upr,
                      color = kreisfrei, fill = kreisfrei)) +
  geom_line() + geom_ribbon(alpha = .7) +
  geom_rug(data = d, aes(x = totalPP, y = NULL, ymin = NULL, ymax = NULL),
           sides = 'b') +
  ylab('Vorhersagte Wahrscheinlichkeit des Erhalts von ERFE') +
  xlab('Verschuldung pro Kopf (Euro)')

X_kreisfrei <- data.frame(totalPP = mean(d$totalPP, na.rm = T),
                          kreisfrei = c(TRUE, FALSE),
                          bevoelkerung = mean(d$bevoelkerung, na.rm = T))

X_kreisfrei$y <- predict(m3, newdata = X_kreisfrei, type = 'response')
X_kreisfrei$se <- predict(m3, newdata = X_kreisfrei,
                        se.fit = T, type = 'response')$se.fit

X_kreisfrei$upr <- X_kreisfrei$y + 1.96 * X_kreisfrei$se
X_kreisfrei$lwr <- X_kreisfrei$y - 1.96 * X_kreisfrei$se

f_pp_kreisfrei <-
  ggplot(X_kreisfrei, aes(x = kreisfrei, y = y, ymin = lwr, ymax = upr)) +
  geom_point() + geom_errorbar(width = 0) +
  ylab('Vorhersagte Wahrscheinlichkeit des Erhalts von ERFE') +
  scale_x_discrete(breaks = c(TRUE, FALSE) ,
                   labels = c('Kreisfreie Stadt', 'Gemeinde')) +
  xlab('') + theme_bw()

X_bevoelkerung_f <- data.frame(
  bevoelkerung = seq(from = min(d$bevoelkerung, na.rm = T),
                     to = max(d$bevoelkerung, na.rm = T),
                     length.out = 1000),
  kreisfrei = FALSE,
  totalPP = mean(d$totalPP, na.rm = T)
)

X_bevoelkerung_t <- X_bevoelkerung_f
X_bevoelkerung_t$kreisfrei <- TRUE

X_bevoelkerung <- rbind(X_bevoelkerung_f, X_bevoelkerung_t)

X_bevoelkerung$y <- predict(m3, newdata = X_bevoelkerung, type = 'response')
X_bevoelkerung$se <- predict(m3, newdata = X_bevoelkerung,
                        se.fit = T, type = 'response')$se.fit

X_bevoelkerung$upr <- X_bevoelkerung$y + 1.96 * X_bevoelkerung$se
X_bevoelkerung$lwr <- X_bevoelkerung$y - 1.96 * X_bevoelkerung$se

f_pp_bevoelkerung <-
  ggplot(X_bevoelkerung, aes(x = bevoelkerung, y = y, ymin = lwr, ymax = upr,
                        color = kreisfrei, fill = kreisfrei)) +
  geom_line() + geom_ribbon(alpha = .7) +
  geom_rug(data = d, aes(x = bevoelkerung, y = NULL, ymin = NULL, ymax = NULL),
           sides = 'b') +
  ylab('Vorhersagte Wahrscheinlichkeit\n des Erhalts von ERFE') +
  xlab('Einwohnerzahl')
