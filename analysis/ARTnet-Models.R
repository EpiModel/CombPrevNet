
e <- readRDS("est/epistats.rda")
e1 <- e$acts.mod

summary(e1)

View(cbind(coef(e1), confint(e1)))

dat <- expand.grid(duration = seq(50, 500, 50),
                   race.combo = c(1, 6),
                   comb.age = c(40, 80),
                   hiv.concord.pos = 0,
                   city = 1,
                   ptype = 1:2)
pred <- predict(e1, newdata = dat, type = "response")/52
pred <- cbind(dat, pred)
pred

library("ggplot2")

pred2 <- pred
pred2$comb.age <- as.factor(pred2$comb.age)
pred2$ptype <- as.factor(pred2$ptype)
pred2$race.combo <- ifelse(pred2$race.combo == 1, "BB", "WW")

p1 <- ggplot(pred2, aes(duration, pred)) +
  geom_line(aes(col = comb.age,
                lty = ptype),
            lwd = 1) +
  facet_grid(cols = vars(race.combo)) +
  scale_color_brewer(palette = "Set1") +
  xlab(label = "Partnership Duration in Weeks") +
  ylab(label = "Predicted Act Rate per Week") +
  theme_minimal()
p1

ggsave(filename = "analysis/actsMod.pdf", width = 9, height = 5.5, device = "pdf", units = "in")
ggsave(filename = "analysis/actsMod.jpg", width = 9, height = 5.5, device = "jpeg", units = "in")


e2 <- e$cond.mc.mod
summary(e2)

options(scipen = 6)
View(cbind(coef(e2), confint(e2)))


dat <- expand.grid(duration = seq(50, 500, 50),
                   race.combo = 1,
                   comb.age = c(40, 80),
                   hiv.concord.pos = 0,
                   city = 1,
                   ptype = 1:2,
                   prep = 0:1)
pred <- predict(e2, newdata = dat, type = "response")
pred <- cbind(dat, pred)
pred

pred2 <- pred
pred2$comb.age <- as.factor(pred2$comb.age)
pred2$ptype <- as.factor(pred2$ptype)
pred2$prep <- ifelse(pred2$prep == 1, "Current PrEP", "No PrEP")

p1 <- ggplot(pred2, aes(duration, pred)) +
  geom_line(aes(col = comb.age,
                lty = ptype),
            lwd = 1) +
  facet_grid(cols = vars(prep)) +
  scale_color_brewer(palette = "Set1") +
  xlab(label = "Partnership Duration in Weeks") +
  ylab(label = "Predicted Probability of Condom Use per Act") +
  theme_minimal()
p1

ggsave(filename = "analysis/condMod1.pdf", width = 9, height = 5.5, device = "pdf", units = "in")
ggsave(filename = "analysis/condMod1.jpg", width = 9, height = 5.5, device = "jpeg", units = "in")


e3 <- e$cond.oo.mod
summary(e3)
View(cbind(coef(e3), confint(e3)))

dat <- expand.grid(race.combo = 1:6,
                   comb.age = seq(30, 120, 5),
                   hiv.concord.pos = 0,
                   city = 1,
                   prep = 0:1)
pred <- predict(e3, newdata = dat, type = "response")
pred <- cbind(dat, pred)
pred

pred2 <- pred
pred2$race.combo <- as.factor(pred2$race.combo)
pred2$prep <- ifelse(pred2$prep == 1, "Current PrEP", "No PrEP")

p1 <- ggplot(pred2, aes(comb.age, pred)) +
  geom_line(aes(col = race.combo,
                lty = prep),
            lwd = 0.9) +
  scale_color_brewer(palette = "Set1") +
  xlab(label = "Combined Ego and Partner Age in Years") +
  ylab(label = "Predicted Probability of Condom Use within Contact") +
  theme_minimal()
p1

ggsave(filename = "analysis/condMod2.pdf", width = 9, height = 5.5, device = "pdf", units = "in")
ggsave(filename = "analysis/condMod2.jpg", width = 9, height = 5.5, device = "jpeg", units = "in")


### Demographics ---

e <- readRDS("est/epistats.rda")
n <- readRDS("est/netstats.rda")
n$demog
