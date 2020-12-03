### R code from vignette source 'etmCIF_tutorial.Rnw'

###################################################
### code chunk number 1: etmCIF_tutorial.Rnw:34-37
###################################################
library(etm)
library(survival)
data(abortion)


###################################################
### code chunk number 2: etmCIF_tutorial.Rnw:51-52
###################################################
head(abortion)


###################################################
### code chunk number 3: etmCIF_tutorial.Rnw:96-99
###################################################
cif.abortion <- etmCIF(Surv(entry, exit, cause != 0) ~ group,
                   abortion, etype = cause, failcode = 3)
cif.abortion


###################################################
### code chunk number 4: etmCIF_tutorial.Rnw:108-109
###################################################
s.cif.ab <- summary(cif.abortion)


###################################################
### code chunk number 5: etmCIF_tutorial.Rnw:116-117
###################################################
s.cif.ab


###################################################
### code chunk number 6: etmCIF_tutorial.Rnw:129-130
###################################################
plot(cif.abortion)


###################################################
### code chunk number 7: etmCIF_tutorial.Rnw:145-148
###################################################
plot(cif.abortion, curvlab = c("Control", "Exposed"), ylim = c(0, 0.6),
     ci.type = "bars", pos.ci = 27, col = c(1, 2), ci.lwd = 6,
     lwd = 2, lty = 1, cex = 1.3)


###################################################
### code chunk number 8: etmCIF_tutorial.Rnw:167-170
###################################################
plot(cif.abortion, curvlab = c("Control", "Exposed"), ylim = c(0, 0.6),
     ci.type = "bars", pos.ci = c(27, 28), col = c(1, 1), ci.lwd = 6,
     lwd = 2, lty = c(2, 1), cex = 1.3)


###################################################
### code chunk number 9: etmCIF_tutorial.Rnw:183-185
###################################################
plot(cif.abortion, curvlab = c("Control", "Exposed"), ylim = c(0, 0.5),
     ci.type = "pointwise", col = c(1, 2), lwd = 2, lty = 1, cex = 1.3)


###################################################
### code chunk number 10: etmCIF_tutorial.Rnw:200-206
###################################################
plot(cif.abortion, which.cif = c(1, 2), ylim = c(0, 0.8), lwd = 2,
     col = c(1, 1, 2, 2), lty = c(1, 2, 1, 2), legend = FALSE)
legend(0, 0.8, c("Control", "Exposed"), col = c(1, 2), lty = 1,
       bty = "n", lwd = 2)
legend(0, 0.7, c("ETOP", "Life Birth"), col = 1, lty = c(1, 2),
       bty = "n", lwd = 2)


###################################################
### code chunk number 11: etmCIF_tutorial.Rnw:226-232
###################################################
abortion$status <- with(abortion, ifelse(cause == 2, "life birth",
                        ifelse(cause == 1, "ETOP", "spontaneous abortion")))
abortion$status <- factor(abortion$status)

abortion$treat <- with(abortion, ifelse(group == 0, "control", "exposed"))
abortion$treat <- factor(abortion$treat)


###################################################
### code chunk number 12: etmCIF_tutorial.Rnw:237-240
###################################################
new.cif <- etmCIF(Surv(entry, exit, status != 0) ~ treat, abortion,
                  etype = status, failcode = "spontaneous abortion")
new.cif


###################################################
### code chunk number 13: etmCIF_tutorial.Rnw:261-262
###################################################
trprob(new.cif[[1]], "0 spontaneous abortion", c(1, 10, 27))


###################################################
### code chunk number 14: etmCIF_tutorial.Rnw:276-277 (eval = FALSE)
###################################################
## lines(cif.abortion[[2]], tr.choice = "0 1", col = 2, lwd = 2)


###################################################
### code chunk number 15: etmCIF_tutorial.Rnw:282-286
###################################################
plot(cif.abortion, curvlab = c("Control", "Exposed"), ylim = c(0, 0.6),
     ci.type = "bars", pos.ci = c(27, 28), col = c(1, 1), ci.lwd = 6,
     lwd = 2, lty = c(2, 1), cex = 1.3)
lines(cif.abortion[[2]], tr.choice = "0 1", col = 2, lwd = 2)


