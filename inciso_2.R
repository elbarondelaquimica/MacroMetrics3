#-------------------------------------------------------------#
#               Inciso 2
#-------------------------------------------------------------#

remove(list = ls(all.names = TRUE))
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Variables ####
source("data.R")

# Variables in differences
Y.d <- cbind(i_dif, pi_dif, emae_dif)

# Variables in levels
emae <- dlog_residuos
Y <- cbind(i, pi, emae)

# VAR estimation ####
library(vars)

# Lag order selection
pmax <- 12
popt <- VARselect(Y.d, lag.max = pmax, type = "const")
#popt
p <- popt$selection[2] # HQ

# ------------------------
# Why HQ?
# AIC and FPE select 10 lags (this seems too many lags, with high variance)
# SC selects only 1 lag
# HQ selects 2 lags
# ------------------------

Yd0 <- Y.d[1:pmax, ] # Initial values
Ydt <- Y.d[(pmax - p + 1):nrow(Y.d), ] # Starting in Jan-04

# Estimation
VAR <- VAR(Ydt, p = p, type = "const")

m <- VAR$K # No. of variables in the VAR
N <- VAR$obs # No. of effective sample observations, excluding "p" starting values

# Model checking
roots(VAR, modulus = TRUE)
serial.test(VAR, lags.bg = 12, type = "ES")

# SVAR estimation ####
# A Matrix
Amat <- diag(m)
for (i in 2:m) {
  for (j in 1:(i - 1)) {
    Amat[i, j] <- NA
  }
}

# B Matrix
Bmat <- matrix(0, m, m)
for (i in 1:m) {
  Bmat[i, i] <- NA
}

# SVAR estimation (AB model configuration)
SVAR <- SVAR(VAR, Amat = Amat, Bmat = Bmat, lrtest = FALSE)
SVAR

# SVAR analysis ####
source("PS2_SVAR_Analysis.R")
source("PS2_SVAR_Bootstrap.R")
source("PS2_SVAR_Plots.R")

# Parameters
a <- 0.95 # Confidence level
R <- 500 # No. of bootstrap replications
H <- 8 # Horizon

# Bootstrap replications
Yb <- boot.rb.replicate(VAR, Yd0, pmax, R)

# IRF (bootstrap)
SVAR.SIRF.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, Yb, pmax, H, a, R)
plot.sirf.boot(SVAR.SIRF.boot, m, H)

# Cumulative IRF (bootstrap)
SVAR.SIRF.c.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, Yb, pmax, H, a, R, cumulative = TRUE)
plot.sirf.boot(SVAR.SIRF.c.boot, m, H)

# FEVD (bootstrap)
SVAR.FEVD.boot <- SVAR.fevd.boot(SVAR, Amat, Bmat, Yb, pmax, H, a, R)
plot.fevd.boot(SVAR.FEVD.boot, m, H)

# HD
SVAR.HD <- SVAR.hd(SVAR)
plot.hd(Y.d, SVAR.HD, m, pmax)












