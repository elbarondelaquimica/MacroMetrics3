#-------------------------------------------------------------#
#               Inciso 3
#-------------------------------------------------------------#

remove(list = ls(all.names = TRUE))
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Variables ####
source("data.R")

# Variables in differences
r_dif <- diff(r)
Y.d <- cbind(emae_dif, r_dif, i_dif)

# Variables in levels
emae <- dlog_residuos
Y <- cbind(emae, r, i)

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
# SVAR estimation (AB model configuration)
SVAR <- BQ(VAR)
SVAR

# SVAR t0 impact matrix
S.ML <- t(resid(VAR)) %*% resid(VAR) / N
S.OLS <- t(resid(VAR)) %*% resid(VAR) / (N - m * p - 1)
S.OLS

# SVAR t0 impact matrix (implied by LR restrictions)
P <- SVAR$B
S.SVAR <- P %*% t(P)
S.SVAR

# Other SVAR parameters
pars.R <- Bcoef(VAR) # VAR
pars.S <- solve(P, pars.R) # SVAR
pars.R
pars.S

# SVAR analysis ####
source("PS3_SVAR_Analysis_LR.R")
source("PS3_SVAR_Bootstrap_LR.R")
source("PS3_SVAR_Plots_LR.R")
source("SVAR_SIRF_m.R")
source("SIRF_transform.R")

H <- 12 # Horizon
a <- 0.95 # Confidence level
R <- 500 # No. of bootstrap replications

# Bootstrap replications
Yb <- boot.rb.replicate(VAR, Yd0, pmax, R)


# IRF (bootstrap)
#SVAR.SIRF.boot.LR <- SVAR.sirf.boot.LR(SVAR, Yb, pmax, H, a, R)
#plot.sirf.boot(SVAR.SIRF.boot.LR, m, H)

# IRF (bootstrap with modified file)
SVAR.SIRF.boot.LR2 <- SVAR.sirf.boot.LR.modified(SVAR, Yb, pmax, H, a, R)
plot.sirf.boot(SVAR.SIRF.boot.LR2, m, H)


# Cumulative IRF (bootstrap)
#SVAR.SIRF.c.boot.LR <- SVAR.sirf.boot.LR(SVAR, Yb, pmax, H, a, R, cumulative = TRUE)
#plot.sirf.boot(SVAR.SIRF.c.boot.LR, m, H)

# Cumulative IRF (bootstrap with modified file)
SVAR.SIRF.c.boot.LR2 <- SVAR.sirf.boot.LR.modified(SVAR, Yb, pmax, H, a, R, cumulative = TRUE)
plot.sirf.boot(SVAR.SIRF.c.boot.LR2, m, H)


# FEVD (bootstrap)
SVAR.FEVD.boot.LR <- SVAR.fevd.boot.LR(SVAR, Yb, pmax, H, a, R)
plot.fevd.boot(SVAR.FEVD.boot.LR, m, H)

# HD
#SVAR.HD <- SVAR.hd(SVAR)
#plot.hd(Y.d, SVAR.HD, m, pmax)


# SVAR analysis: transformed IRF for inflation (TBC) ####
SVAR.SIRF.c.boot.LR.inflation <- trans.pw.boot(operator = "-", I = SVAR.SIRF.c.boot.LR2$pe, vi=3, vj=2, sk=3, boot = SVAR.SIRF.c.boot.LR2$boot, a)
plot(SVAR.SIRF.c.boot.LR2$boot)
