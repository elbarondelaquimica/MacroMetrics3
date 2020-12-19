#-------------------------------------------------------------#
#               Inciso 1
#-------------------------------------------------------------#  
# Clean and set directory:
remove(list = ls(all.names = TRUE))
gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Required libraries
library(tsbox) # for working with different objects
library(ggplot2)  # for creating graphs
library(urca) # for tests


# Variables
source("data.R")
Y.d <- Y
emae <- dlog_residuos
Y <- cbind(emae, i, r)


# Unitary root testing ####
# Dickey Fuller test with drift

#Para las variables sin estar en diferencias.
r_df <- ur.df(y = r, type = "drift", lags = 2, selectlags = "Fixed") # ***
i_df <- ur.df(y = i, type = "drift", lags = 2, selectlags = "Fixed") # ***
emae_df <- ur.df(y = emae, type = "drift", lags = 2, selectlags = "Fixed") # ***
pi_df <- ur.df(y = pi, type = "drift", lags = 2, selectlags = "Fixed") # ***

# Para las variables en diferencias
i_dif_df <- ur.df(y = i_dif, type = "drift", lags = 2, selectlags = "Fixed") # ***
pi_dif_df <- ur.df(y = pi_dif, type = "drift", lags = 2, selectlags = "Fixed") # ***
emae_dif_df <- ur.df(y = emae_dif, type = "drift", lags = 2, selectlags = "Fixed") # ***


# Show results
summary(r_df)
summary(i_df)
summary(emae_df)
summary(pi_df)

summary(i_dif_df)
summary(pi_dif_df)
summary(emae_dif_df)


# Dickey Fuller test with deterministic trend

#Para las variables sin estar en diferencias.
r_df2 <- ur.df(y = r, type = "trend", lags = 2, selectlags = "Fixed") # ***
i_df2 <- ur.df(y = i, type = "trend", lags = 2, selectlags = "Fixed") # ***
emae_df2 <- ur.df(y = emae, type = "trend", lags = 2, selectlags = "Fixed") # ***
pi_df2 <- ur.df(y = pi, type = "trend", lags = 2, selectlags = "Fixed") # ***

# Para las variables en diferencias
i_dif_df2 <- ur.df(y = i_dif, type = "trend", lags = 2, selectlags = "Fixed") # ***
pi_dif_df2 <- ur.df(y = pi_dif, type = "trend", lags = 2, selectlags = "Fixed") # ***
emae_dif_df2 <- ur.df(y = emae_dif, type = "trend", lags = 2, selectlags = "Fixed") # ***


# Show results
summary(r_df2)
summary(i_df2)
summary(emae_df2)
summary(pi_df2)

summary(i_dif_df2)
summary(pi_dif_df2)
summary(emae_dif_df2)


# Work with data ####
# Convert to data frame and merge in order to have dataframes for ggplot.
emae.df <- ts_df(emae)
i.df <- ts_df(i)
#pi.df <- ts_df(pi)  # For some reason, it raises an error
r.df <- ts_df(r)

source_of_graphs <- merge(emae.df, i.df, by = 1, all=TRUE)
source_of_graphs2 <- merge(source_of_graphs, r.df, by = 1, all=TRUE)
source_of_graphs2$pi <- pi
source_of_graphs2$pi <- as.numeric(source_of_graphs2$pi)
colnames(source_of_graphs2) <- c("time", "emae", "i", "r", "pi")

emae_dif.df <- ts_df(emae_dif)
i_dif.df <- ts_df(i_dif)
pi_dif.df <- ts_df(pi_dif) 

source_of_graphs3 <- merge(emae_dif.df, i_dif.df, by=1, all = TRUE)
source_of_graphs3 <- merge(source_of_graphs3, pi_dif.df, by=1, all = TRUE)
colnames(source_of_graphs3) <- c("time", "emae_dif", "i_dif", "pi_dif")

# Remove variables that will no longer be used 
remove(emae, i, r, pi, emae_dif, i_dif, pi_dif)


# Graph the "originals" ####
source("inciso_1_aux.R")

graph_emae <- make_graph(df = source_of_graphs2, colour = "deepskyblue4", variable = source_of_graphs2$emae, title = "")
graph_emae
ggsave("emae.png")

graph_i <- make_graph(colour = "goldenrod3", variable = source_of_graphs2$i, title = "")
graph_i
ggsave("i.png")

graph_pi <- make_graph(colour = "firebrick4", variable = source_of_graphs2$pi, title = "")
graph_pi
ggsave("pi.png")

graph_r <- make_graph(colour = "slategrey", variable = source_of_graphs2$r, title = "")
graph_r
ggsave("r.png")

# Graph the "differences" ####
graph_emae_dif <- make_graph(df = source_of_graphs3, colour = "deepskyblue4", variable = source_of_graphs3$emae_dif, title = "")
graph_emae_dif
ggsave("emae_dif.png")

graph_i_dif  <- make_graph(df = source_of_graphs3, colour = "goldenrod3", variable = source_of_graphs3$i_dif, title = "")
graph_i_dif
ggsave("i_dif.png")

graph_pi_dif <- make_graph(df = source_of_graphs3, colour = "firebrick4", variable = source_of_graphs3$pi_dif, title = "")
graph_pi_dif
ggsave("pi_dif.png")


