library(demest)

##Make required inputs

border_crossing_data <- readRDS("out/migration_data_2001_2019.rds")

mig_long <- border_crossing_data %>%
  filter(mig_status == "long_term") %>%
  mutate(time = factor(time, levels = unique(time))) %>%
  dtabs(count ~ direction + time) %>%
  Counts()

mig_all <- border_crossing_data %>%
  mutate(time = factor(time, levels = unique(time))) %>%
  dtabs(count ~ direction + time) %>%
  Counts()

##Modelling

filename <- "out/migration_model.est"
model <- Model(y ~ Binomial(mean ~ direction + time), 
               time ~ DLM(season = Season(n=12)),
               jump = 0.05)
estimateModel(model, 
              y = mig_long,
              exposure = mig_all,
              filename = filename,
              nBurnin = 200,
              nSim = 200,
              nChain = 4,
              nThin = 5)

fetchSummary(filename)

prob <- fetch(filename, where = c("model","li", "prob"))
dplot(~time | direction, data = prob, midpoints = "time")
dplot(~time | direction, data = prob, subarray = time > 2016, midpoints = "time")

filename_pred <- "out/migration_model.pred"

##predictModel(filenameEst = filename, filenamePred = filename_pred, n = 12)

mig_long_forecast <- mig_long %>%
  extrapolate(labels = "Mar-2019", type = "missing")

mig_all_forecast <- mig_all %>%
  extrapolate(labels = "Mar-2019", type = "missing")

estimateModel(model, 
              y = mig_long_forecast,
              exposure = mig_all_forecast,
              filename = filename,
              nBurnin = 200,
              nSim = 200,
              nChain = 4,
              nThin = 5)

prob_mcmc <- fetchMCMC(filename, where = c("mod", "li", "pr"))
plot(prob_mcmc, ask = T, sm = F)

seasonal <- fetch(filename, where = c("mod", "hy", "ti", "season"))

dplot(~ time, seasonal, subarray =time > 2016)
