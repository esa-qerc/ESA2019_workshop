# set a seed to make sure we don't get ridiculous simulated values
set.seed(2019-11-27)

# simulate some data with the following attributes
n_sites <- 200
n_species <- 3

# we need some predictor variables
predictors <- data.frame(
  intercept = rep(1, n_sites),
  temperature_c = rnorm(n_sites, mean = 25, sd = 3),
  precipitation_mm = rnorm(n_sites, mean = 500, sd = 30),
  tree_cover_pc = runif(n_sites, min = 20, max = 80),
  flowering_pc = runif(n_sites, min = 0, max = 45)
)

# simulate some species-level associations with covariates
n_covariates <- ncol(predictors)
covariate_effects <- matrix(rnorm(n_species * n_covariates, mean = 0, sd = (1 / apply(predictors, 2, mean))),
                            nrow = n_covariates,
                            ncol = n_species)

# we can use these to create a `linear_predictor` that gives the mean
#   probability of occurrence for each species at each site (assuming
#   a logit link function)
linear_predictor <- plogis(as.matrix(predictors) %*% covariate_effects)

# we can use these to simulate species occurrences
species_occurrences <- matrix(
  rbinom(n = (n_sites * n_species), size = 1, prob = linear_predictor),
  nrow = n_sites,
  ncol = n_species
)

# now we want to fit a GLM to these data to estimate probabilities of
#   occurrence as as function of the environmental variables

# start by rescaling the environmental variables so they're all centered
#   and have variance = 1
predictors$temperature_std <-
  (predictors$temperature_c - mean(predictors$temperature_c)) /
  sd(predictors$temperature_c)
predictors$precipitation_std <-
  (predictors$precipitation_mm - mean(predictors$precipitation_mm)) /
  sd(predictors$precipitation_mm)
predictors$tree_cover_std <-
  (predictors$tree_cover_pc - mean(predictors$tree_cover_pc)) /
  sd(predictors$tree_cover_pc)
predictors$flowering_std <-
  (predictors$flowering_pc - mean(predictors$flowering_pc)) /
  sd(predictors$flowering_pc)

# create a data set with everything includes
data_set <- data.frame(
  occ_sp1 = species_occurrences[, 1],
  occ_sp2 = species_occurrences[, 2],
  occ_sp3 = species_occurrences[, 3],
  predictors
)

# fit a GLM model for the first species
model_sp1 <- glm(occ_sp1 ~ temperature_std + precipitation_std + tree_cover_std + flowering_std,
                 data = data_set,
                 family = binomial())

# repeat for species 2 and 3
model_sp2 <- glm(occ_sp2 ~ temperature_std + precipitation_std + tree_cover_std + flowering_std,
                 data = data_set,
                 family = binomial())
model_sp3 <- glm(occ_sp3 ~ temperature_std + precipitation_std + tree_cover_std + flowering_std,
                 data = data_set,
                 family = binomial())

# now we can make some plots of occurrence probabilities against predictors

# how many values should we use to draw our fitted association?
n_plot <- 100

# we need a sequence of predictor values (temperature, here) so we can predict
#   occurrence probabilities along a gradient
temp_seq <- seq(min(data_set$temperature_std), max(data_set$temperature_std), length = n_plot)

# now we make a new data.frame, with the predictor sequence add for the variable
#   of interest, and zeros otherwise (holding all other predictors at their mean)
plot_data <- data.frame(
  temperature_std = temp_seq,
  precipitation_std = rep(0, n_plot),
  tree_cover_std = rep(0, n_plot),
  flowering_std = rep(0, n_plot)
)

# we can use this new data.frame to predict the probability of occurrence at
#   each value along the predictor sequence
plot_values <- predict(model_sp1, newdata = plot_data, type = "response")

# now plot it
plot(plot_values ~ temp_seq, type = "l")

# there's a few things we might want to fix with this plot:
#   1. the x-axis has the standardised values but ideally would have the true temperature values
#   2. we could add some informative axis labels
#   3. we could make some formatting changes based on our own preferences

# change the axis range
temp_actual <- temp_seq * sd(data_set$temperature_c) + mean(data_set$temperature_c)
plot(plot_values ~ temp_actual, type = "l")

# change the axis labels
plot(plot_values ~ temp_actual,
     type = "l",
     xlab = "Temperature (C)",
     ylab = "Probabiliy of occurrence")

# other random things we can change
plot(plot_values ~ temp_actual,
     type = "l",
     bty = "l",
     las = 1,
     lwd = 2.5,
     ylim = c(0, 1),
     xlab = "Temperature (C)",
     ylab = "Probabiliy of occurrence")

# could even add a label to the plot
mtext("Species 1", side = 3, line = 0.5, adj = 0.02, cex = 1.5)

# that was easy enough. But now we want to repeat this processs for other
#   predictors and species

# let's start with precipitation for species 1
precip_seq <- seq(min(data_set$precipitation_std), max(data_set$precipitation_std), length = n_plot)
plot_data <- data.frame(
  temperature_std = rep(0, n_plot),
  precipitation_std = precip_seq,
  tree_cover_std = rep(0, n_plot),
  flowering_std = rep(0, n_plot)
)
plot_values <- predict(model_sp1, newdata = plot_data, type = "response")
precip_actual <- precip_seq * sd(data_set$precipitation_mm) + mean(data_set$precipitation_mm)
plot(plot_values ~ precip_actual,
     type = "l",
     bty = "l",
     las = 1,
     lwd = 2.5,
     ylim = c(0, 1),
     xlab = "Precipitation (mm)",
     ylab = "Probabiliy of occurrence")

# could even add a label to the plot
mtext("Species 1", side = 3, line = 0.5, adj = 0.02, cex = 1.5)

# what about species 2?
# let's start with precipitation for species 1
precip_seq <- seq(min(data_set$precipitation_std), max(data_set$precipitation_std), length = n_plot)
plot_data <- data.frame(
  temperature_std = rep(0, n_plot),
  precipitation_std = precip_seq,
  tree_cover_std = rep(0, n_plot),
  flowering_std = rep(0, n_plot)
)
plot_values <- predict(model_sp2, newdata = plot_data, type = "response")
precip_actual <- precip_seq * sd(data_set$precipitation_mm) + mean(data_set$precipitation_mm)
plot(plot_values ~ precip_actual,
     type = "l",
     bty = "l",
     las = 1,
     lwd = 2.5,
     ylim = c(0, 1),
     xlab = "Precipitation (mm)",
     ylab = "Probabiliy of occurrence")

# could even add a label to the plot
mtext("Species 2", side = 3, line = 0.5, adj = 0.02, cex = 1.5)

# great, but this seems like a tedious amount of copy-paste if we want to plot
#   three species by four predictors. What if we had more species?
