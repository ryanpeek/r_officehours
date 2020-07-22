# brms

# see the full post/rmd here:
# - https://calogica.com/r/rstan/2020/07/05/season-pass-hierarchical-modelng-r-stan-brms.html
# - https://github.com/clausherther/rstan/blob/master/hierarchical_modelng_r_stan_brms_season_pass.Rmd


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(broom)
library(janitor)
library(brms)
library(bayesplot)
library(hrbrthemes)
theme_set(hrbrthemes::theme_ipsum_rc())
bayesplot_theme_set(hrbrthemes::theme_ipsum_tw())


# Get Data ----------------------------------------------------------------

season_pass_data <- readr::read_csv("http://goo.gl/J8MH6A")

# save data for future use:
write_csv(season_pass_data, "data/season_pass_data.csv")


# Wrangle -----------------------------------------------------------------

# add 1 count n, add factor columns for promo and channel. We’ll also convert the Pass variable to a Bernoulli style outcome variable of 0s and 1s
season_pass_data <- season_pass_data %>%
  mutate(n = 1,
         bought_pass = case_when(Pass == "YesPass" ~ 1, TRUE ~ 0),
         promo = factor(Promo, levels = c("NoBundle", "Bundle")),
         channel = factor(Channel, levels = c("Mail", "Park", "Email"))
  )

# look at contrasts
contrasts(season_pass_data$promo)
contrasts(season_pass_data$channel)


## Convert Bernoulli to Binomial Data
season_pass_data_grp <- season_pass_data %>% 
  group_by(promo, channel) %>%
  summarise(bought_pass = sum(bought_pass), 
            n = sum(n)) %>%
  ungroup() 

season_pass_data_grp


# Visualize ---------------------------------------------------------------


## Customers by channel
season_pass_data_grp %>%
  select(channel, promo, bought_pass) %>%
  pivot_wider(names_from = promo, values_from = bought_pass) %>%
  adorn_totals("col") 

season_pass_data_grp %>% 
  ggplot(aes(x = channel, y = bought_pass, group = promo, fill = promo)) +
  geom_col() + 
  scale_y_continuous() +
  scale_fill_ft() +
  labs(x = "",
       y = "# Bought Season Pass",
       title = "Customers by Channel",
       subtitle = "by Promotion (Bundle/NoBundle)"
  )

## % of customers by season pass/channel
season_pass_data_grp %>% 
  group_by(channel) %>%
  summarise(bought_pass = sum(bought_pass), 
            n = sum(n),
            percent_bought = bought_pass/n) %>%
  ggplot(aes(x = channel, 
             y = percent_bought, 
             fill = channel, 
             label = scales::percent(percent_bought))) + 
  geom_col(width = .5) + 
  coord_flip() +
  theme(legend.position = "none") +
  geom_text(hjust = "outward", nudge_y=.01, color="Black") + 
  scale_fill_ft() +
  scale_y_continuous(labels = NULL) +
  labs(x = "",
       y = "% Bought Season Pass by Channel",
       title = "% of Customers by Channel"
  )


## Bundle Customers by Channel 

season_pass_data_grp %>%
  select(channel, promo, bought_pass) %>%
  pivot_wider(names_from = promo, values_from = bought_pass) %>%
  mutate(percent_bundle = Bundle/(NoBundle + Bundle)) -> season_pass_data_grp_pct_bundle

season_pass_data_grp_pct_bundle

season_pass_data_grp_pct_bundle %>% 
  ggplot(aes(x = channel, 
             y = percent_bundle, 
             fill = channel, 
             label = scales::percent(percent_bundle)
  )
  ) +
  geom_col(width = .5) + 
  coord_flip() +
  theme(legend.position = "none") +
  geom_text(hjust = "outward", nudge_y=.01, color="Black") + 
  scale_y_continuous(labels = NULL) +
  scale_fill_ft() +
  labs(x = "",
       y = "% Bought Season Pass w/Bundle",
       title = "% of Bundle Customers by Channel"
  )



# Baseline Model ----------------------------------------------------------

library(ggdist)

# iterations to use for MCMC sampling
iter <- 10000

# seed
set.seed(42)

draws <- 1000
norm_df <- as_tibble(data.frame(sd_1 = rnorm(draws, mean = 0, sd = 1),
                                sd_2 = rnorm(draws, mean = 0, sd = 2),
                                sd_5 = rnorm(draws, mean = 0, sd = 5))) %>%
  pivot_longer(cols = c(sd_1, sd_2, sd_5), names_to = "prior", values_to = "samples")

ggplot(norm_df, aes(y = fct_rev(prior), x=samples, fill = stat(abs(x) < 2.5))) + 
  stat_halfeye() +
  scale_fill_manual(values = c("gray80", "skyblue")) +
  # scale_x_continuous(breaks=seq(-10,10,1)) + 
  labs(title = "Normal distribution priors",
       x = "log-odds",
       y  = "stdev")

# MODEL
base_line_promo_model <- brm(bought_pass | trials(n) ~ 1 + promo,
                             prior = c(prior(normal(0, 1), class = Intercept),
                                       prior(normal(0, 1), class = b)),
                             data = season_pass_data_grp,
                             family = binomial(link = "logit"),
                             iter = iter
)

# traceplot
mcmc_trace(base_line_promo_model, regex_pars = c("b_"), facet_args = list(nrow = 2))

# summarize
summary(base_line_promo_model)

# plot
mcmc_areas(
  base_line_promo_model,
  regex_pars = "b_",
  prob = 0.95, 
  point_est = "median",
  area_method = "equal height"
) +
  geom_vline(xintercept = 0, color = "red", alpha = 0.6, lwd = .8, linetype = "dashed") +
  labs(
    title = "Effect of Bundle Promotion on Sales"
  )


## The slope coefficient promoBundle is positive and does not contain 0 in the uncertainty interval. The value of 0.39 represents the effect of the Bundle treatment in terms of log-odds, i.e. bundling increases the log odds of buying a season pass by 0.39. We can convert that to a % by exponentiating the coefficients (which we get via fixef) to get the % increase of the odds:

exp(fixef(base_line_promo_model))


# In terms of percent change, we can say that the odds of a customer buying a season pass when offered the bundle are 47% higher than if they’re not offered the bundle.

# LOG ODDS: Log-odds, as the name implies are the logged odds of an outcome. For example, an outcome with odds of 4:1, i.e. a probability of 80% (4/(4+1)) has log-odds of log(4/1) = 1.386294

season_pass_data %>% 
  group_by(promo) %>%
  summarise(bought_pass = sum(bought_pass),
            did_not_buy = sum(n) - sum(bought_pass)) %>%
  adorn_totals(c("row", "col"), name="total") %>%
  mutate(percent_bought = bought_pass/total)

# We estimated an intercept of -0.19, which are the log-odds for NoBundle (the baseline). We observed 670 of 1,482 customers that were not offered the bundle bought a season pass vs 812 that didn’t buy. With odds defined as bought/didn’t buy, the log of the NoBundle buy odds is

odds_no_bundle <- 670/812
log(odds_no_bundle)

# While our estimated slope of 0.39 for Bundle is the log of the ratio of buy/didn’t buy odds for Bundle vs NoBundle
(odds_no_bundle <- 670/812)
(odds_bundle <- 919/755)
log(odds_bundle/odds_no_bundle)


exp(fixef(base_line_promo_model))

# We can think of 1.47 as the odds ratio of Bundle vs NoBundle, where ratio of 1 would indicate no improvement.

# using logistic regression we could arrive at same answer:
newdata <- data.frame(promo = factor(c("NoBundle", "Bundle")), n = 1)

predict(base_line_promo_model, newdata)[c(1:2)]



# Add Model Interaction ---------------------------------------------------

# Create both individual slopes for each variable, as well as the interaction term
promo_channel_model_interactions <- brm(bought_pass | trials(n) ~ promo*channel, 
                                        prior = c(prior(normal(0, 1), class = Intercept),
                                                  prior(normal(0, 1), class = b)),
                                        data = season_pass_data_grp,
                                        family = binomial(link = "logit"),
                                        iter = iter)

mcmc_trace(promo_channel_model_interactions, regex_pars = c("b_"), facet_args = list(nrow = 3))

# plot
mcmc_areas(
  promo_channel_model_interactions,
  regex_pars = "b_",
  prob = 0.95, # 80% intervals
  prob_outer = 1, # 99%
  point_est = "median",
  area_method = "equal height"
) +
  geom_vline(xintercept = 0, color = "red", alpha = 0.6, lwd = .8, linetype = "dashed") +
  labs(
    title = "Effect of Channel and Bundle Promotion",
    subtitle = "with interactions"
  )

# the Email channel is associated with a -2.93 decrease in log odds of selling a season pass (vs the baseline channel Mail )
# however, the interaction term promoBundle:channelEmail, i.e. the effect of the Bundle promo given the Email channel shows a ~2.6x increase in log-odds over the baseline
# interestingly, the Park channel does not seem to meaningfully benefit from offering a bundle promotion, shown by the fact that its posterior uncertainty interval spans 0


# MULTILEVEL MODELS -------------------------------------------------------

# multilevel models. They offer both the ability to model interactions (and deal with the dreaded collinearity of model parameters) and a built-in way to regularize our coefficient to minimize the impact of outliers and, thus, prevent overfitting.

# model this with both varying intercepts and slopes, since we observed that the different channels appear to have overall lower baselines (arguing for varying intercepts) and also show different effects of offering the bundle promotion (arguing for varying slopes).

# We will model both a varying intercept (1) and varying slope (promo) by channel, removing the standard population level intercept (0) and slope. (The || tells brms not to bother to compute correlations.)

promo_channel_model_hierarchical <- brm(bought_pass | trials(n) ~ 0 + (1 + promo || channel),
                                        prior = c(prior(normal(0, 1), class = sd)),
                                        data = season_pass_data_grp,
                                        control = list(adapt_delta = 0.95),
                                        family = binomial(link = "logit"),
                                        iter = iter
)


summary(promo_channel_model_hierarchical)

# summarize
library(broom.mixed)

parnames(promo_channel_model_hierarchical)

tidy(promo_channel_model_hierarchical,parameters = c("^r_chann", "^sd"), prob = 0.95)

# From the output above, we can see that Email in general is still performing worse vs the other channels judging from its low negative coefficient, while the effect of the Bundle promo for the Email channel is positive at ~2 increase in log-odds. However, compared to our single-level interaction models, we see that the hierarchical model did a better job constraining the estimate of the effect of offering the bundle in emails by shrinking the estimate a bit towards the group mean

# visualize

mcmc_areas(
  promo_channel_model_hierarchical,
  regex_pars = "r_channel",
  prob = 0.8, # 80% intervals
  point_est = "median",
  area_method = "equal height"
) +
  geom_vline(xintercept = 0, color = "red", alpha = 0.6, lwd = .8, linetype = "dashed") +
  labs(
    title = "Effect of Channel and Bundle Promotion",
    subtitle = "hierarchical model: random intercept and slope"
  )

# summary: shows email response and take rates are the lowest of all channels, we can confidently tell our marketing partners that offering bundling via email has a positive effect that is worth studying more and gathering more data

newdata_channel <- data.frame(promo = factor(c("Bundle", "Bundle")), 
                              channel = factor(c("Email", "Park")), n = 1)

predict(promo_channel_model_interactions, newdata_channel)

predict(promo_channel_model_hierarchical, newdata_channel)

# advantage for the hierarchical model in this case really comes from the ability to regularize the model more efficiently, and to be able to more easily interpret the coefficients. In more complex modeling challenges, hierarchical models really shine when there are more than one and/or nested grouping levels (hence “hierarchical”).