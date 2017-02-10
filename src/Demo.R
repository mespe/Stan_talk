library(rstanarm)
library(rstan)
library(shinystan)
library(loo)
load('~/Ag_projects/breeding_analysis/data/mod_data.Rda')

i <- mod_data$id %in% c("M202","M104","M401")
mod_data <- mod_data[i,]

n.level <- function(x){
    length(unique(x))
}
to.idx <- function(x){
    as.integer(factor(x))
}

dd <- with(mod_data, list(N = nrow(mod_data),
           n_cult = n.level(id),
           n_site = n.level(site),
           n_year = n.level(year),
           n_yr_site = n.level(yr_site_fact),
           n_var = 4L,
           cult = to.idx(id),
           site = to.idx(site),
           year = to.idx(year),
           siteyr = to.idx(yr_site_fact),
           yield = yield_lb))


stan.fit <- stan_glmer(yield_cs ~ (1|site) + (1|id) + (1|year) + (1|yr_site_fact),
                       data = mod_data, cores = 2L, iter = 200)

launch_shinystan(stan.fit)


## Let's write our own

my_mod <- stan_model("Demo.stan")

fit <- sampling(my_mod, data = dd, iter = 200, chains = 4L, cores = 2L)

summary(stan.fit, regex_pars = " id", digits = 2)
print(fit, pars = "b_cult")

loo(stan.fit)
loo.after <- loo(extract(fit, pars = "log_lik")$log_lik)
loo.before <- loo(extract(fit, pars = "log_lik")$log_lik)

compare(loo.after, loo.before)


## Prepping data for cmdstan

stan_rdump(file = "demo.rdump", list = names(dd), envir= as.environment(dd))




