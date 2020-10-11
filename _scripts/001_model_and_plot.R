# Poll Tracker

# Jack Bailey
# The University of Manchester
# jack.bailey@manchester.ac.uk
# @PoliSciJack


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(tidyverse)
library(tidybayes)
library(lubridate)
library(htmltab)
library(jbmisc)
library(brms)
library(here)


# Set random seed

set.seed(666)



# 2. Scrape polling data --------------------------------------------------

# We'll get the data we need from Wikipedia. This is useful because there
# are a whole host of people who keep these pages up to date and it means
# that we don't have to manage data collection ourselves.

url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_United_Kingdom_general_election"


# Now, we'll use the htmltab() function to scrape the contents of the second
# table on the page which includes all of the polls since the last election.

dta <- htmltab(doc = url, which = 2)


# Next, we'll select the variables that we need, rename them too, and save
# the resulting object as a tibble.

dta <-
  dta %>%
  select(
    date = Datesconducted,
    pollster = Pollster,
    gb = Area,
    con = Con,
    lab = Lab,
    lib = `Lib Dem`,
    snp = SNP,
    grn = Green
  ) %>%
  tibble()


# At the moment, the data contain rows that include only information on the
# events that came to pass in UK politics. We don't need these, so we will
# remove them from the data.

dta <-
  dta %>%
  filter(!(gb == con))


# We'll also remove the actual election results themselves.

dta <-
  dta %>%
  filter(!(pollster == "2019 general election"))


# The next step is to recode the date variable so that R knows that it is a
# date. We'll pick the last date that the poll was in the field.

dta <-
  dta %>%
  mutate(
    date =
      date %>%
      str_remove(".*–") %>%
      paste(., "2020") %>%
      dmy()
  )


# The model requires a numeric time tracking variable, which we'll create by
# converting the date to a number indexed to the date of the election that
# counts up in years.

dta <-
  dta %>%
  mutate(time = interval("2019-12-12", date)/years(1))


# The pollster date all looks good, except that "Kantar" and "Kantar Public"
# are included as separate organisations. We'll address that here.

dta <-
  dta %>%
  mutate(
    pollster =
      pollster %>%
      str_remove(" Public")
  )


# We'll also need to set "GB" as the reference category in for the variable
# that tracks what geography the poll used.

dta <-
  dta %>%
  mutate(gb = factor(gb, levels = c("GB", "UK")))


# Now, we need to convert all of the party figures so that they are scaled as
# proportions rather than percentages.

dta[names(dta) %in% c("con", "lab", "lib", "snp", "grn")] <-
  dta[names(dta) %in% c("con", "lab", "lib", "snp", "grn")] %>%
  mutate_all(function(x) as.numeric(str_remove(x, "%"))/100)


# We also need to create a generic "Other" category that accounts for any other
# parties in Great Britain that voters might choose to support.

dta <-
  dta %>%
  mutate(oth = 1 - (con + lab + lib + snp + grn))


# Finally, we'll create a column of simplexes that include each poll's figures.

dta <-
  dta %>%
  mutate(
    outcome = as.matrix(dta[names(dta) %in% c("con", "lab", "lib", "snp", "grn", "oth")])
  )



# 3. Fit model ------------------------------------------------------------

# Now that we have our data, we can fit our model.

m1 <-
  brm(formula = bf(outcome ~ 1 + gb + s(time, k = 10) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "oth"),
      prior =
        prior(normal(-1, .5), class = "Intercept", dpar = "mucon") +
        prior(normal(0, 0.5), class = "b", dpar = "mucon") +
        prior(exponential(2), class = "sd", dpar = "mucon") +
        prior(exponential(2), class = "sds", dpar = "mucon") +
        prior(normal(-1, .5), class = "Intercept", dpar = "mugrn") +
        prior(normal(0, 0.5), class = "b", dpar = "mugrn") +
        prior(exponential(2), class = "sd", dpar = "mugrn") +
        prior(exponential(2), class = "sds", dpar = "mugrn") +
        prior(normal(-1, .5), class = "Intercept", dpar = "mulab") +
        prior(normal(0, 0.5), class = "b", dpar = "mulab") +
        prior(exponential(2), class = "sd", dpar = "mulab") +
        prior(exponential(2), class = "sds", dpar = "mulab") +
        prior(normal(-1, .5), class = "Intercept", dpar = "mulib") +
        prior(normal(0, 0.5), class = "b", dpar = "mulib") +
        prior(exponential(2), class = "sd", dpar = "mulib") +
        prior(exponential(2), class = "sds", dpar = "mulib") +
        prior(normal(-1, .5), class = "Intercept", dpar = "musnp") +
        prior(normal(0, 0.5), class = "b", dpar = "musnp") +
        prior(exponential(2), class = "sd", dpar = "musnp") +
        prior(exponential(2), class = "sds", dpar = "musnp"),
      data = dta,
      backend = "cmdstanr",
      iter = 2e3,
      chains = 2,
      cores = 2,
      threads = threading(2),
      refresh = 5,
      file = here("_output", paste0("model", "-", as.numeric(Sys.Date())))
  )

