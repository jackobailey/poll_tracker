# Poll Tracker

# Jack Bailey
# The University of Manchester
# jack.bailey@manchester.ac.uk
# @PoliSciJack


# 1. Housekeeping ---------------------------------------------------------

# Load packages

library(tidyverse)
library(patchwork)
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
# and third tables on the page which includes all of the polls since the last
# election (i.e. for 2020 and 2021). We'll append the year to each date and
# then rbind() them together.

dta_2021 <-
  htmltab(doc = url, which = 2) %>%
  mutate(Datesconducted = paste(Datesconducted, "2021"))

dta_2020 <-
  htmltab(doc = url, which = 3) %>%
  mutate(Datesconducted = paste(Datesconducted, "2020"))

dta <- rbind(dta_2021, dta_2020)


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
    # snp = SNP,
    # grn = Green
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
      str_remove(".*(–|-)") %>%
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

# dta[names(dta) %in% c("con", "lab", "lib", "snp", "grn")] <-
#   dta[names(dta) %in% c("con", "lab", "lib", "snp", "grn")] %>%
#   mutate_all(function(x) as.numeric(str_remove(x, "%"))/100)

dta[names(dta) %in% c("con", "lab", "lib")] <-
  dta[names(dta) %in% c("con", "lab", "lib")] %>%
  mutate_all(function(x) as.numeric(str_remove(x, "%"))/100)


# We also need to create a generic "Other" category that accounts for any other
# parties in Great Britain that voters might choose to support.

# dta <-
#   dta %>%
#   mutate(oth = 1 - (con + lab + lib + snp + grn))

dta <-
  dta %>%
  mutate(oth = 1 - (con + lab + lib))


# Finally, we'll create a column of simplexes that include each poll's figures.

# dta <-
#   dta %>%
#   mutate(
#     outcome = as.matrix(dta[names(dta) %in% c("con", "lab", "lib", "snp", "grn", "oth")])
#   )

dta <-
  dta %>%
  mutate(
    outcome = as.matrix(dta[names(dta) %in% c("con", "lab", "lib", "oth")])
  )



# 3. Fit model ------------------------------------------------------------

# Now that we have our data, we can fit our model.

# m1 <-
#   brm(formula = bf(outcome ~ 1 + gb + s(time, k = 10) + (1 | pollster)),
#       family = dirichlet(link = "logit", refcat = "oth"),
#       prior =
#         prior(normal(0, 1.5), class = "Intercept", dpar = "mucon") +
#         prior(normal(0, 0.5), class = "b", dpar = "mucon") +
#         prior(exponential(2), class = "sd", dpar = "mucon") +
#         prior(exponential(2), class = "sds", dpar = "mucon") +
#         prior(normal(0, 1.5), class = "Intercept", dpar = "mugrn") +
#         prior(normal(0, 0.5), class = "b", dpar = "mugrn") +
#         prior(exponential(2), class = "sd", dpar = "mugrn") +
#         prior(exponential(2), class = "sds", dpar = "mugrn") +
#         prior(normal(0, 1.5), class = "Intercept", dpar = "mulab") +
#         prior(normal(0, 0.5), class = "b", dpar = "mulab") +
#         prior(exponential(2), class = "sd", dpar = "mulab") +
#         prior(exponential(2), class = "sds", dpar = "mulab") +
#         prior(normal(0, 1.5), class = "Intercept", dpar = "mulib") +
#         prior(normal(0, 0.5), class = "b", dpar = "mulib") +
#         prior(exponential(2), class = "sd", dpar = "mulib") +
#         prior(exponential(2), class = "sds", dpar = "mulib") +
#         prior(normal(0, 1.5), class = "Intercept", dpar = "musnp") +
#         prior(normal(0, 0.5), class = "b", dpar = "musnp") +
#         prior(exponential(2), class = "sd", dpar = "musnp") +
#         prior(exponential(2), class = "sds", dpar = "musnp") +
#         prior(gamma(1, 0.01), class = "phi"),
#       data = dta,
#       seed = 666,
#       iter = 2e3,
#       chains = 4,
#       cores = 4,
#       refresh = 5,
#       control =
#         list(
#           adapt_delta = .95,
#           max_treedepth = 15
#         ),
#       file = here("_output", paste0("model", "-", Sys.Date()))
#   )

m1 <-
  brm(formula = bf(outcome ~ 1 + gb + s(time, k = 10) + (1 | pollster)),
      family = dirichlet(link = "logit", refcat = "oth"),
      prior =
        prior(normal(0, 1.5), class = "Intercept", dpar = "mucon") +
        prior(normal(0, 0.5), class = "b", dpar = "mucon") +
        prior(exponential(2), class = "sd", dpar = "mucon") +
        prior(exponential(2), class = "sds", dpar = "mucon") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "mulab") +
        prior(normal(0, 0.5), class = "b", dpar = "mulab") +
        prior(exponential(2), class = "sd", dpar = "mulab") +
        prior(exponential(2), class = "sds", dpar = "mulab") +
        prior(normal(0, 1.5), class = "Intercept", dpar = "mulib") +
        prior(normal(0, 0.5), class = "b", dpar = "mulib") +
        prior(exponential(2), class = "sd", dpar = "mulib") +
        prior(exponential(2), class = "sds", dpar = "mulib") +
        prior(gamma(1, 0.01), class = "phi"),
      backend = "cmdstanr",
      data = dta,
      seed = 666,
      iter = 2e3,
      chains = 2,
      cores = 2,
      threads = threading(2),
      refresh = 5,
      adapt_delta = .95,
      max_treedepth = 15,
      file = here("_output", paste0("model", "-", Sys.Date()))
  )



# 4. Plot voting intention over time --------------------------------------

# Convert today's date to numeric

today <- interval("2019-12-12", Sys.Date())/years(1)


# Create empty data set the same length as the real data

pred_dta <-
  tibble(
    time = seq(0, today, length.out = nrow(dta)),
    date = as.Date(time*365, origin = "2019-12-12"),
    gb = "GB"
  )


# Compute predicted probabilities

pred_dta <-
  add_fitted_draws(
    model = m1,
    newdata = pred_dta,
    re_formula = NA
  ) %>%
  group_by(date, .category) %>%
  summarise(
    est = median(.value),
    lower = quantile(.value, probs = .05),
    upper = quantile(.value, probs = .95),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  rename(party = .category)


# Format data so that the party variable takes the full party names

# pred_dta <-
#   pred_dta %>%
#   mutate(
#     party =
#       party %>%
#       factor(
#         levels = c("con", "lab", "lib", "snp", "grn", "oth"),
#         labels =
#           c(
#             "Conservative",
#             "Labour",
#             "Lib Dems",
#             "SNP",
#             "Green",
#             "Other"
#           )
#       )
#   )

pred_dta <-
  pred_dta %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("con", "lab", "lib", "oth"),
        labels =
          c(
            "Conservative",
            "Labour",
            "Lib Dems",
            "Other"
          )
      )
  )


# Get final predictions for labels

pred_end <-
  pred_dta %>%
  filter(date == max(date)) %>%
  mutate(
    label = scales::percent(est, accuracy = 1)
  )


# Convert raw data to long-format

# point_dta <-
#   dta[names(dta) %in% c("date", "con", "lab", "lib", "snp", "grn")] %>%
#   pivot_longer(
#     cols = -date,
#     names_to = "party",
#     values_to = "est"
#   ) %>%
#   mutate(
#     party =
#       party %>%
#       factor(
#         levels = c("con", "lab", "lib", "snp", "grn", "oth"),
#         labels =
#           c(
#             "Conservative",
#             "Labour",
#             "Lib Dems",
#             "SNP",
#             "Green",
#             "Other"
#           )
#       )
#   )

point_dta <-
  dta[names(dta) %in% c("date", "con", "lab", "lib", "oth")] %>%
  pivot_longer(
    cols = -date,
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party =
      party %>%
      factor(
        levels = c("con", "lab", "lib", "oth"),
        labels =
          c(
            "Conservative",
            "Labour",
            "Lib Dems",
            "Other"
          )
      )
  )


# Plot

# poll_plot <-
#   ggplot() +
#   geom_point(data =
#                point_dta %>%
#                filter(party != "Other"),
#              aes(x = date,
#                  y = est,
#                  colour = party,
#                  fill = party),
#              alpha = .3,
#              size = 1) +
#   geom_ribbon(data =
#                 pred_dta %>%
#                 filter(party != "Other"),
#               aes(x = date,
#                   y = est,
#                   ymin = lower,
#                   ymax = upper,
#                   colour = party,
#                   fill = party),
#               alpha = .3,
#               colour = NA) +
#   geom_line(data =
#               pred_dta %>%
#               filter(party != "Other"),
#             aes(x = date,
#                 y = est,
#                 colour = party),
#             size = 1) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_x_date(date_breaks = "1 month",
#                date_labels = "%b") +
#   coord_cartesian(xlim = c(min(dta$date), max(dta$date)),
#                   ylim = c(0, .6)) +
#   scale_fill_manual(values = c("#0087DC",
#                                "#DC241f",
#                                "#FAA61A",
#                                "#E8DD67",
#                                "#6AB023")) +
#   scale_color_manual(values = c("#0087DC",
#                                 "#DC241f",
#                                 "#FAA61A",
#                                 "#E8DD67",
#                                 "#6AB023")) +
#   labs(title = paste("UK Poll of Polls"),
#        subtitle = format(Sys.Date(), "%d %b %Y"),
#        caption = "",
#        y = "",
#        x = "") +
#   theme_bailey() +
#   theme(legend.position = "none")

poll_plot <-
  ggplot() +
  geom_point(data =
               point_dta %>%
               filter(party != "Other"),
             aes(x = date,
                 y = est,
                 colour = party,
                 fill = party),
             alpha = .3,
             size = 1) +
  geom_ribbon(data =
                pred_dta %>%
                filter(party != "Other"),
              aes(x = date,
                  y = est,
                  ymin = lower,
                  ymax = upper,
                  colour = party,
                  fill = party),
              alpha = .3,
              colour = NA) +
  geom_line(data =
              pred_dta %>%
              filter(party != "Other"),
            aes(x = date,
                y = est,
                colour = party),
            size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  coord_cartesian(xlim = c(min(dta$date), max(dta$date)),
                  ylim = c(0, .6)) +
  scale_fill_manual(values = c("#0087DC",
                               "#DC241f",
                               "#FAA61A")) +
  scale_color_manual(values = c("#0087DC",
                                "#DC241f",
                                "#FAA61A")) +
  labs(title = paste("UK Poll of Polls"),
       subtitle = format(Sys.Date(), "%d %b %Y"),
       caption = "",
       y = "",
       x = "") +
  theme_bailey() +
  theme(legend.position = "none")



# 5. Plot predicted vote intention ----------------------------------------

# Get predicted probabilities now and order by size

# vi_pred <-
#   add_fitted_draws(
#     model = m1,
#     newdata =
#       tibble(time = today,
#              gb = "GB"),
#     re_formula = NA
#   ) %>%
#   group_by(.category) %>%
#   summarise(
#     est = median(.value),
#     lower = quantile(.value, probs = .05),
#     upper = quantile(.value, probs = .95),
#     .groups = "drop"
#   ) %>%
#   ungroup() %>%
#   rename(party = .category) %>%
#   arrange(desc(est)) %>%
#   mutate(
#     party =
#       party %>%
#       factor(
#         levels =
#           c("con",
#             "lab",
#             "lib",
#             "snp",
#             "grn",
#             "oth"),
#         labels =
#           c("Conservative",
#             "Labour",
#             "Lib Dem",
#             "SNP",
#             "Green",
#             "Other")
#       ),
#     col =
#       case_when(
#         party == "Conservative" ~ "#0087DC",
#         party == "Labour" ~ "#DC241f",
#         party == "Lib Dem" ~ "#FAA61A",
#         party == "SNP" ~ "#E8DD67",
#         party == "Green" ~ "#6AB023"
#       )
#   ) %>%
#   mutate(
#     label = scales::percent(est, accuracy = 1)
#   )

vi_pred <-
  add_fitted_draws(
    model = m1,
    newdata =
      tibble(time = today,
             gb = "GB"),
    re_formula = NA
  ) %>%
  group_by(.category) %>%
  summarise(
    est = median(.value),
    lower = quantile(.value, probs = .05),
    upper = quantile(.value, probs = .95),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  rename(party = .category) %>%
  arrange(desc(est)) %>%
  mutate(
    party =
      party %>%
      factor(
        levels =
          c("con",
            "lab",
            "lib",
            "oth"),
        labels =
          c("Con",
            "Lab",
            "LD",
            "Oth")
      ),
    col =
      case_when(
        party == "Con" ~ "#0087DC",
        party == "Lab" ~ "#DC241f",
        party == "LD" ~ "#FAA61A"
      )
  ) %>%
  mutate(
    label = scales::percent(est, accuracy = 1)
  )


# Order parties by predicted vote share

vi_pred <-
  vi_pred %>%
  mutate(
    party =
      party %>%
      factor(levels = party)
  )


# Plot

# vi_plot <-
#   vi_pred %>%
#   filter(party != "Other") %>%
#   ggplot(aes(x = party,
#              y = est,
#              ymin = lower,
#              ymax = upper,
#              colour = party,
#              label = label)) +
#   geom_interval(size = 1,
#                 alpha = .4) +
#   geom_point(size = 2) +
#   geom_text(color = "black",
#             nudge_x = .3,
#             family = "Cabin",
#             size = 2.2) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_color_manual(values = vi_pred$col[vi_pred$party != "Other"]) +
#   coord_cartesian(ylim = c(0, .6)) +
#   labs(title = paste("UK Poll of Polls"),
#        subtitle = format(Sys.Date(), "%d %b %Y"),
#        caption = "@PoliSciJack",
#        y = "",
#        x = "") +
#   theme_bailey() +
#   theme(
#     legend.position = "none",
#     plot.title = element_text(colour = "white"),
#     plot.subtitle = element_text(colour = "white"))

vi_plot <-
  vi_pred %>%
  filter(party != "Oth") %>%
  ggplot(aes(x = party,
             y = est,
             ymin = lower,
             ymax = upper,
             colour = party,
             label = label)) +
  geom_interval(size = 1,
                alpha = .4) +
  geom_point(size = 2) +
  geom_text(color = "black",
            nudge_x = .3,
            family = "Cabin",
            size = 2.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = vi_pred$col[vi_pred$party != "Oth"]) +
  coord_cartesian(ylim = c(0, .6)) +
  labs(title = paste("UK Poll of Polls"),
       subtitle = format(Sys.Date(), "%d %b %Y"),
       caption = "@PoliSciJack",
       y = "",
       x = "") +
  theme_bailey() +
  theme(
    legend.position = "none",
    plot.title = element_text(colour = "white"),
    plot.subtitle = element_text(colour = "white"))



# 6. Render plot graphic --------------------------------------------------

# Display plots

poll_plot
vi_plot


# Save plot to disk

png(filename = here("_output", "polls.png"),
    res = 72*8,
    width = 600*8,
    height = 335*8)

poll_plot + vi_plot

dev.off()


# Remove "Other"

vi_pred <- vi_pred %>% filter(party != "Oth")


# Save text summary to disk

sink(here("_output", "tweet.txt"))
# cat(
#   paste("UK Poll of Polls,", format(Sys.Date(), "%d %b %Y")),
#   paste0(
#     "\n\n",
#     vi_pred$party[1], ": ", vi_pred$label[1], " (", scales::percent(vi_pred$lower[1]), "-", scales::percent(vi_pred$upper[1]), ")\n",
#     vi_pred$party[2], ": ", vi_pred$label[2], " (", scales::percent(vi_pred$lower[2]), "-", scales::percent(vi_pred$upper[2]), ")\n",
#     vi_pred$party[3], ": ", vi_pred$label[3], " (", scales::percent(vi_pred$lower[3]), "-", scales::percent(vi_pred$upper[3]), ")\n",
#     vi_pred$party[4], ": ", vi_pred$label[4], " (", scales::percent(vi_pred$lower[4]), "-", scales::percent(vi_pred$upper[4]), ")\n",
#     vi_pred$party[5], ": ", vi_pred$label[5], " (", scales::percent(vi_pred$lower[5]), "-", scales::percent(vi_pred$upper[5]), ")"
#   )
# )
cat(
  paste("UK Poll of Polls,", format(Sys.Date(), "%d %b %Y")),
  paste0(
    "\n\n",
    vi_pred$party[1], ": ", vi_pred$label[1], " (", scales::percent(vi_pred$lower[1]), "-", scales::percent(vi_pred$upper[1]), ")\n",
    vi_pred$party[2], ": ", vi_pred$label[2], " (", scales::percent(vi_pred$lower[2]), "-", scales::percent(vi_pred$upper[2]), ")\n",
    vi_pred$party[3], ": ", vi_pred$label[3], " (", scales::percent(vi_pred$lower[3]), "-", scales::percent(vi_pred$upper[3]), ")\n"
  )
)
sink()



# 7. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", paste0("session-info-", Sys.Date(), ".txt")))


# One more thing...

thanks()

