library(bsts)
library(MacrobondAPI)

unemp <- FetchOneTimeSeries("uslama3294")
if (getIsError(unemp)) 
  stop(getErrorMessage(unemp))

unemp <- (as.xts(unemp) / 1000) # in thousands
unemp <- window(unemp, start = as.Date("2004-01-01"))

ss <- AddLocalLinearTrend(list(), unemp)
ss <- AddSeasonal(ss, unemp, nseasons=52)
model1 <- bsts(unemp, state.specification = ss, niter=500)

plot(model1, "comp")

data(iclaims)
initial.claims$michigan.unemployment

library(gtrendsR)
library(dplyr)
library(ggplot2)
search_terms <- c(
  "unemployment ny", "unemployment nj", "unemployment texas", "unemployment california"
)
trends <- gtrends(keyword=search_terms, geo="US",
                  gprop="web", time="2004-01-05 2024-08-10", onlyInterest = T)
trends %>% .$interest_over_time %>% glimpse()

trends %>% .$interest_over_time %>% ggplot(aes(x=date, y=hits)) + facet_wrap(~keyword) +
  geom_line(color="darkblue", linewidth=1.5)

trends$interest_over_time
