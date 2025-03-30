#######################
# HW9
#######################
# load libraries
#######################
library(tidyverse)
library(nleqslv)

data <- read_csv("agacis.csv")

# clean the data
dat.precip.long <- data |>    
  dplyr::select(-Annual) |>                   # Remove annual column 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr,   # pivot the column data into one col
                        May, Jun, Jul, Aug, 
                        Sep, Oct, Nov, Dec), 
               values_to = "Precipitation",   # store the values in Precipitation
               names_to = "Month") |>         # store the months in Month
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
                                   TRUE                 ~ Precipitation))|>
  mutate(Precipitation = as.numeric(Precipitation))

# summarize the data
ggplot(data=dat.precip.long) +
  geom_histogram(aes(x=Precipitation, y=after_stat(density)),
                 breaks=seq(0, 15, 1),
                 color="grey")+
  geom_hline(yintercept = 0)+
  theme_bw() +
  xlab("Precipitation (Inches)")
ylab("Density")

# extract data summaries
library(e1071)
data.summaries <- dat.precip.long |>
  summarize(
    mean = mean(Precipitation, na.rm=T),
    sd = sd(Precipitation, na.rm=T),
    min = min(Precipitation, na.rm=T),
    max = max(Precipitation, na.rm=T),
    skew = skewness(Precipitation, na.rm=T),
    kurt = kurtosis(Precipitation, na.rm=T)
  )

#####################################
# Maximum likelihood (Gamma)
#####################################
llgamma <- function(par, data, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik <- sum(log(dgamma(x=data, shape=alpha, scale=beta)), na.rm = T)
  
  return(ifelse(neg, -loglik, loglik))
}

gamma.MLEs <- optim(fn = llgamma,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)

#####################################
# Maximum likelihood (Log Normal)
#####################################
ll.lognorm <- function(par, data, neg=F){
  mu <- par[1]
  sigma <- par[2]
  
  loglik <- sum(log(dlnorm(x=data, meanlog = mu, sdlog = sigma)), na.rm = T)
  
  return(ifelse(neg, -loglik, loglik))
}

lognorm.MLEs <- optim(fn = ll.lognorm,
              par = c(0,1),
              data = dat.precip.long$Precipitation,
              neg=T)


