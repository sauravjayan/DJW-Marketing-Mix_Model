library(tidyverse)
library(lfe)
library(stargazer)

#Section 1

#1.

dat <- read_csv("~/Downloads/randomized_data_for_hw.csv")

#3.
dat <- dat %>% mutate(nat=log(nat+1), loc=log(loc+1), dig=log(dig+1), 
                      compnat=log(compnat+1), comploc=log(comploc+1), compdig=log(compdig+1), 
                      qua=log(qua+1), val=log(val+1), sat=log(sat+1))

#4.

dat <- dat %>% group_by(Brand) %>% mutate(lag1qua=lag(qua, n=1), lag2qua=lag(qua, n=2), 
                                          lag3qua=lag(qua,n=3), lag4qua=lag(qua,n=4), 
                                          lag5qua=lag(qua,n=5), lag6qua=lag(qua,n=6), 
                                          lag7qua=lag(qua,n=7), lag8qua=lag(qua,n=8),
                                          lag9qua=lag(qua,n=9), lag10qua=lag(qua,n=10),
                                          lag11qua=lag(qua,n=11), lag12qua=lag(qua,n=12), 
                                          lag13qua=lag(qua,n=13))

dat <- dat %>% group_by(Brand) %>% mutate(lag1val=lag(val, n=1), lag2val=lag(val, n=2), 
                                          lag3val=lag(val,n=3), lag4val=lag(val,n=4), 
                                          lag5val=lag(val,n=5), lag6val=lag(val,n=6), 
                                          lag7val=lag(val,n=7), lag8val=lag(val,n=8),
                                          lag9val=lag(val,n=9), lag10val=lag(val,n=10),
                                          lag11val=lag(val,n=11), lag12val=lag(val,n=12), 
                                          lag13val=lag(val,n=13))

dat <- dat %>% group_by(Brand) %>% mutate(lag1sat=lag(sat, n=1), lag2sat=lag(sat, n=2), 
                                          lag3sat=lag(sat,n=3), lag4sat=lag(sat,n=4), 
                                          lag5sat=lag(sat,n=5), lag6sat=lag(sat,n=6), 
                                          lag7sat=lag(sat,n=7), lag8sat=lag(sat,n=8),
                                          lag9sat=lag(sat,n=9), lag10sat=lag(sat,n=10),
                                          lag11sat=lag(sat,n=11), lag12sat=lag(sat,n=12), 
                                          lag13sat=lag(sat,n=13))

#5.

dat <- dat %>% group_by(Brand) %>% mutate(lag1nat=lag(nat, n=1), lag2nat=lag(nat, n=2), 
                                          lag3nat=lag(nat,n=3), lag4nat=lag(nat,n=4), 
                                          lag5nat=lag(nat,n=5))

dat <- dat %>% group_by(Brand) %>% mutate(lag1loc=lag(loc, n=1), lag2loc=lag(loc, n=2), 
                                          lag3loc=lag(loc,n=3), lag4loc=lag(loc,n=4), 
                                          lag5loc=lag(loc,n=5))

dat <- dat %>% group_by(Brand) %>% mutate(lag1dig=lag(dig, n=1), lag2dig=lag(dig, n=2), 
                                          lag3dig=lag(dig,n=3), lag4dig=lag(dig,n=4), 
                                          lag5dig=lag(dig,n=5))

dat <- dat %>% group_by(Brand) %>% mutate(lag1compnat=lag(compnat, n=1), lag2compnat=lag(compnat, n=2), 
                                          lag3compnat=lag(compnat,n=3), lag4compnat=lag(compnat,n=4), 
                                          lag5compnat=lag(compnat,n=5))

dat <- dat %>% group_by(Brand) %>% mutate(lag1compdig=lag(compdig, n=1), lag2compdig=lag(compdig, n=2), 
                                          lag3compdig=lag(compdig,n=3), lag4compdig=lag(compdig,n=4), 
                                          lag5compdig=lag(compdig,n=5))

dat <- dat %>% group_by(Brand) %>% mutate(lag1comploc=lag(comploc, n=1), lag2comploc=lag(comploc, n=2), 
                                          lag3comploc=lag(comploc,n=3), lag4comploc=lag(comploc,n=4), 
                                          lag5comploc=lag(comploc,n=5))
#6

dat <- dat %>%
  group_by(Brand) %>%
  slice(14:n())


#Section 2

#1.
re1 <- lm(qua~nat + loc + dig + lag1qua   +  lag2qua   +  lag3qua  +   lag4qua    +
            lag5qua  +   lag6qua   +  lag7qua   +  lag8qua  +   lag9qua    + lag10qua  +  lag11qua  +  lag12qua  +  lag13qua +  
            lag1val   +  lag2val +    lag3val +    lag4val    + lag5val    + lag6val  +   lag7val +    lag8val  +   lag9val  +  
            lag10val  +  lag11val    +lag12val +   lag13val  +  lag1sat   +  lag2sat   +  lag3sat +    lag4sat  +   lag5sat   + 
            lag6sat   +  lag7sat   +  lag8sat   +  lag9sat   +  lag10sat   + lag11sat  +  lag12sat   + lag13sat    +lag1nat   + 
            lag2nat   +  lag3nat   +  lag4nat  +   lag5nat   +  lag1loc   +  lag2loc    + lag3loc    + lag4loc    + lag5loc  +  
            lag1dig +    lag2dig   +  lag3dig+     lag4dig  +   lag5dig +    lag1compnat  +lag2compnat  +lag3compnat+  lag4compnat +
            lag5compnat  +lag1comploc+ lag2comploc +lag3comploc +lag4comploc +lag5comploc+ lag1compdig+ lag2compdig +lag3compdig+
            lag4compdig+ lag5compdig + as.factor(Brand) + as.factor(time_period), data=dat)

#2.

re2 <- felm(qua~nat + loc + dig + lag1qua   +  lag2qua   +  lag3qua  +   lag4qua    +
              lag5qua  +   lag6qua   +  lag7qua   +  lag8qua  +   lag9qua    + lag10qua  +  lag11qua  +  lag12qua  +  lag13qua +  
              lag1val   +  lag2val +    lag3val +    lag4val    + lag5val    + lag6val  +   lag7val +    lag8val  +   lag9val  +  
              lag10val  +  lag11val    +lag12val +   lag13val  +  lag1sat   +  lag2sat   +  lag3sat +    lag4sat  +   lag5sat   + 
              lag6sat   +  lag7sat   +  lag8sat   +  lag9sat   +  lag10sat   + lag11sat  +  lag12sat   + lag13sat    +lag1nat   + 
              lag2nat   +  lag3nat   +  lag4nat  +   lag5nat   +  lag1loc   +  lag2loc    + lag3loc    + lag4loc    + lag5loc  +  
              lag1dig +    lag2dig   +  lag3dig+     lag4dig  +   lag5dig +    lag1compnat  +lag2compnat  +lag3compnat+  lag4compnat +
              lag5compnat  +lag1comploc+ lag2comploc +lag3comploc +lag4comploc +lag5comploc+ lag1compdig+ lag2compdig +lag3compdig+
              lag4compdig+ lag5compdig | as.factor(Brand) + as.factor(time_period), data=dat)

#3.

re3 <- felm(qua~nat + loc + dig + lag1qua   +  lag2qua   +  lag3qua  +   lag4qua    +
              lag5qua  +   lag6qua   +  lag7qua   +  lag8qua  +   lag9qua    + lag10qua  +  lag11qua  +  lag12qua  +  lag13qua +  
              lag1val   +  lag2val +    lag3val +    lag4val    + lag5val    + lag6val  +   lag7val +    lag8val  +   lag9val  +  
              lag10val  +  lag11val    +lag12val +   lag13val  +  lag1sat   +  lag2sat   +  lag3sat +    lag4sat  +   lag5sat   + 
              lag6sat   +  lag7sat   +  lag8sat   +  lag9sat   +  lag10sat   + lag11sat  +  lag12sat   + lag13sat    +lag1nat   + 
              lag2nat   +  lag3nat   +  lag4nat  +   lag5nat   +  lag1loc   +  lag2loc    + lag3loc    + lag4loc    + lag5loc  +  
              lag1dig +    lag2dig   +  lag3dig+     lag4dig  +   lag5dig +    lag1compnat  +lag2compnat  +lag3compnat+  lag4compnat +
              lag5compnat  +lag1comploc+ lag2comploc +lag3comploc +lag4comploc +lag5comploc+ lag1compdig+ lag2compdig +lag3compdig+
              lag4compdig+ lag5compdig | as.factor(Brand) + as.factor(time_period), data=dat, weights = dat$qua_vol)

#4.

re4 <- felm(qua~nat + loc + dig + lag1qua   +  lag2qua   +  lag3qua  +   lag4qua    +
              lag5qua  +   lag6qua   +  lag7qua   +  lag8qua  +   lag9qua    + lag10qua  +  lag11qua  +  lag12qua  +  lag13qua +  
              lag1val   +  lag2val +    lag3val +    lag4val    + lag5val    + lag6val  +   lag7val +    lag8val  +   lag9val  +  
              lag10val  +  lag11val    +lag12val +   lag13val  +  lag1sat   +  lag2sat   +  lag3sat +    lag4sat  +   lag5sat   + 
              lag6sat   +  lag7sat   +  lag8sat   +  lag9sat   +  lag10sat   + lag11sat  +  lag12sat   + lag13sat    +lag1nat   + 
              lag2nat   +  lag3nat   +  lag4nat  +   lag5nat   +  lag1loc   +  lag2loc    + lag3loc    + lag4loc    + lag5loc  +  
              lag1dig +    lag2dig   +  lag3dig+     lag4dig  +   lag5dig +    lag1compnat  +lag2compnat  +lag3compnat+  lag4compnat +
              lag5compnat  +lag1comploc+ lag2comploc +lag3comploc +lag4comploc +lag5comploc+ lag1compdig+ lag2compdig +lag3compdig+
              lag4compdig+ lag5compdig | as.factor(Brand) + as.factor(yrqtr) + as.factor(time_period), data=dat, weights = dat$qua_vol)

#5

re5 <- felm(qua~nat + loc + dig + lag1qua   +  lag2qua   +  lag3qua  +   lag4qua    +
              lag5qua  +   lag6qua   +  lag7qua   +  lag8qua  +   lag9qua    + lag10qua  +  lag11qua  +  lag12qua  +  lag13qua +  
              lag1val   +  lag2val +    lag3val +    lag4val    + lag5val    + lag6val  +   lag7val +    lag8val  +   lag9val  +  
              lag10val  +  lag11val    +lag12val +   lag13val  +  lag1sat   +  lag2sat   +  lag3sat +    lag4sat  +   lag5sat   + 
              lag6sat   +  lag7sat   +  lag8sat   +  lag9sat   +  lag10sat   + lag11sat  +  lag12sat   + lag13sat    +lag1nat   + 
              lag2nat   +  lag3nat   +  lag4nat  +   lag5nat   +  lag1loc   +  lag2loc    + lag3loc    + lag4loc    + lag5loc  +  
              lag1dig +    lag2dig   +  lag3dig+     lag4dig  +   lag5dig +    lag1compnat  +lag2compnat  +lag3compnat+  lag4compnat +
              lag5compnat  +lag1comploc+ lag2comploc +lag3comploc +lag4comploc +lag5comploc+ lag1compdig+ lag2compdig +lag3compdig+
              lag4compdig+ lag5compdig | as.factor(Brand)+ as.factor(yrqtr)+ as.factor(Industry) +as.factor(time_period), data=dat, weights = dat$qua_vol)

#6

df <- data.frame(re2=re2$coefficients[-c(4:42),'qua'], re3=re3$coefficients[-c(4:42),'qua'] ,re4=re4$coefficients[-c(4:42),'qua'], re5=re5$coefficients[-c(4:42),'qua'])

nats <- df[c('nat','lag1nat','lag2nat','lag3nat','lag4nat','lag5nat'),] 
locs <- df[c('loc','lag1loc','lag2loc','lag3loc','lag4loc','lag5loc'),]
digs <- df[c('nat','lag1dig','lag2dig','lag3dig','lag4dig','lag5dig'),]



plt_nats_re2 <- ggplot(nats, aes(x=seq(0,5), y=re2)) + geom_line() + xlab('Lags') + ylab("Model 2")
plt_nats_re3 <- ggplot(nats, aes(x=seq(0,5), y=re3)) + geom_line() + xlab('Lags') + ylab("Model 3")
plt_nats_re4 <- ggplot(nats, aes(x=seq(0,5), y=re4)) + geom_line() + xlab('Lags') + ylab("Model 4")
plt_nats_re5 <- ggplot(nats, aes(x=seq(0,5), y=re5)) + geom_line() + xlab('Lags') + ylab("Model 5")

plt_locs_re2 <- ggplot(locs, aes(x=seq(0,5), y=re2)) + geom_line() + xlab('Lags') + ylab("Model 2")
plt_locs_re3 <- ggplot(locs, aes(x=seq(0,5), y=re3)) + geom_line() + xlab('Lags') + ylab("Model 3")
plt_locs_re4 <- ggplot(locs, aes(x=seq(0,5), y=re4)) + geom_line() + xlab('Lags') + ylab("Model 4")
plt_locs_re5 <- ggplot(locs, aes(x=seq(0,5), y=re5)) + geom_line() + xlab('Lags') + ylab("Model 5")

plt_digs_re2 <- ggplot(digs, aes(x=seq(0,5), y=re2)) + geom_line() + xlab('Lags') + ylab("Model 2")
plt_digs_re3 <- ggplot(digs, aes(x=seq(0,5), y=re3)) + geom_line() + xlab('Lags') + ylab("Model 3")
plt_digs_re4 <- ggplot(digs, aes(x=seq(0,5), y=re4)) + geom_line() + xlab('Lags') + ylab("Model 4")
plt_digs_re5 <- ggplot(digs, aes(x=seq(0,5), y=re5)) + geom_line() + xlab('Lags') + ylab("Model 5")


pretty_table <- stargazer(re2, re3, re4,re5 , type="text",
          title="Regression Results",  omit = rownames(re2$coefficients)[4:42], out = 'tab.html', 
          column.labels = c('Model1','Model2','Model3','Model4'), model.numbers = FALSE, 
          dep.var.caption = 'Percieved Quality')


