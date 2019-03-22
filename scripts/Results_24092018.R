library(haven); library(R2jags); library(MCMCpack); library(reshape); library(reshape2) 
library(plyr); library(dplyr); library(ggplot2); library(runjags)
library(lme4); library(arm); library(broom); library(gridExtra)
library(stargazer);  library(lfe); library(plm); library(AER)
library(simcf); library(readr)


#-------------------------------------------------------------------------
# CONTENT:
# 
# REPLICATE SPECIFICATIONS IN THE MAIN TEXT
# 1) FIGURE 2: Baseline results
#    - Reduced and full measure
#    - Pooled, 2way FE, and FD results.
# 2) FIGURE 3: How Dynamic is Dynamic Responsiveness?
#    - Varying Time Horizons
#    - Varying Effects over Time
#
# REPLICATE SPECIFICATIONS IN THE APPENDIX
#-------------------------------------------------------------------------

# read in data
# make sure to change the path to the correct local directory on your machine!
# df <- read_csv("~/GitHub/Danish_muni/data/CityPolicy_20092018.csv", 
#                col_types = cols(bluevote = col_number(), 
#                fd_bluevote = col_number()))

df <- read_csv("~/GitHub/Danish_muni/data/CityPolicy_25092018.csv")

# df$socdem_reduced <- df$socdem_reduced*-1
# 
# df$fd_socdem_reduced <- df$lead_socdem_reduced - df$socdem_reduced


pan.dat <- pdata.frame(df, index = c("muni", "year")) # reformat data as panel

pan.dat$lag_fv <- plm::lag(pan.dat$estbluevote, 4)

pan.dat$fd_fv <- pan.dat$estblue_fv - pan.dat$lag_fv

pan.dat$estblue_same <- ifelse(is.na(pan.dat$bluevote)==T, NA,  pan.dat$estbluevote)

#---------------------------------------------------------------------------
#
# REPLICATE RESULTS IN THE MAIN TEXT
#
#--------------------------------------------------------------------------



######################################################################
#
# 1) FIGURE 2: Baseline results
#    - Reduced and full measure
#    - Pooled, 2way FE, and FD results.
#
#######################################################################

#---------------------------------------------------------------------
# Results with municipal election results


#####
# pooled results

# full measure
pool.mod <- plm(lag_socdem1 ~ bluevote+ log_pop, data = pan.dat , 
                model = "pooling")
coeftest(pool.mod, vcovHC(pool.mod, type = "HC1", cluster = "group"))

# reduced measure
pool.mod.red <- plm(lead_socdem_reduced ~ bluevote+ log_pop, data = pan.dat, 
                    model = "pooling")
coeftest(pool.mod.red, vcovHC(pool.mod.red, type = "HC1", cluster = "group"))

#####
# fixed effects results

#full measure
fe.mod <- plm(lag_socdem1 ~ bluevote + log_pop + factor(year), data = pan.dat , 
              model = "within", effects = "individual")
coeftest(fe.mod, vcovHC(fe.mod, cluster="group", type = "HC1"))

# allow time-trends/shocks to vary by population size and region (amt)
lfe_mod <- felm(lag_socdem1 ~ bluevote 
                | factor(muni) + factor(year):log_pop + factor(year) + factor(year):factor(region)|
                  0| muni,
                data = pan.dat)
summary(lfe_mod)

#reduced measure

fe.mod.red <- plm(lead_socdem_reduced ~ bluevote+ log_pop + factor(year), 
                  data = pan.dat , 
                  model = "within", effects = "individual")
coeftest(fe.mod.red, vcovHC(fe.mod.red, cluster = "group", type = "HC1"))

# varying trends
lfe_mod_red <- felm(lead_socdem_reduced ~ bluevote 
                    | factor(muni) + factor(year):log_pop + factor(year) + factor(year):factor(region)|
                      0| muni,exactDOF=F,
                    data = pan.dat)
summary(lfe_mod_red)

####
# FD results

# four-year lead difference in outcome
# four-year lagged difference in covariates

fd.mod <- plm(fd_socdem ~ fd_bluevote +  fd_pop + factor(year), 
              data = pan.dat, model = "pooling")

coeftest(fd.mod, vcovBK(fd.mod, type = "HC1"))

#reduced measure
fd.mod.red <- plm(fd_socdem_reduced ~ fd_bluevote + fd_pop + factor(year), 
                  data = pan.dat, model = "pooling", 
                  effects = "time")
coeftest(fd.mod.red, vcovBK(fd.mod.red, type = "HC1"))


#---------------------------------------------------------------------
# Results with national election results


#####
# pooled results

# full measure
pool.nat <- plm(lag_socdem1 ~ estbluevote + log_pop, data = pan.dat , 
                model = "pooling")
coeftest(pool.nat, vcovHC(pool.nat, type = "HC1", cluster = "group"))

# reduced measure
pool.nat.red <- plm(lead_socdem_reduced ~ estblue_same + log_pop, data = pan.dat, 
                    model = "pooling")
coeftest(pool.nat.red, vcovHC(pool.nat.red, type = "HC1", cluster = "group"))

#####
# fixed effects results

pan.dat$muni_nat <- pan.dat$bluevote - pan.dat$estblue_same 

#full measure
fe.nat <- plm(plm::lag(correctsocdem, 1) ~  estblue_same + log_pop + factor(year), data = pan.dat , 
              model = "within", effects = "twoway")
coeftest(fe.nat, vcovHC(fe.nat, cluster="group", type = "HC1"))

# allow time-trends/shocks to vary by population size and region (amt)
lfe_nat <- felm(lag_socdem1 ~ estblue_fv_same 
                | factor(muni) + factor(year):log_pop + factor(year) + factor(year):factor(region)|
                  0| muni,
                data = pan.dat)
summary(lfe_nat)

#reduced measure

fe.nat.red <- plm(spendcap_scale ~ estblue_fv_same+ log_pop + factor(year), 
                  data = pan.dat , 
                  model = "within", effects = "individual")
coeftest(fe.nat.red, vcovHC(fe.nat.red, cluster = "group", type = "HC1"))

# varying trends
lfe_nat_red <- felm(lead_socdem_reduced ~ estblue_fv_same 
                    | factor(muni) + factor(year):log_pop + factor(year) + factor(year):factor(region)|
                      0| muni,exactDOF=F,
                    data = pan.dat)
summary(lfe_nat_red)

####
# FD results

# four-year lead difference in outcome
# four-year lagged difference in covariates

fd.nat <- plm(fd_socdem ~ fd_fv +  fd_pop + factor(year), 
              data = pan.dat, model = "pooling")

coeftest(fd.nat, vcovBK(fd.nat, type = "HC1"))

#reduced measure
fd.nat.red <- plm(fd_socdem_reduced ~ fd_fv + fd_pop + factor(year), 
                  data = pan.dat, model = "pooling", 
                  effects = "time")
coeftest(fd.nat.red, vcovBK(fd.nat.red, type = "HC1"))

########
# PRESENT RESULTS

#regression table municipal election results

p_se <- sqrt(diag(vcovHC(pool.mod, type = "HC1")))
fe_se <- sqrt(diag(vcovHC(fe.mod, type = "HC1")))
fe2_se <- sqrt(diag(vcov(lfe_mod)))
fd_se <- sqrt(diag(vcovBK(fd.mod, type = "HC1")))
p_red_se <- sqrt(diag(vcovHC(pool.mod.red, type = "HC1")))
fe_red_se <- sqrt(diag(vcovHC(fe.mod.red, type = "HC1")))
fe2_red_se <- sqrt(diag(vcov(lfe_mod_red)))
fd_red_se <- sqrt(diag(vcovBK(fd.mod.red, type = "HC1")))


stargazer(pool.mod, fe.mod, lfe_mod, fd.mod,
          pool.mod.red, fe.mod.red, lfe_mod_red, fd.mod.red,
          se = list(p_se, fe_se, fe2_se, fd_se,
                    p_red_se, fe_red_se, fe2_red_se, fd_red_se),
          covariate.labels = c("Right-Wing Vote Share", 
                               "Population Size (logged)"),
          dep.var.labels = c("Fiscal Conservatism", "Fiscal Conservatism (Four-Year Differenced)"),
          column.labels = c("Pooled", "Fixed Effects", "Diff Trend", "First-Difference",
                            "Pooled", "Fixed Effects", "Diff Trend", "First-Difference"),
          no.space = T, df=F,
          model.names = F,
          omit="factor\\(",
          omit.stat = c("f", "rsq", "ser"),
          add.lines = list(c("Municipality FE?", "No", "Yes", "Yes", "No", "No", "Yes", "Yes", "No"),
                           c("Year FE?", "No", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes"),
                           c("Year X Region FE?", "No", "No", "Yes", "No", "No", "No", "Yes", "No"),
                           c("Four-Year Differenced?", "No", "No", "No", "Yes", "No", "No", "No", "Yes"))
          )

# table with national election results
# insert manually into table above .tex file

p_se_nat <- sqrt(diag(vcovHC(pool.nat, type = "HC1")))
fe_se_nat <- sqrt(diag(vcovHC(fe.nat, type = "HC1")))
fe2_se_nat <- sqrt(diag(vcov(lfe_nat)))
fd_se_nat <- sqrt(diag(vcovBK(fd.nat, type = "HC1")))
p_red_se_nat <- sqrt(diag(vcovHC(pool.nat.red, type = "HC1")))
fe_red_se_nat <- sqrt(diag(vcovHC(fe.nat.red, type = "HC1")))
fe2_red_se_nat <- sqrt(diag(vcov(lfe_nat_red)))
fd_red_se_nat <- sqrt(diag(vcovBK(fd.nat.red, type = "HC1")))


stargazer(pool.nat, fe.nat, lfe_nat, fd.nat,
          pool.nat.red, fe.nat.red, lfe_nat_red, fd.nat.red,
          se = list(p_se_nat, fe_se_nat, fe2_se_nat, fd_se_nat,
                    p_red_se_nat, fe_red_se_nat, fe2_red_se_nat, fd_red_se_nat),
          covariate.labels = c("Right-Wing Vote Share National", 
                               "Population Size (logged)"),
          dep.var.labels = c("Fiscal Conservatism", "Fiscal Conservatism (Four-Year Differenced)"),
          column.labels = c("Pooled", "Fixed Effects", "Diff Trend", "First-Difference",
                            "Pooled", "Fixed Effects", "Diff Trend", "First-Difference"),
          no.space = T, df=F,
          model.names = F,
          omit="factor\\(",
          omit.stat = c("f", "rsq", "ser")
)

# plot results
# extract
res <- data.frame(rbind(coef(pool.mod)[2], coef(fe.mod)[1],coef(lfe_mod)[1], coef(fd.mod)[2],
                        coef(pool.mod.red)[2], coef(fe.mod.red)[1], coef(lfe_mod_red)[1], coef(fd.mod.red)[2]),
                  rbind(sqrt(diag(vcovHC(pool.mod, type = "HC1")))[2],
                        sqrt(diag(vcovHC(fe.mod, type = "HC1")))[1],
                        sqrt(diag(vcov(lfe_mod)))[1],
                        sqrt(diag(vcovBK(fd.mod, type = "HC1")))[2],
                        sqrt(diag(vcovHC(pool.mod.red, type = "HC1")))[2],
                        sqrt(diag(vcovHC(fe.mod.red, type = "HC1")))[1],
                        sqrt(diag(vcov(lfe_mod_red)))[1],
                        sqrt(diag(vcovBK(fd.mod.red, type = "HC1")))[2]),
                  spec=c("Pooled", "Twoway\nFixed Effects", "Fixed Effects\nVarying Trends",
                         "First-Difference"),
                  #cont=c(rep("Simple Model",6), rep("+ Controls", 6)),
                  measure=c(rep("Full Measure",4), rep("Reduced Measure", 4)))
names(res)[1:2] <- c("PE", "SE")

res$spec <- factor(res$spec, levels = c("First-Difference", 
                                        "Fixed Effects\nVarying Trends",
                                        "Twoway\nFixed Effects",
                                        "Pooled"))


c_plot<-ggplot(res, aes(x = PE, y = spec)) +
  geom_point() +
  geom_errorbarh(aes(xmin=PE-(1.96*SE),
                     xmax=PE+(1.96*SE)), height = 0, size = .25) +
  geom_errorbarh(aes(xmin=PE-(1.64*SE),
                     xmax=PE+(1.64*SE)), height = 0, size = .75) +
  facet_wrap(~measure, scales = "free_x") +
  theme_classic() + 
  geom_vline(xintercept= 0, lty=2) +
  #scale_y_continuous(limits = c(-.1,2.5))+
  #coord_flip() +
  labs(x=NULL,y="Coefficient on Electoral Support for\n Right-Wing Parties") 
c_plot

#setwd("C:/Users/ex-bce/Dropbox/Responsiveness/images")
setwd("~/GitHub/Danish_muni/images")
ggsave(c_plot, filename = "CoefPlot_05102018.eps",
       device = cairo_ps, 
       width = 7.34,
       heigh = 4.56)


################################################################
#
# 2) FIGURE 3: How Dynamic is Dynamic Responsiveness?
#    - Varying Time Horizons
#    - Varying Effects over Time
#
###############################################################

#---------------------------------------------------------------
# PANEL A: varying the time horizon

# placebo model
mod1 <- plm(plac ~ bluevote + log_pop + factor(year), data = pan.dat, 
            model = "within", effects = "indvidual")

coeftest(mod1, vcovHC(mod1, cluster = "group"))


plac_res <- cbind(estimate = coef(mod1)[1], 
                  SE = sqrt(diag(vcovHC(mod1, cluster = "group",
                                        type = "HC1")))[1], -4)

for(y in 1:3){
  crnt_mod <- plm(plm::lag(correctsocdem, y) ~ bluevote+ log_pop + factor(year), 
                  data = pan.dat , model = "within", effects = "individual")
  crnt_res <- cbind(estimate = coef(crnt_mod)[1], 
                    SE = sqrt(diag(vcovHC(crnt_mod, cluster = "group",
                                          type = "HC1")))[1], -y)
  plac_res<-rbind(plac_res, crnt_res)
}


# model at year zero -- to append all leads on within the loop
mod2 <- plm(correctsocdem ~ bluevote+ log_pop + factor(year), 
            data = pan.dat , model = "within", effects = "individual")

# extract results from year zero
loop_res <- cbind(estimate = coef(mod2)[1], 
                  SE = sqrt(diag(vcovHC(mod2)))[1],0)

#loop over lead 1:20 and append results on year zero
for(i in 1:20){
  this_lag <- -i
  crnt_mod <- plm(plm::lag(correctsocdem, this_lag) ~ bluevote+ log_pop + factor(year), 
                  data = pan.dat , model = "within", effects = "individual")
  crnt_res <- cbind(estimate = coef(crnt_mod)[1], 
                    SE = sqrt(diag(vcovHC(crnt_mod, cluster = "group",
                                          type = "HC1")))[1],i)
  loop_res<-rbind(loop_res, crnt_res)
  
}




loop_res <- rbind(plac_res, loop_res)
loop_res <- as.data.frame(loop_res)
names(loop_res)[3]<-"years"
#loop_res$years <- c(-4,0:20)

h1<-ggplot(loop_res, aes( x = years, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - (1.96*SE),
                    ymax = estimate + (1.96*SE)),
                width = 0) +
  geom_hline(yintercept=0, lty = 3) +
  theme_classic() +
  scale_x_continuous(limits = c(-5,22),
                     breaks = loop_res$years,
                      labels = c("4 years earlier\n(Previous election)",
                                 " ",
                                 " ",
                                 " ",
                                 "Year 0\n(This election)",
                                 " ",
                                 " ",
                                 " ",
                                 "4 years\n(Next election)",
                                 " ",
                                 " ",
                                 " ",
                                 "8 years later\n(2nd election)",
                                 " ",
                                 " ",
                                 " ",
                                 "12 years later\n(3rd election)",
                                 " ",
                                 " ",
                                 " ",
                                 "16 years later\n(4th election)",
                                 " ",
                                 " ",
                                 " ",
                                 "20 years later\n(5th election)"))+
  labs(x = "Years until Election", 
       y = "Coefficient on Electoral Support for\nRight-Wing Parties")
h1

setwd("~/GitHub/Danish_muni/images")

ggsave(h1, filename="dynamics.eps",
       device = cairo_ps,
       width = 9,
       height = 6.5)
#----------------------------------------------------------------
# PANEL B: Varying effects over time (Random slopes by year)

# remove missings
helper <- lm(lag_socdem1 ~ correctsocdem + bluevote + log_pop + muni + 
               year, data=pan.dat)
used <- extractdata(helper, pan.dat, na.rm=T)

year.slope <- lmer(lag_socdem1 ~ correctsocdem + bluevote + log_pop + 
                     (1 + bluevote| year) +
                     factor(year) + factor(muni), data = used)

# bootstrap the random coefficients for each year to get uncertainty
boot_year <- bootMer(year.slope, FUN = function(x) coef(x)$year[,3],
                     nsim = 100, .progress="txt")

#extract and plot results
year.fx <- data.frame(year = rownames(as.data.frame(coef(year.slope)$year)),
                      summary(boot_year)[4:5])
year.fx$year <- as.numeric(as.character(year.fx$year))#make sure year is numeric

h2<-ggplot(year.fx, aes(x = year, y = bootMed)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=bootMed-(1.64*bootSE), ymax=bootMed+(1.64*bootSE)), 
              color = "grey", alpha = .2) +
  #geom_ribbon(aes(ymin=lwr90, ymax=upr90), color = "grey", alpha = .3) +
  theme_classic() +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = coef(fe.mod)[1], lty = 2) +
  annotate(geom = "text", x = 1984, y = 0.14, 
           label = "Baseline Estimate", size = 3.5) +
  labs(x = NULL, y = "Simulated Median Coefficient\n(Random slope by year)",
       title = "B: Varying Effects Over Time") +
  scale_x_continuous(breaks = as.vector(year.fx$year))

h2

h <- grid.arrange(h1, h2, ncol = 1)

setwd("~/GitHub/Danish_muni/images")
ggsave(h, filename="EffectsVsTime.eps",
       device = cairo_ps,
       width = 8.56,
       heigh = 6.5)


#---------------------------------------------------------------------------
#
# REPLICATE RESULTS IN THE APPENDIX
#
#--------------------------------------------------------------------------


####################################################################
#
# APPENDIX D: Some Descriptive Features of Municipal Fiscal Policy
#
####################################################################



agg.all <- df %>%
  group_by(muniname) %>%
  summarise(av.score = mean(correctsocdem, na.rm = T) )

agg.all <- agg.all[order( agg.all$av.score),]

most.soc <- agg.all[224:274,]
most.soc$SocDem <- "51 Most SocDem Municipalities"

least.soc <- agg.all[1:51,]
least.soc$SocDem <- "51 Least SocDem Municipalities"

agg.all2 <- rbind(most.soc, least.soc)

p2 <- ggplot(agg.all2, aes(x = reorder(muniname, av.score), y = av.score)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  facet_wrap(~ SocDem, scales = "free_y") +
  labs(x = " ", y = "Fiscal Conservatism \n (Averaged over 1974-2006)") +
  theme_bw() + theme( axis.line = element_blank(), # axis.line = element_line(colour = "black"),
                      panel.border = element_blank(), plot.title = element_text(hjust = 0.5),
                      axis.text.y = element_text(size = 6.5))

p2

p3 <- grid.arrange(p1, p2)

setwd("C:/Users/ex-bce/Desktop/responsiveness/images")
ggsave(plot = p3, filename = "socialdemocratism.eps", device = cairo_ps)




####################################################################
#
# Granger test for reverse causality
#
######################################################################

pan.dat2$lag_SocDem <- plm::lag(pan.dat2$correct.SocDem, 1)
pan.dat2$lag_SocDem2 <- plm::lag(pan.dat2$correct.SocDem, 2)
pan.dat2$lag_SocDem3 <- plm::lag(pan.dat2$correct.SocDem, 3)
pan.dat2$lag_SocDem4 <- plm::lag(pan.dat2$correct.SocDem, 4)


grang <- plm(bluevote ~ lag_SocDem + log_pop+ factor(year), 
             model = "within", effects = "individual",
             data = pan.dat2)

coeftest(grang, vcovHC(grang, cluster = "group", type = "HC1"))


grang2 <- plm(bluevote ~ lag_SocDem2 + log_pop+ factor(year), 
              model = "within", effects = "individual",
              data = pan.dat2)
coeftest(grang2, vcovHC(grang2, cluster = "group", type = "HC1"))

grang3 <- plm(bluevote ~ lag_SocDem3+ log_pop+ factor(year), 
              model = "within", effects = "individual",
              data = pan.dat2)
coeftest(grang3, vcovHC(grang3, cluster = "group", type = "HC1"))

grang4 <- plm(bluevote ~ lag_SocDem4+ log_pop + factor(year), 
              model = "within", effects = "individual",
              data = pan.dat2)
coeftest(grang4, vcovHC(grang4, cluster = "group", type = "HC1"))

# combine results and plot

grang_res <- data.frame(rbind(coef(grang)[1], coef(grang2)[1], coef(grang3)[1],
                              coef(grang4)[1]),
                        rbind(sqrt(diag(vcovHC(grang, type = "HC1")))[1],
                              sqrt(diag(vcovHC(grang2, type = "HC1")))[1],
                              sqrt(diag(vcovHC(grang3, type = "HC1")))[1],
                              sqrt(diag(vcovHC(grang4, type = "HC1")))[1]),
                        spec=c("One-Year Lag", "Two-Year Lag", "Three-Year Lag", "Four-Year Lag"))
names(grang_res)[1:2] <- c("PE", "SE")

grang_res$spec <- factor(grang_res$spec, levels = c("Four-Year Lag",
                                                    "Three-Year Lag",
                                                    "Two-Year Lag",
                                                    "One-Year Lag"))

g_plot<-ggplot(grang_res, aes(y = PE, x = spec)) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = .16, lty = 2) +
  scale_x_discrete() +
  annotate(geom = "text",
           x = 4,
           y = .157,
           label = "Baseline Effect\n(Standardized)",
           angle = 90,
           size = 3) +
  geom_point() +
  geom_errorbar(aes(ymin=PE-(1.96*SE),
                    ymax=PE+(1.96*SE)), width = 0) +
  theme_classic() +
  scale_y_continuous(limits = c(-.20, .25)) +
  coord_flip() +
  labs(x=NULL, y="Coefficient on Fiscal Conservatism") 



g_plot

#setwd("~/GitHub/Danish_muni/images")
setwd("C:/Users/ex-bce/Dropbox/Responsiveness/images")
ggsave(g_plot, filename = "granger_18092018.eps",
       device = cairo_ps, 
       width = 4.55,
       height = 4.55)

#######################################################################
#
# Balance on socio economic characteristics
#
##########################################################################

#balance across socio economic covariates
#first: demean by year
helper <-plm(bluevote~ educ + immig +unemployment + factor(year) + muni, data = pan.dat2 , 
             model = "pooling", effects = "individual")

used <- extractdata(helper, pan.dat2, na.rm=T)
used <-pdata.frame(used, index=c("muni","year"))

# residualize bluevote by year
used$dm_bluevote <- resid(lm(bluevote ~ factor(year), data =used))

#run model
balance <- plm(dm_bluevote~ educ + immig +unemployment, 
               data = used , 
               model = "within", effects = "individual")

coeftest(balance, vcovHC(balance, cluster="group", type = "HC1"))

# can we reject coefficients jointly zero? No.
wald  <- pwaldtest(balance, test = "Chisq", 
                   vcov=vcovHC(balance, cluster="group", type = "HC1"))
wald <- round(wald$statistic, digits = 3)
bal_se <- sqrt(diag(vcovHC(balance, 
                           cluster = "group", 
                           type = "HC1")))

stargazer(balance, se = list(bal_se),
          covariate.labels = c("Education", "Immigrants",
                               "Unemployed"),
          omit.stat = c("rsq", "f"),
          add.lines = list(c("Wald Stat", wald),
                           c("Municipality?", "Yes"),
                           c("Year FE?", "Yes"))) 

##################################################################
#
# The mechanism: responsiveness or selection?
#
#################################################################

# function for computing marginal effects

marginsplotdf<-function(model, xterm, zterm, zseq){
  coefs<-coef(model)
  cov<-vcovNW(model, type = "HC1")
  intterm<-ifelse(is.na(coefs[paste(xterm,zterm,sep=":")]),paste(zterm,xterm,sep=":"),paste(xterm,zterm,sep=":"))
  dy.dx<-coefs[xterm]+coefs[intterm]*zseq
  se.dy.dx<-sqrt(cov[xterm,xterm]+zseq^2*cov[intterm,intterm]+zseq*2*cov[xterm,intterm])
  margins<-data.frame(z=zseq,dydx=dy.dx,se=se.dy.dx)
  return(margins)
}


# do politicians from both major parties respond equally?
# interaction models
pan.dat3$mayor_bin <- relevel(pan.dat3$mayor_bin, ref = 0)

#2way FE model
fe.inter <- plm(lag_SocDem1 ~ bluevote*mayor_bin + factor(year), 
                data = pan.dat3, model = "within", effects = "individual")
coeftest(fe.inter, vcovHC(fe.inter, type = "HC1", cluster = "group"))

marg_fx <- marginsplotdf(fe.inter, xterm = "bluevote", zterm = "mayor_bin1", zseq=0:1)

marg_fx2 <- marginsplotdf(fe.inter, xterm = "bluevote", zterm = "mayor_bin2", zseq=0:1)
marg_fx <- rbind(marg_fx, marg_fx2[2,])

marg_fx$party <- c("Social Democrats", "The Liberal Party", "Other Party")



marg_plot<-ggplot(marg_fx, aes(y = party, x = dydx)) +
  geom_point() +
  geom_errorbarh(aes(xmin=dydx-(1.96*se),
                     xmax=dydx+(1.96*se)),
                 height=0) +
  theme_classic() +
  scale_x_continuous(limits=c(-0.2,.5)) +
  geom_vline(xintercept = 0, lty=2) +
  labs(x = "Effect Conditional on Mayoral Party",
       y = NULL)
marg_plot
setwd("~/GitHub/Danish_muni/images")
ggsave(marg_plot, filename = "MargFX_31082018.eps",
       width = 5.7,
       height = 4.5)


# are changes in policy driven by the politicians, voters select?
# post-treatment mediator models

#2way FE models

fe.party <- plm(lag_SocDem1 ~  mayor_bin+ log_pop + factor(year), 
                data = pan.dat3, model = "within", effects = "individual")
coeftest(fe.party, vcovHC(fe.party, type = "HC1", cluster = "group"))

fe.medi <- plm(lag_SocDem1 ~ bluevote + mayor_bin+ log_pop + factor(year), 
               data = pan.dat3, model = "within", effects = "individual")
coeftest(fe.medi, vcovHC(fe.medi, type = "HC1", cluster = "group"))

medi_res <- data.frame(PE=coef(fe.medi)[1:3],
                       SE=sqrt(diag(vcovNW(fe.medi, type = "HC1")))[1:3])
medi_res <- rbind(medi_res, data.frame(PE=coef(fe.mod)[1],
                                       SE = sqrt(diag(vcovHC(fe.mod, type="HC1")))[1]))
medi_res$spec <- c("W/ Mayoral Party Control", "Liberal vs. SocDem", "Other vs. SocDem",
                   "Baseline Estimate")

medi_res$spec <- factor(medi_res$spec, levels = c("Liberal vs. SocDem", 
                                                  "Other vs. SocDem",
                                                  "W/ Mayoral Party Control",
                                                  "Baseline Estimate"))

medi_plot<-ggplot(medi_res, aes(y=spec, x = PE)) +
  geom_point() +
  geom_errorbarh(aes(xmin=PE-(1.96*SE),
                     xmax=PE+(1.96*SE)),
                 height=0) +
  theme_classic() +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 3.5, lty = 3) +
  labs(x="Estimated Coefficient", y = NULL)
medi_plot

ggsave(medi_plot, filename = "PostTreatControl.eps",
       device = cairo_ps,
       width = 5.7,
       height = 4.5)
######################################################################
#
# Item-by-item regressions
#
######################################################################


item_df <- pan.dat3[,c(1,2,20,38:51)]

item_res <- list()

for(i in 4:17){
  item_df[,i] <- plm::lag(item_df[,i], -4)
  
  mod <- plm(item_df[,i] ~ bluevote + factor(year), data = item_df, 
             model = "within", effect = "individual")
  
  item_res[[i]] <- data.frame(PE = coef(mod)[1],
                              SE = sqrt(diag(vcovHC(mod, cluster = "group", 
                                                    type = "HC1")))[1],
                              varname = names(item_df)[i])
  
  
}

item_res <- do.call("rbind", item_res)
item_res$real_name <- c("Income Tax",
                        "Property Tax",
                        "Public Employees",
                        "Day Care",
                        "Food Delivery",
                        "Nursing Home",
                        "Relief Stay",
                        "Competition",
                        "Privately Operated\nServices",
                        "Public Housing",
                        "Class Size",
                        "Spending pr. Pupil",
                        "Commercial Real Estate Tax",
                        "Spending/capita")


item_p<-ggplot(item_res, aes(y=reorder(real_name, PE), x = PE)) +
  geom_point() +
  geom_errorbarh(aes(xmin=PE-(1.96*SE),
                     xmax=PE+(1.96*SE)),
                 height=0) +
  theme_classic() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x="Estimated Coefficient\n(Outcomes are centered and standardized)", y = NULL) +
  scale_x_continuous(breaks = c(-10, seq(-5, 5)))
item_p

setwd("~/GitHub/Danish_muni/images")
ggsave(item_p, filename = "ItemByItem_18092018.eps")















