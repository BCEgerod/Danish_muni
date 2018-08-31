
# preliminaries

# load packages
library(plm); library(AER); library(lfe); library(readr); library(ggplot2)
library(cowplot)

#devtools::install_github("mattblackwell/DirectEffects", ref = "develop")
library(DirectEffects)

# load data
df <- read_csv("~/GitHub/Danish_muni/data/full_data_SameScale.csv")
pan.dat <- pdata.frame(df, index = c("muniname", "year")) # reformat data as panel

#recode mayor variable
pan.dat$mayor_bin <- as.factor(ifelse(pan.dat$mayor == "a", 0,
                                  ifelse(pan.dat$mayor == "v", 1, 2)))
#pan.dat$mayor_bin <- relevel(pan.dat$mayor_bin, ref = "soc_dem")#make SocDem reference

# various measures of muni fiscal SocDem
pan.dat$cons <- pan.dat$full.SocDem*-1
pan.dat$cons_red <- pan.dat$spend.SocDem*-1

pan.dat$lag_SocDem1 <- plm::lag(pan.dat$full.SocDem,-4) # full measure
pan.dat$lead_SocDem_reduced <- plm::lag(pan.dat$cons_red,-4) #reduced measure
pan.dat$fd_SocDem <- pan.dat$lag_SocDem1 - pan.dat$cons #FD full measure
pan.dat$fd_SocDem_reduced <- pan.dat$lead_SocDem_reduced - pan.dat$cons_red #FD reduced measure

# first-difference of red votes
pan.dat$blue_vote <- pan.dat$redvote*-1
pan.dat$lag_redvote <- plm::lag(pan.dat$blue_vote, 4)
pan.dat$fd_redvote <- pan.dat$blue_vote - pan.dat$lag_redvote

# first-difference of logged population size
pan.dat$log_pop <- log(pan.dat$estpop) # log transform
pan.dat$lag_pop <- plm::lag(pan.dat$log_pop, 4) #four-year lag
pan.dat$fd_pop <- pan.dat$log_pop - pan.dat$lag_pop #four-year difference


################################################################
#
# Plot four-year differences association
#
################################################################

p1<-ggplot(pan.dat, aes(x = fd_redvote, y = fd_SocDem)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm", se = F, colour = "black") +
  theme_classic() +
  labs(x = "Difference in prop. votes for the Right-Wing Parties\n(Since last election)",
       y = "Difference in Fiscal Conservatism\n(Between this and next election)")

yhist <- axis_canvas(p1, axis = "y", coord_flip = TRUE) +
  geom_density(data = pan.dat, aes(x = fd_SocDem, alpha = .5), fill = "grey") +
  #scale_fill_manual(values = c("grey", "black")) +
  coord_flip()

combined_plot2 <- insert_yaxis_grob(p1, yhist, position = "right")

xhist <- axis_canvas(p1, axis = "x", coord_flip = F) +
  geom_density(data = pan.dat, aes(x = fd_redvote, alpha = .5), fill = "grey") #+
  #scale_fill_manual(values = c("grey", "black")) +
  #coord_flip()

combined_plot2 <- insert_xaxis_grob(combined_plot2, xhist, position = "top")


ggdraw(combined_plot2)

setwd("~/GitHub/Danish_muni/images")
ggsave(combined_plot2, filename= "fd_plot.eps",
       device = cairo_ps,
       width = 5.24,
       height = 4.56)



##################################################################
#
# Main Results -- changes in voter preferences on policy in four years
#
##################################################################

####
# pooled results
pool.mod <- plm(lag_SocDem1 ~ redvote+ log_pop, data = pan.dat , 
                model = "pooling")
coeftest(pool.mod, vcovNW(pool.mod, type = "HC1", cluster = "group"))

# reduced measure
pool.mod.red <- plm(lead_SocDem_reduced ~ redvote+ log_pop, data = pan.dat, 
                model = "pooling")
coeftest(pool.mod.red, vcovNW(pool.mod.red, type = "HC1", cluster = "group"))

#####
# fixed effects results

#full measure
fe.mod <- plm(lag_SocDem1 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(fe.mod, vcovNW(fe.mod, type = "HC1", cluster = "group"))

#reduced measure

fe.mod.red <- plm(lead_SocDem_reduced ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(fe.mod.red, vcovNW(fe.mod.red, type = "HC1", cluster = "group"))

####
# FD results

# four-year lead difference in outcome
# four-year lagged difference in covariates
fd.mod <- plm(fd_SocDem ~ fd_redvote + fd_pop, 
              data = pan.dat, model = "pooling", 
              effects = "time")

coeftest(fd.mod, vcovNW(fd.mod, type = "HC1", cluster = "group"))

#reduced measure
fd.mod.red <- plm(fd_SocDem_reduced ~ fd_redvote + fd_pop, 
              data = pan.dat, model = "pooling", 
              effects = "time")
coeftest(fd.mod.red, vcovNW(fd.mod.red, type = "HC1", cluster = "group"))

########
# PRESENT RESULTS

# extract
res <- data.frame(rbind(coef(pool.mod)[2], coef(fe.mod)[1], coef(fd.mod)[2],
                  coef(pool.mod.red)[2], coef(fe.mod.red)[1], coef(fd.mod.red)[2]),
                  rbind(sqrt(diag(vcovNW(pool.mod, type = "HC1")))[2],
                        sqrt(diag(vcovNW(fe.mod, type = "HC1")))[1],
                        sqrt(diag(vcovNW(fd.mod, type = "HC1")))[2],
                        sqrt(diag(vcovNW(pool.mod.red, type = "HC1")))[2],
                        sqrt(diag(vcovNW(fe.mod.red, type = "HC1")))[1],
                        sqrt(diag(vcovNW(fd.mod.red, type = "HC1")))[2]),
                  spec=c("Pooled", "Twoway Fixed Effects", "First-Difference"),
                  measure=c(rep("Full Measure",3), rep("Reduced Measure", 3)))
names(res)[1:2] <- c("PE", "SE")

res$spec <- factor(res$spec, levels = c("First-Difference", 
                                        "Twoway Fixed Effects",
                                        "Pooled"))

c_plot<-ggplot(res, aes(y = PE, x = spec)) +
  geom_point() +
  geom_errorbar(aes(ymin=PE-(1.96*SE),
                     ymax=PE+(1.96*SE)), width = 0) +
  facet_wrap(~measure, scales = "free_x") +
  theme_classic() +
  coord_flip() +
  labs(x=NULL,y="Coefficient on Electoral Support for Right-Wing Parties")

setwd("~/GitHub/Danish_muni/images")
ggsave(c_plot, filename = "coef_31082018.eps",
       device = cairo_ps, 
       width = 7.34,
       heigh = 4.56)

#####################################################################
#
# EFFECTS OVER TIME
#
#####################################################################


pan.dat$plac <- plm::lag(pan.dat$full.SocDem, 4)
pan.dat$SocDem1 <- plm::lag(pan.dat$full.SocDem, -1)
pan.dat$SocDem4 <- plm::lag(pan.dat$full.SocDem, -4)
pan.dat$SocDem8 <- plm::lag(pan.dat$full.SocDem, -8)
pan.dat$SocDem12 <- plm::lag(pan.dat$full.SocDem, -12)
pan.dat$SocDem16 <- plm::lag(pan.dat$full.SocDem, -16)
pan.dat$SocDem20 <- plm::lag(pan.dat$full.SocDem, -20)


mod1 <- plm(plac ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod1, vcovNW(mod1,type="HC1"))

mod2 <- plm(full.SocDem ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod2, vcovNW(mod2,type="HC1"))

mod3 <- plm(SocDem1 ~ redvote+ log_pop, data = pan.dat, model = "within", effects = "twoway")
coeftest(mod3, vcovNW(mod3,type="HC1"))

mod4 <- plm(SocDem4 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod4, vcovNW(mod4,type="HC1"))

mod5 <- plm(SocDem8 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod5, vcovNW(mod5,type="HC1"))

mod6 <- plm(SocDem12 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod6, vcovNW(mod6,type="HC1"))

mod7 <- plm(SocDem16 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod7, vcovNW(mod7,type="HC1"))

mod8 <- plm(SocDem20 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod8, vcovNW(mod8,type="HC1"))
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

#2way FE model
fe.inter <- plm(lag_SocDem1 ~ blue_vote*mayor_bin, data = pan.dat, model = "within", effects = "twoways")
coeftest(fe.inter, vcovNW(fe.inter, type = "HC1", cluster = "group"))

marg_fx <- marginsplotdf(fe.inter, xterm = "blue_vote", zterm = "mayor_bin1", zseq=0:1)

marg_fx2 <- marginsplotdf(fe.inter, xterm = "blue_vote", zterm = "mayor_bin2", zseq=0:1)
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

setwd("~/GitHub/Danish_muni/images")
ggsave(marg_plot, filename = "MargFX_31082018.eps")


# are changes in policy driven by the politicians, voters select?
# post-treatment mediator models

#2way FE models
pan.dat$mayor_bin <- relevel(pan.dat$mayor_bin, ref = 0)

fe.medi <- plm(lag_SocDem1 ~ blue_vote + mayor_bin+ log_pop, data = pan.dat, model = "within", effects = "twoways")
coeftest(fe.medi, vcovHC(fe.medi, type = "HC1", cluster = "group"))

medi_res <- data.frame(PE=coef(fe.medi)[1:3],
                       SE=sqrt(diag(vcovNW(fe.medi, type = "HC1")))[1:3])
medi_res <- rbind(medi_res, data.frame(PE=coef(fe.mod)[1],
                                       SE = sqrt(diag(vcovNW(fe.mod, type="HC1")))[1]))
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

ggsave(medi_plot, filename = "PostTreatControl.eps",
       device = cairo_ps)

#Acharya, Blackwell, Sen mediation
# this doesn't work ...
first <- lm(lag_SocDem1 ~ redvote + mayor_bin + log_pop, data = pan.dat)

form_main <- as.formula(lag_SocDem1  ~ redvote + mayor_bin + log_pop | mayor_bin)

direct <- sequential_g(formula = form_main,
                       first_mod = first,
                       data = df)





