
# preliminaries

# load packages
library(plm); library(AER); library(lfe); library(readr); library(ggplot2)
library(cowplot); library(haven); library(gridExtra)

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
pan.dat$lead_SocDem_reduced <- plm::lag(pan.dat$spend.SocDem,-4) #reduced measure
pan.dat$fd_SocDem <- pan.dat$lag_SocDem1 - pan.dat$full.SocDem #FD full measure
pan.dat$fd_SocDem_reduced <- pan.dat$lead_SocDem_reduced - pan.dat$spend.SocDem #FD reduced measure

# first-difference of red votes
#pan.dat$blue_vote <- pan.dat$redvote*-1
pan.dat$lag_redvote <- plm::lag(pan.dat$redvote, 4)
pan.dat$fd_redvote <- pan.dat$redvote - pan.dat$lag_redvote

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
coeftest(fe.mod, vcovHC(fe.mod, type = "HC1", cluster = "group"))

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

#################################################################
#
# Add some controls
#
#################################################################

setwd("C:/Users/ex-bce/Dropbox/effects on effects/responsiveness")

df <- read_dta("collectv2.dta")
df <- dplyr::select(df, muni, year, educ, immig, need, unemployment)
df <- subset(df, year > 1973)

df$year <- as.factor(df$year)

pan.dat2 <- dplyr::left_join(pan.dat, df, by = c("muni", "year"))
pan.dat2 <- pdata.frame(pan.dat2, index = c("muni", "year"))

# four year differences
pan.dat2$lag_educ <- plm::lag(pan.dat2$educ, 4)
pan.dat2$fd_educ <- pan.dat2$educ - pan.dat2$lag_educ

pan.dat2$lag_immig <- plm::lag(pan.dat2$immig, 4)
pan.dat2$fd_immig <- pan.dat2$immig - pan.dat2$lag_immig

pan.dat2$lag_unemployment <- plm::lag(pan.dat2$unemployment, 4)
pan.dat2$fd_unemp <- pan.dat2$unemployment - pan.dat2$lag_unemployment

pan.dat2$lag_need <- plm::lag(pan.dat2$need, 4)
pan.dat2$fd_need <- pan.dat2$need - pan.dat2$lag_need

#models

#pooling
pool.cont <- plm(lag_SocDem1 ~ redvote+ educ + immig +  unemployment, 
                 data = pan.dat2, model = "pooling")
coeftest(pool.cont, vcovHC(pool.cont, group = "cluster", type = "HC1"))

pool.cont.red <- plm(lead_SocDem_reduced ~ redvote+ educ + immig +  unemployment, 
                     data = pan.dat2, model = "pooling")
coeftest(pool.cont.red, vcovHC(pool.cont.red, group = "cluster", type = "HC1"))

#2way FE

fe.cont <- plm(lag_SocDem1 ~ redvote+ educ + immig + unemployment, 
               data = pan.dat2, model = "within", effects = "twoway")
coeftest(fe.cont, vcovHC(fe.cont, type = "HC1", cluster = "group"))

fe.cont.red <- plm(lead_SocDem_reduced ~ redvote+ educ + immig + unemployment, 
                   data = pan.dat2, model = "within", effects = "twoway")
coeftest(fe.cont.red, vcovHC(fe.cont.red, type = "HC1", cluster = "group"))

# four year differences

fd.cont <- plm(fd_SocDem ~ fd_redvote + fd_pop + fd_unemp + fd_immig + fd_educ,
               data = pan.dat2, model = "pooling", effect = "time")

coeftest(fd.cont, vcovHC(fd.cont, cluster = "group", type = "HC1"))

fd.cont.red <- plm(fd_SocDem_reduced ~ fd_redvote + fd_pop + fd_unemp + fd_immig + fd_educ,
                   data = pan.dat2, model = "pooling", effect = "time")

coeftest(fd.cont.red, vcovHC(fd.cont.red, cluster = "group", type = "HC1"))
########
# PRESENT RESULTS

# extract
res <- data.frame(rbind(coef(pool.mod)[2], coef(fe.mod)[1], coef(fd.mod)[2],
                  coef(pool.mod.red)[2], coef(fe.mod.red)[1], coef(fd.mod.red)[2],
                  coef(pool.cont)[2], coef(fe.cont)[1], coef(fd.cont)[2],
                  coef(pool.cont.red)[2], coef(fe.cont.red)[1], coef(fd.cont.red)[2]),
                  rbind(sqrt(diag(vcovHC(pool.mod, type = "HC1")))[2],
                        sqrt(diag(vcovHC(fe.mod, type = "HC1")))[1],
                        sqrt(diag(vcovHC(fd.mod, type = "HC1")))[2],
                        sqrt(diag(vcovHC(pool.mod.red, type = "HC1")))[2],
                        sqrt(diag(vcovHC(fe.mod.red, type = "HC1")))[1],
                        sqrt(diag(vcovHC(fd.mod.red, type = "HC1")))[2],##
                        sqrt(diag(vcovHC(pool.cont, type = "HC1")))[2],
                        sqrt(diag(vcovHC(fe.cont, type = "HC1")))[1],
                        sqrt(diag(vcovHC(fd.cont, type = "HC1")))[2],
                        sqrt(diag(vcovHC(pool.cont.red, type = "HC1")))[2],
                        sqrt(diag(vcovHC(fe.cont.red, type = "HC1")))[1],
                        sqrt(diag(vcovHC(fd.cont.red, type = "HC1")))[2]),
                  spec=c("Pooled", "Twoway\nFixed Effects", "First-Difference"),
                  cont=c(rep("Simple Model",6), rep("+ Controls", 6)),
                  measure=c(rep("Full Measure",3), rep("Reduced Measure", 3)))
names(res)[1:2] <- c("PE", "SE")

res$spec <- factor(res$spec, levels = c("First-Difference", 
                                        "Twoway\nFixed Effects",
                                        "Pooled"))
cols = c("+ Controls" = "grey", "Simple Model" = "black")

res$cont <- factor(res$cont, levels = c("Simple Model", "+ Controls"))


df1 <- subset(res, measure == "Full Measure")
df2 <- subset(res, measure == "Reduced Measure")


p1 <- ggplot(df1, aes(y = PE, x = spec, colour = cont)) +
  geom_point(position = position_dodge(1/2)) +
  geom_errorbar(aes(ymin=PE-(1.96*SE),
                    ymax=PE+(1.96*SE)), width = 0, 
                position = position_dodge(1/2)) +
  theme_classic() + 
  scale_y_continuous(limits = c(-.1,0.4))+
  coord_flip() +
  labs(x=NULL,y=NULL) +
  scale_colour_manual(name = " ", values = cols) +
  theme(legend.position="bottom") +
  geom_hline(yintercept = 0, lty = 2) +
  ggtitle("A: Full Measure")

p1



p2 <-ggplot(df2, aes(y = PE, x = spec, colour = cont)) +
  geom_point(position = position_dodge(1/2)) +
  geom_errorbar(aes(ymin=PE-(1.96*SE),
                    ymax=PE+(1.96*SE)), width = 0, 
                position = position_dodge(1/2)) +
  theme_classic() + 
  scale_y_continuous(limits = c(-.1,2.5))+
  scale_x_discrete(labels = NULL) +
  coord_flip() +
  labs(x=NULL,y=NULL) +
  scale_colour_manual(name = " ", values = cols) +
  theme(legend.position="bottom") +
  geom_hline(yintercept = 0, lty = 2) +
  ggtitle("B: Reduced Measure")

p2

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


mylegend<-g_legend(p1)

p3 <- grid.arrange(arrangeGrob(p1 + theme(legend.position="none"),
                               p2 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1), 
                   bottom = "Coefficient on Electoral Support for Right-Wing Parties")

# c_plot<-ggplot(res, aes(y = PE, x = spec, colour = cont)) +
#   geom_point(position = position_dodge(1/2)) +
#   geom_errorbar(aes(ymin=PE-(1.96*SE),
#                      ymax=PE+(1.96*SE)), width = 0, 
#                 position = position_dodge(1/2)) +
#   facet_wrap(~measure, scales = "free_x") +
#   theme_classic() + 
#   scale_y_continuous(limits = c(-.1,2.5))+
#   coord_flip() +
#   labs(x=NULL,y="Coefficient on Electoral Support for Right-Wing Parties") +
#   scale_colour_manual(name = "Specification", values = cols) +
#   theme(legend.position="bottom") +
#   geom_blank(data=dummy_df, inherit.aes = F)
# c_plot

setwd("~/GitHub/Danish_muni/images")
ggsave(c_plot, filename = "coef_11092018.eps",
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

test <- plm(plm::lag(redvote,-4) ~ full.SocDem,data=pan.dat,
            model="within", effects="twoway")
summary(test)

mod1 <- plm(plac ~ redvote + log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod1, vcovHC(mod1,type="HC1", cluster = "group"))

mod2 <- plm(full.SocDem ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod2, vcovHC(mod2,type="HC1", cluster = "group"))

# mod3 <- plm(SocDem1 ~ redvote+ log_pop, data = pan.dat, model = "within", effects = "twoway")
# coeftest(mod3, vcovHC(mod3,type="HC1", cluster = "group"))

mod4 <- plm(SocDem4 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod4, vcovHC(mod4,type="HC1", cluster = "group"))

mod5 <- plm(SocDem8 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod5, vcovHC(mod5,type="HC1", cluster = "group"))

mod6 <- plm(SocDem12 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod6, vcovHC(mod6,type="HC1", cluster = "group"))

mod7 <- plm(SocDem16 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod7, vcovNW(mod7,type="HC1"))

mod8 <- plm(SocDem20 ~ redvote+ log_pop, data = pan.dat , model = "within", effects = "twoway")
coeftest(mod8, vcovNW(mod8,type="HC1"))



####################################################################
#
# Granger test for reverse causality
#
######################################################################

pan.dat$lag_SocDem <- plm::lag(pan.dat$full.SocDem, 1)
pan.dat$lag_SocDem2 <- plm::lag(pan.dat$full.SocDem, 2)
pan.dat$lag_SocDem3 <- plm::lag(pan.dat$full.SocDem, 3)
pan.dat$lag_SocDem4 <- plm::lag(pan.dat$full.SocDem, 4)


grang <- plm(redvote ~ lag_SocDem + log_pop, model = "within", effects = "twoway",
             data = pan.dat)

coeftest(grang, vcovHC(grang, cluster = "group", type = "HC1"))


grang2 <- plm(redvote ~ lag_SocDem2 + log_pop, model = "within", effects = "twoway",
             data = pan.dat)
coeftest(grang2, vcovHC(grang2, cluster = "group", type = "HC1"))

grang3 <- plm(redvote ~ lag_SocDem3+ log_pop, model = "within", effects = "twoway",
              data = pan.dat)
coeftest(grang3, vcovHC(grang3, cluster = "group", type = "HC1"))

grang4 <- plm(redvote ~ lag_SocDem4+ log_pop, model = "within", effects = "twoway",
              data = pan.dat)
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

setwd("~/GitHub/Danish_muni/images")
ggsave(g_plot, filename = "granger_11092018.eps",
       device = cairo_ps, 
       width = 4.55,
       height = 4.55)

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

######################################################################
#
# Item-by-item regressions
#
######################################################################

item_df <- pan.dat2[, c(3:4, 13, 25:33, 35)]
#item_df <- pdata.frame(item_df, index =c("muni", "year"))

item_res <- list()

for(i in 4:13){
  item_df[,i] <- plm::lag(item_df[,i], -4)
  
  mod <- plm(item_df[,i] ~ redvote, data = item_df, 
             model = "within", effects = "twoway")
  
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


setwd("~/GitHub/Danish_muni/images")
ggsave(item_p, filename = "ItemByItem_11092018.eps")






























































