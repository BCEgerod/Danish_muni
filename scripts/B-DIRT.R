library(haven); library(R2jags); library(MCMCpack); library(reshape); library(reshape2) 
library(plyr); library(dplyr); library(ggplot2); library(runjags)

setwd("C:/Users/ex-bce/Desktop/responsiveness/bayes results")
load("full_SocDem.RData")
load("spending_SocDem.RData")

setwd("C:/Users/ex-bce/Desktop/responsiveness")

df <- read_dta("collect.dta")


df$spend.cap <- df$spend/df$estpop
df$aflastningsophold <- as.numeric(as.character(df$aflastningsophold))

new.df <- df[,c(7, 8, 13:20, 22)]

new.df <- df[,c(1:2, 10, 11, 18, 24:34, 68]


new.df <- as.data.frame(new.df)

for(i in 1:ncol(new.df)){
  crnt.var <- as.data.frame(new.df[, i])
  df[,ncol(df)+1] <- as.data.frame(scale(crnt.var, center = T, scale = T))
  names(df)[ncol(df)] <- paste(names(new.df)[i], "_scale", sep = "")
  }

new.df <- subset(df, year > 1973)

##
df$muni_year <- paste(df$muni, df$year, sep = "-")

length(unique(df$muni))

# order df

new.df <- new.df[order( new.df$muni, new.df$year ),]


# new.df2 <- dplyr::select(new.df2, incometax_scale,  propertytax_scale, spend.cap_scale,
#                          muniname,muni, year)
# write.csv(new.df2, "data_til_IRT.csv")
# 
# new.df2 <- subset(new.df2, is.na(incometax_scale) == F |
#                     is.na(propertytax_scale) == F |
#                     is.na(spend.cap_scale) == F)
# new.df2 <- new.df2[order( new.df2$muni, new.df2$year ),]
# 
# new.df2 <- na.omit(new.df2)
# 
# # find row numbes of Brøndby and Gentofte in the first year of the panel
# which(new.df2$muniname == "Brøndby" & new.df2$year == min(new.df2$year))
# which(new.df2$muniname == "Gentofte" & new.df2$year == min(new.df2$year))
# 
# bron<-new.df2[which(new.df2$muniname == "Brøndby" & new.df2$year == 1974),]
# gen<-new.df2[which(new.df2$muniname == "Gentofte" & new.df2$year == 1974),]
# 
## set up model
muni_year <- nrow(new.df)
Nmuni <- length(unique(new.df$muniname))
Nyear <- length(unique(new.df$year))
y <- as.matrix(dplyr::select(new.df, incometax_scale, propertytax_scale, spend.cap_scale))

muni <- new.df$muni
year <- new.df$year


# new.df2$prior <- rep(0,nrow(new.df2))
# 
# new.df2$prior <- ifelse(new.df2$muniname == "Brøndby" & new.df2$year == 1978, -1, 
#                         ifelse(new.df2$muniname == "Gentofte" & new.df2$year == 1978, 1, new.df2$prior))
# 

mod <- "model{
  for (i in 1:muni_year){
      for(j in 1:Nitems){
        y[i,j] ~ dnorm(MuY[i,j], Inv_sig_e1[j])
      
        MuY[i,j]<-beta[j]*Z[i] + alpha[j] 
     
      }
        Z[i] ~ dnorm(0,Inv_sig_f1)

}
  
    #  Z[25, 1978] <- -1.0  ## Brøndby 1974 on the left
    #  Z[73, 1978] <- 1.0   ## Gentofte 1974 on the right
    # 
    # for(i in 1:24){
    #     Z[muni[i],year[i]] <- dnorm(0, Inv_sig_f1)
    # }
    # 
    # for(i in 26:72){
    #     Z[muni[i],year[i]] <- dnorm(0, Inv_sig_f1)
    # }
    # 
    # for(i in 74:muni_year){
    #     Z[muni[i],year[i]] <- dnorm(0, Inv_sig_f1)
    # }


  #Prior distribution
  for (j in 1:Nitems){
    beta[j] ~ dnorm(0,0.1)T(0,)
    alpha[j] ~ dnorm(0,0.1)

  }
  
  for (j in 1:Nitems){
    Inv_sig_e1[j] ~ dgamma(0.1,0.1)
  }
  
  Inv_sig_f1 ~ dgamma(0.1,0.1)
}"



mod <- "model{
  for (i in 1:muni_year){
    for(j in 1:3){
      y[i,j] ~ dnorm(MuY[i,j], Inv_sig_e1[j])

      MuY[i,j]<-beta[j]*Z[muni[i], year[i]] + alpha[j] 
    }
    #Z[muni[i],year[i]] ~ dnorm(0,Inv_sig_f1)
  }

for(j in 1:Nmuni){
  Z[j,1] ~ dnorm(0,Inv_sig_f1)
    for(i in 2:Nyear){
      Z[j,i] ~ dnorm(Z[j, i-1],Inv_sig_f1)

    }
}

#Prior distribution
  for (j in 1:3){
    alpha[j] ~ dnorm(0,0.001)
    beta[j] ~ dnorm(0,0.1)
  }

  for (j in 1:3){
    Inv_sig_e1[j] ~ dgamma(0.1,0.1)
  }

  Inv_sig_f1 ~ dgamma(0.1,0.1)

}"


muni.data <- list(
  
  muni_year = muni_year,
  Nitems = 3,
  y = y,
  Nmuni = Nmuni,
  Nyear = Nyear,
  muni = new.df$muni,
  year = new.df$year
  
)

out <- run.jags(mod, data = muni.data, monitor = c("alpha", "beta", "Z","delta"), burnin = 5000, sample = 5000, 
                thin = 5, summarise = F, n.chains = 3)


#####
crnt.res1 <- as.data.frame(out$mcmc[[1]])
crnt.res1 <- crnt.res1[, grep(pattern = "Z\\[", x = names(crnt.res1))]

crnt.res2 <- as.data.frame(out$mcmc[[2]])
crnt.res2 <- crnt.res2[, grep(pattern = "Z\\[", x = names(crnt.res2))]

crnt.res3 <- as.data.frame(out$mcmc[[3]])
crnt.res3 <- crnt.res3[, grep(pattern = "Z\\[", x = names(crnt.res3))]

muni1 <- matrix(NA, nrow =3, ncol = ncol(crnt.res1))
muni2 <- matrix(NA, nrow =3, ncol = ncol(crnt.res2))
muni3 <- matrix(NA, nrow =3, ncol = ncol(crnt.res3))


for(j in 1:ncol(crnt.res1)){
  muni1[,j]  <- quantile(crnt.res1[, j], probs = c(0.025, 0.5, 0.975))
  muni2[,j] <- quantile(crnt.res2[, j], probs = c(0.025, 0.5, 0.975))
  muni3[,j] <- quantile(crnt.res3[, j], probs = c(0.025, 0.5, 0.975))
}

muni1 <- as.data.frame(t(muni1))
muni2 <- as.data.frame(t(muni2))
muni3 <- as.data.frame(t(muni3))

muni <- data.frame(lwr95 = (muni1$V1 + muni2$V1 + muni3$V1)/3,
                   spend.SocDem = (muni1$V2 + muni2$V2 + muni3$V2)/3,
                   upr95 = (muni1$V3 + muni2$V3 + muni3$V3)/3)

new.df <- data.frame(new.df, muni)

# coefficients

b_a <- as.data.frame(out$mcmc[[1]])[,1:6]
b_a2 <- as.data.frame(out$mcmc[[2]])[,1:6]
b_a3 <- as.data.frame(out$mcmc[[3]])[,1:6]

parm1 <- matrix(NA, nrow =3, ncol = 6)
parm2 <- matrix(NA, nrow =3, ncol = 6)
parm3 <- matrix(NA, nrow =3, ncol = 6)

for(j in 1:6){
  parm1[,j]  <- quantile(b_a[, j], probs = c(0.025, 0.5, 0.975))
  parm2[,j] <- quantile(b_a2[, j], probs = c(0.025, 0.5, 0.975))
  parm3[,j] <- quantile(b_a3[, j], probs = c(0.025, 0.5, 0.975))
}

parm1 <- as.data.frame(t(parm1))
parm2 <- as.data.frame(t(parm2))
parm3 <- as.data.frame(t(parm3))


parm <- data.frame(alpha1 = (parm1$V1 + parm2$V1 + parm3$V1)/3,
                   alpha2 = (parm1$V2 + parm2$V2 + parm3$V2)/3,
                   alpha3 = (parm1$V3 + parm2$V3 + parm3$V3)/3,
                   beta1 = (parm1$V4 + parm2$V4 + parm3$V4)/3,
                   beta2 = (parm1$V5 + parm2$V5 + parm3$V5)/3,
                   beta3 = (parm1$V6 + parm2$V6 + parm3$V6)/3)

write.csv(parm, file = "IRT_parms.csv")

save(out, file = "spending_SocDem.RData")

## model with all items

for(i in c(25:31)){
  var.name <- paste(names(new.df)[i],"_rev",sep = "")
  new.df[,ncol(new.df)+1] <- min(new.df[,i],na.rm = T) - new.df[,i]
  names(new.df)[ncol(new.df)] <- var.name
  rm(var.name)
}

y2 <- as.matrix(new.df[,c(23:25,33,38:43)])

muni.data2 <- list(
  
  muni_year = muni_year,
  Nitems = 10,
  y = y2,
  Nmuni = Nmuni,
  Nyear = Nyear,
  muni = new.df$muni,
  year = new.df$year
)

all_items <- run.jags(mod, data = muni.data2, monitor = c("alpha", "beta", "Z"), burnin = 5000, sample = 5000, 
                      thin = 5, summarise = F, n.chains = 3)


all.res1 <- as.data.frame(all_items$mcmc[[1]])
all.res1 <- all.res1[, grep(pattern = "Z\\[", x = names(all.res1))]

all.res2 <- as.data.frame(all_items$mcmc[[2]])
all.res2 <- all.res2[, grep(pattern = "Z\\[", x = names(all.res2))]

all.res3 <- as.data.frame(all_items$mcmc[[3]])
all.res3<- all.res3[, grep(pattern = "Z\\[", x = names(all.res3))]

muni1 <- matrix(NA, nrow =3, ncol = ncol(all.res1))
muni2 <- matrix(NA, nrow =3, ncol = ncol(all.res2))
muni3 <- matrix(NA, nrow =3, ncol = ncol(all.res3))


for(j in 1:ncol(all.res1)){
  muni1[,j]  <- quantile(all.res1[, j], probs = c(0.025, 0.5, 0.975))
  muni2[,j] <- quantile(all.res2[, j], probs = c(0.025, 0.5, 0.975))
  muni3[,j] <- quantile(all.res3[, j], probs = c(0.025, 0.5, 0.975))
}

muni1 <- as.data.frame(t(muni1))
muni2 <- as.data.frame(t(muni2))
muni3 <- as.data.frame(t(muni3))

muni <- data.frame(full_lwr95 = (muni1$V1 + muni2$V1 + muni3$V1)/3,
                   full.SocDem = (muni1$V2 + muni2$V2 + muni3$V2)/3,
                   full_upr95 = (muni1$V3 + muni2$V3 + muni3$V3)/3)

new.df <- data.frame(new.df, muni)


# coefficients

b_a <- as.data.frame(all_items$mcmc[[1]])[,1:20]
b_a2 <- as.data.frame(all_items$mcmc[[2]])[,1:20]
b_a3 <- as.data.frame(all_items$mcmc[[3]])[,1:20]

parm1 <- matrix(NA, nrow =3, ncol = 20)
parm2 <- matrix(NA, nrow =3, ncol = 20)
parm3 <- matrix(NA, nrow =3, ncol = 20)

for(j in 1:20){
  parm1[,j]  <- quantile(b_a[, j], probs = c(0.025, 0.5, 0.975))
  parm2[,j] <- quantile(b_a2[, j], probs = c(0.025, 0.5, 0.975))
  parm3[,j] <- quantile(b_a3[, j], probs = c(0.025, 0.5, 0.975))
}

parm1 <- as.data.frame(parm1)
parm2 <- as.data.frame(parm2)
parm3 <- as.data.frame(parm3)


parm <- data.frame(alpha1 = (parm1$V1 + parm2$V1 + parm3$V1)/3,
                   alpha2 = (parm1$V2 + parm2$V2 + parm3$V2)/3,
                   alpha3 = (parm1$V3 + parm2$V3 + parm3$V3)/3,
                   alpha4 = (parm1$V4 + parm2$V4 + parm3$V4)/3,
                   alpha5 = (parm1$V5 + parm2$V5 + parm3$V5)/3,
                   alpha6 = (parm1$V6 + parm2$V6 + parm3$V6)/3,
                   alpha7 = (parm1$V7 + parm2$V7 + parm3$V7)/3,
                   alpha8 = (parm1$V8 + parm2$V8 + parm3$V8)/3,
                   alpha9 = (parm1$V9 + parm2$V9 + parm3$V9)/3,
                   alpha10 = (parm1$V10 + parm2$V10 + parm3$V10)/3,
                   beta1 = (parm1$V11 + parm2$V11 + parm3$V11)/3,
                   beta2 = (parm1$V12 + parm2$V12 + parm3$V12)/3,
                   beta3 = (parm1$V13 + parm2$V13 + parm3$V13)/3,
                   beta4 = (parm1$V14 + parm2$V14 + parm3$V14)/3,
                   beta5 = (parm1$V15 + parm2$V15 + parm3$V15)/3,
                   beta6 = (parm1$V16 + parm2$V16 + parm3$V16)/3,
                   beta7 = (parm1$V17 + parm2$V17 + parm3$V17)/3,
                   beta8 = (parm1$V18 + parm2$V18 + parm3$V18)/3,
                   beta9 = (parm1$V19 + parm2$V19 + parm3$V19)/3,
                   beta10 = (parm1$V20 + parm2$V20 + parm3$V20)/3)
names(parm) <- names(new.df)[c(23:25,33,38:43)]

setwd("C:/Users/ex-bce/Desktop/responsiveness/bayes results")
write.csv(parm, file = "All_IRT_parms.csv")

save(all_items, file = "full_SocDem.RData")

rm(list=setdiff(ls(), "new.df"))
##

write.csv(new.df, filename = "")



###
# face validity

agg.muni <- new.df %>%
  group_by(muniname) %>%
  summarise(av.score = mean(spend.SocDem, na.rm = T) )


ggplot(agg.muni, aes(x = reorder(muniname, av.score), y = av.score)) +
  geom_bar(stat = "identity") +
  coord_flip()


agg.muni <- agg.muni[order( agg.muni$av.score),]

most.soc <- agg.muni[218:268,]
most.soc$SocDem <- "51 Most SocDem Municipalities"

least.soc <- agg.muni[1:51,]
least.soc$SocDem <- "51 Least SocDem Municipalities"

agg.muni2 <- rbind(most.soc, least.soc)

p1 <- ggplot(agg.muni2, aes(x = reorder(muniname, av.score), y = av.score)) +
            geom_bar(stat = "identity") +
            coord_flip() + 
            facet_wrap(~ SocDem, scales = "free_y") +
            labs(x = " ", y = "Fiscal Social Democratism \n (Averaged over 1978-2006)") +
            theme_bw() + theme( axis.line = element_blank(), # axis.line = element_line(colour = "black"),
                               panel.border = element_blank(), plot.title = element_text(hjust = 0.5),
                               axis.text.y = element_text(size = 6.5))
          
ggsave(plot = p1, filename = "socialdemocratism.eps", device = cairo_ps)


# all items

agg.all <- new.df %>%
  group_by(muniname) %>%
  summarise(av.score = mean(full.SocDem, na.rm = T) )

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
          labs(x = " ", y = "Fiscal Social Democratism \n (Averaged over 1974-2006)") +
          theme_bw() + theme( axis.line = element_blank(), # axis.line = element_line(colour = "black"),
                              panel.border = element_blank(), plot.title = element_text(hjust = 0.5),
                              axis.text.y = element_text(size = 6.5))

p3 <- grid.arrange(p1, p2)

setwd("C:/Users/ex-bce/Desktop/responsiveness/images")
ggsave(plot = p3, filename = "socialdemocratism.eps", device = cairo_ps)



ggplot(new.df, aes(x = redvote, y = full.SocDem)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  facet_wrap( ~ year)


## preliminary models
library(plm); library(AER)

#df <- left_join(new.df2, df, by = c("muniname", "year"))

model <- lm(spend.SocDem ~ redvote, data = new.df)
summary(model)

p.coef <- model$coefficients[2]
p.se<- sqrt(diag(vcov(model)))[2]

#
model <- lm(redvote ~  factor(year), data = new.df)
coeftest(model, vcovHC(model))

# fixed effects
pan.dat <- pdata.frame(new.df, index = c("muniname", "year"))

fe.mod <- plm(plm::lag(spend.SocDem,-4) ~ redvote+ log(estpop), data = pan.dat, model = "within", effects = "twoways")
summary(fe.mod)
coeftest(fe.mod, vcovHC(fe.mod, type = "HC1", cluster = "group"))


fe.coef <- fe.mod$coefficients[1]
fe.se<- sqrt(diag(vcov(fe.mod)))[1]

# lag y

fe.lag <- plm(plm::lag(spend.SocDem,-4) ~ plm::lag(spend.SocDem,-2) + plm::lag(spend.SocDem, -3) + 
                redvote + log(estpop), data = pan.dat, model = "within", effects = "twoways")
summary(fe.lag)
coeftest(fe.lag, vcovHC(fe.lag, type = "HC1", cluster = "group"))
coeftest(fe.lag, vcovBK(fe.lag))

fe.coef2 <- fe.lag$coefficients[3]
fe.se2<- sqrt(diag(vcov(fe.lag)))[3]

# first difference

fd.lag <- plm(plm::lag(spend.SocDem, -4) ~  redvote +  plm::lag(spend.SocDem,-3)  + log(estpop) + factor(year), data = pan.dat, model = "fd")
summary(fd.lag)
coeftest(fd.lag, vcovBK(fd.lag))

fd.coef <- fd.lag$coefficients[2]
fd.se <- sqrt(diag(vcovBK(fd.lag)))[2]

# interacted fixed effects
library(simcf); library(phtt)
mod <- plm(spend.SocDem ~ redvote + year + muniname, data = pan.dat, model = "pooling", effect = "individual")
used <- extractdata(mod, data = pan.dat, na.rm = T)

used <- dplyr::select(used, muniname, year, spend.SocDem, redvote)

used_cast <- reshape2::dcast(used[,c(1,2,3)], formula =  used$year ~ used$muniname, value.var = "spend.SocDem")
spend_cast <- as.matrix(used_cast[,-1])
used_cast <- reshape2::dcast(used[,c(1,2,4)], formula =  used$year ~ used$muniname, value.var = "redvote")
red_cast <- as.matrix(used_cast[,-1])

mod1 <- KSS(spend_cast ~ red_cast, additive.effects = "twoways")
summary(mod1)

interFE <- coef(mod1)$Slope.Coef
interFE.se <- sqrt(diag(mod1$beta.V))

## combine estimates
betas <- rbind(p.coef, fe.coef, fe.coef2, fd.coef)
SEs <- rbind(p.se, fe.se, fe.se2, fd.se)

fx <- data.frame(betas, SEs)
names(fx) <- c("PE", "SE")

fx$lwr95 <- fx$PE - 1.96*fx$SE 
fx$lwr90 <- fx$PE - 1.68*fx$SE 
fx$upr90 <- fx$PE + 1.68*fx$SE 
fx$upr95 <- fx$PE + 1.96*fx$SE 

fx$spec <- c("Pooled", "Two-way fixed effects", "+ Lag DV", "First-differences")
fx$order <- 1:4


p2<- ggplot(fx, aes(x = PE, y = reorder(spec, -order))) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr95, xmax = upr95), size = .7, height = 0) + 
  geom_errorbarh(aes(xmin = lwr90, xmax = upr90), size = 1.4, height = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), axis.line = element_blank(), # axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  geom_ribbon(data = subset(fx, spec != "pooled"),aes( ymin = 2, ymax = 5),
              alpha = 0.3) +
  geom_vline(xintercept = 0, lty = 2, color = "red") +
  labs(x = "Coefficient on vote share \n of left-wing parties", y = " ")

ggsave(plot = p2, filename = "coef.eps", device = cairo_ps)


# models with full index
model <- plm(plm::lag(full.SocDem,-0) ~ redvote, data = pan.dat, model = "pooling")
summary(model)

p.coef <- model$coefficients[2]
p.se<- sqrt(diag(vcov(model)))[2]

#
model <- plm(redvote ~  factor(year), data = new.df, model = "pooling")
coeftest(model, vcovHC(model))

# fixed effects
pan.dat <- pdata.frame(new.df, index = c("muniname", "year"))

fe.mod.full <- plm(plm::lag(full.SocDem,-4) ~ redvote+ log(estpop), data = pan.dat, model = "within", effects = "twoways")
summary(fe.mod.full)
coeftest(fe.mod.full, vcovHC(fe.mod.full, type = "HC1", cluster = "group"))


fe.coef.full <- fe.mod.full$coefficients[1]
fe.se.full<- sqrt(diag(vcov(fe.mod.full)))[1]

# lag y

fe.lag.full <- plm(plm::lag(full.SocDem,-4) ~ plm::lag(full.SocDem,-2) + plm::lag(full.SocDem, -3) + 
                redvote + log(estpop), data = pan.dat, model = "within", effects = "twoways")
summary(fe.lag.full)
coeftest(fe.lag.full, vcovHC(fe.lag.full, type = "HC1", cluster = "group"))
coeftest(fe.lag.full, vcovBK(fe.lag.full))

fe.coef.full2 <- fe.lag.full$coefficients[3]
fe.se.full2<- sqrt(diag(vcovBK(fe.lag.full)))[3]

# first difference

fd.lag.full <- plm(plm::lag(full.SocDem, -4) ~  redvote +  plm::lag(full.SocDem,-3)  + log(estpop) + factor(year), data = pan.dat, model = "fd")
summary(fd.lag.full)
coeftest(fd.lag.full, vcovBK(fd.lag.full))

fd.coef.full <- fd.lag.full$coefficients[2]
fd.se.full <- sqrt(diag(vcov(fd.lag.full)))[2]

# interacted fixed effects
library(simcf); library(phtt)
mod <- plm(plm::lag(used$full.SocDem,-4) ~ redvote + year + muniname, data = pan.dat, model = "pooling", effect = "individual")
used <- extractdata(mod, data = pan.dat, na.rm = T)

used <- dplyr::select(used, muniname, year, full.SocDem, redvote)
used$full.SocDem.lag <- 

used_cast <- reshape2::dcast(used[,c(1,2,3)], formula =  used$year ~ used$muniname, value.var = "full.SocDem")
spend_cast <- as.matrix(used_cast[,-1])
used_cast <- reshape2::dcast(used[,c(1,2,4)], formula =  used$year ~ used$muniname, value.var = "redvote")
red_cast <- as.matrix(used_cast[,-1])

mod1.full <- KSS(spend_cast ~ red_cast, additive.effects = "twoways")
summary(mod1.full)

interFE.full <- coef(mod1.full)$Slope.Coef
interFE.se.full <- sqrt(diag(mod1.full$beta.V))

###
#gather and plot estimates

betas2 <- rbind(p.coef, fe.coef.full, fe.coef.full2, fd.coef.full)
SEs2 <- rbind(p.se, fe.se.full, fe.se.full2, fd.se.full)

fx2 <- data.frame(betas2, SEs2)
names(fx2) <- c("PE2", "SE2")

fx2$lwr95 <- fx2$PE2 - 1.96*fx2$SE2 
fx2$lwr90 <- fx2$PE2 - 1.68*fx2$SE2 
fx2$upr90 <- fx2$PE2 + 1.68*fx2$SE2 
fx2$upr95 <- fx2$PE2 + 1.96*fx2$SE2 

fx2$spec <- c("Pooled", "Two-way fixed effects", "+ Lag DV", "First-differences")
fx2$order <- 1:4


p3<- ggplot(fx, aes(x = PE, y = reorder(spec, -order))) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr95, xmax = upr95), size = .7, height = 0) + 
  geom_errorbarh(aes(xmin = lwr90, xmax = upr90), size = 1.4, height = 0) +
  theme_bw() + theme(panel.grid.minor = element_blank(), axis.line = element_blank(), # axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5)) +
  geom_ribbon(data = subset(fx, spec != "pooled"),aes( ymin = 2, ymax = 5),
              alpha = 0.3) +
  geom_vline(xintercept = 0, lty = 2, color = "red") +
  labs(x = "Coefficient on vote share \n of left-wing parties", y = " ")


# tile!!

library(tile)

#plot small index

muni95CI_small <- ropeladder(x = fx$PE,
                       lower = fx$lwr95,
                       upper = fx$upr95,
                       labels= fx$spec, fontsize = 5,
                       group = 1, #set group to overplot CIs
                       lwd = .25,
                       plot=1
)

# 90 pct CIs
muni90CI_small <- ropeladder(x = fx$PE,
                       lower = fx$lwr90,
                       upper = fx$upr90,
                       labels=fx$spec, fontsize = 5, 
                       group = 1, # same group as above
                       plot=1
)

NullMark <- linesTile(x = c(0,0),
                       y = c(0,1),
                       lty = "dashed",
                       plot = 1
)


#plot full index
muni95CI_full <- ropeladder(x = fx2$PE,
                     lower = fx2$lwr95,
                     upper = fx2$upr95,
                     labels= NULL, fontsize = 5,
                     group = 1, #set group to overplot CIs
                     lwd = .25,
                     plot=2
)

# 90 pct CIs
muni90CI_full <- ropeladder(x = fx2$PE,
                         lower = fx2$lwr90,
                         upper = fx2$upr90,
                         labels=NULL, fontsize = 5, 
                         group = 1, # same group as above
                         plot=2
)

NullMark2 <- linesTile(x = c(0,0),
                            y = c(0,1),
                            lty = "dashed",
                            plot = 2
)

one_sd_full <- sd(new.df$full.SocDem)
one_sd_small <- sd(new.df$spend.SocDem)


setwd("C:/Users/ex-bce/Desktop/responsiveness/images")

tile(muni95CI_small, muni90CI_small, muni95CI_full, muni90CI_full, NullMark, NullMark2,
     #limits = (-0.05, 0.35),
     #fontsize = 2,
     xaxis = list(fontsize = 4, add = T),
     #width=list(null=3),
     output = list(file="coef_plot", type = "CairoPS"),          
     xaxistitle = list(labels="Coefficient on vote share of left-wing parties", fontsize = 1, cex = .4),
     topaxis= list(at1 = one_sd_small*c(0, 0.25, 0.50, 0.75, 1),
                   labels1 = c( "0x", " ", "0.5x", " ", "1x"),
                    at2 = one_sd_full*c(0, 0.25, 0.50, 0.75, 1),
                   labels2 = c( "0x", " ", "0.5x", " ", "1x"),
                   add = TRUE, fontsize = 4),
     topaxistitle = list(labels="Std. Dev. of Fiscal Social Democratism", fontsize = 1, cex = .4),
     gridlines=list(type="t", col = "lightgrey"), clip = "off",
     columntitle = list(labels = c("A: 3-item Index",
                                   "B: 10-item Indec"), add = TRUE, type = "all",
                        y = .3, cex = .5)
)


# cumulative long-run effects

nsim <- 500
boots <- matrix(nrow = nsim, ncol = 2)
helper <-   plm(plm::lag(full.SocDem,-4) ~ plm::lag(full.SocDem,-2) + plm::lag(full.SocDem, -3) + 
                              redvote + log(estpop) + muniname + year, data = pan.dat, model = "pooling")

used <- extractdata(helper, data = pan.dat, na.rm = T)

for(i in 1:nsim){
  print(paste("Running bootstrap", i, sep = " "))
  samp <- used[sample(1:nrow(used),
                              replace = TRUE),]
 samp <- pdata.frame(samp, index = c("muniname", "year"))
  #samp <- subset(samp, is.na(redvote) == F)
  
  boot.mod <- plm(plm::lag(full.SocDem,-4) ~ plm::lag(full.SocDem,-2) + plm::lag(full.SocDem, -3) + 
                         redvote + log(estpop), data = samp, model = "within", effects = "twoways")
  #coeftest(boot.mod, vcovBK(boot.mod))
  boots[i, 1] <- coef(boot.mod)[3]
      
  boots[i, 2] <- (1/coef(boot.mod)[2])*coef(boot.mod)[3]

}


boots <- as.data.frame(boots)

boots2 <- data.frame(short = quantile(boots$V1, probs = c(0.025, 0.5, 0.975), na.rm = T),
                     long = quantile(boots$V2, probs = c(0.025, 0.5, 0.975), na.rm = T))



###
pack<-c("rgdal", "classInt", "GISTools", "maps", "dismo", "raster", "foreign", "shape")
lapply(pack, require, character.only=T)
library(stringr)

data <- "C:/Users/ex-bce/Desktop/responsiveness/KOMMUNAL_SHAPE_UTM32-EUREF89"

muni <- readOGR(dsn=data,
                        layer="Kommune")
# time <- str_split(muni$til, pattern = "-")
# time <- do.call("rbind", time)
# 
# time2 <- str_split(muni$fra, pattern = "-")
# time2 <- do.call("rbind", time2)
# 
# muni$tid <- as.numeric(as.character(time[,1]))
# muni$tid2 <- as.numeric(as.character(time2[,1]))


muni <- subset(muni, duplicated(navn) == F)
#muni <- subset(muni, tid2 < 2007 | tid2 )


#Ø
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Ã~",
                       replacement = "Ø")
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Ã",
                       replacement = "ø")
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Ã¸",
                       replacement = "ø")
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "ø¸",
                       replacement = "ø")


#Å
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Ã.",
                       replacement = "Å")
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "ø.",
                       replacement = "Å")
#å
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Ã¥",
                       replacement = "å")
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "ø¥",
                       replacement = "å")
#Æ
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "ø???",
                       replacement = "Æ")
#æ
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Ã¦",
                       replacement = "æ")
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "ø¦",
                       replacement = "æ")

#Kommune
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = " Kommune",
                       replacement = "")

# last corrections!
muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Lyngby-Tårbæk",
                       replacement = "Lyngby-Taarbæk")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Faxe",
                       replacement = "Fakse")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Lyngby-Tårbæk",
                       replacement = "Lyngby-Taarbæk")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Holmegård",
                       replacement = "Holmegaard")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Nykøbing Falsters",
                       replacement = "Nykøbing Falster")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Åkirkeby",
                       replacement = "Aakirkeby")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Fåborg",
                       replacement = "Faaborg")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Hårby",
                       replacement = "Haarby")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Nørre Åby",
                       replacement = "Nørre Aaby")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Nørre Åby",
                       replacement = "Nørre Aaby")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Sydlangelands",
                       replacement = "Sydlangeland")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Årup",
                       replacement = "Aarup")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Åbenrå",
                       replacement = "Aabenraa")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Grenå",
                       replacement = "Grenaa")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Ålestrup",
                       replacement = "Aalestrup")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Åbybro",
                       replacement = "Aabybro")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Års",
                       replacement = "Aars")

muni@data$navn <- gsub(x= muni@data$navn,
                       pattern = "Årslev",
                       replacement = "Aarslev")

#Aalborg
# muni@data$navn <- gsub(x= muni@data$navn,
#                        pattern = "Ålborg",
#                        replacement = "Aalborg")

# å --> aa
# muni@data$navn <- gsub(x= muni@data$navn,
#                        pattern = "Å",
#                        replacement = "Aa")
# 
# # Århus
# muni@data$navn <- gsub(x= muni@data$navn,
#                        pattern = "Aarhus",
#                        replacement = "Århus")
# 
# # x --> ks
# muni@data$navn <- gsub(x= muni@data$navn,
#                        pattern = "x",
#                        replacement = "ks")

#fejl i vores eget
new.df$muniname <- gsub(x= new.df$muniname,
                       pattern = "Hirsthals",
                       replacement = "Hirtshals")

new.df$muniname <- gsub(x= new.df$muniname,
                        pattern = "Årslev",
                        replacement = "Aarslev")

# show some data

 plot(muni)
 
 df <- muni@data
 table(df$navn)

## convenience function to clean strings

stringCleaning <- function(x) {
  #convert to lower case, remove unconventional characters, extra spaces
  stringr::str_trim(tolower(gsub("\\s+", " ", gsub("[^[:space:]A-z0-9]", "", x))))
}

# make sure identifier has same name             
names(muni@data)[2] <- "muniname"
muni$muniname <- stringCleaning(muni$muniname) # clean

#subset three interesting years & clean muni names
df1978 <- subset(new.df, year == 1978)
df1993 <- subset(new.df, year == 1993)
df1997 <- subset(new.df, year == 1997)

df2005 <- subset(new.df, year == 2005)
df2001 <- subset(new.df, year == 2001)

df1978$muniname <- stringCleaning(df1978$muniname)
df1993$muniname <- stringCleaning(df1993$muniname)
df1997$muniname <- stringCleaning(df1997$muniname)

df2005$muniname <- stringCleaning(df2005$muniname)
df2001$muniname <- stringCleaning(df2001$muniname)


#rename socialdemocratism variable
names(df1978)[38] <- "SocDem78"
names(df1993)[38] <- "SocDem93"
names(df1997)[38] <- "SocDem97"

names(df2005)[38] <- "SocDem05"
names(df2001)[38] <- "SocDem01"



muni@data <- left_join(muni@data, df1978, by = "muniname")
muni@data <- left_join(muni@data, df1993, by = "muniname")
muni@data <- left_join(muni@data, df1997, by = "muniname")

muni@data <- left_join(muni@data, df2005, by = "muniname")
muni@data <- left_join(muni@data, df2001, by = "muniname")


# exclude amalgamated municipalities as they overlay old ones --> creates missing values!
muni2 <- subset(muni, is.na(SocDem05) == F)

# who isn't matched??
'%!in%' <- function(x,y)!('%in%'(x,y))
test <- df1993[   which( df1993$muniname2 %!in%  muni2$muniname2), ]
test <- dplyr::select(test, muniname, muniname2) #none anymore!



library(tmap)

tm_shape(muni2) +
  tm_fill(c("SocDem93", "SocDem97", "SocDem01"), style = "fixed", 
          breaks=c(-0.1, 0, 0.1, 0.15, 0.2, 0.25, 0.3),
          n = 8, palette = list("Reds", "Reds", "Reds"), 
          legend.hist = TRUE, frame = TRUE) +
  tm_style_grey()

SocMap<- tm_shape(muni2) +
  tm_polygons("SocDem97", style = "fixed",
          breaks=c(-0.1, 0, 0.1, 0.15, 0.2, 0.25, 0.3),
          n = 8, 
          palette = "Reds",
          legend.hist = TRUE, frame = TRUE,
          title = "Fiscal Socialdemocratism anno 1997",
          auto.palette.mapping=FALSE,
          id = "muniname") +
  tm_style_grey()


ggsave(plot = SocMap, filename = "SocialDemocratismMap_1997.eps", device = cairo_ps)

plot(muni)

####

library(systemfit); library(simcf)
pan.dat <- pdata.frame(new.df, index = c("muniname", "year"))

fe.mod.full <- plm(plm::lag(full.SocDem,-4) ~ redvote+ log(estpop) + muniname + year, data = pan.dat, model = "within", effects = "twoways")

pan.dat <- extractdata(fe.mod.full, pan.dat, na.rm = T)
pan.dat <- pdata.frame(pan.dat, index = c("muniname", "year"))

m_sur <- systemfit(plm::lag(full.SocDem,-4) ~ redvote, data = pan.dat, method = "SUR")

muni_fx <- coef(m_sur)
muni_fx <- as.data.frame(muni_fx)
muni_fx$muniname <- rownames(muni_fx)
muni_fx$muniname <- gsub(x=muni_fx$muniname, pattern = " ", replacement = "")
muni_fx <- muni_fx[-c(grep(x=muni_fx$muniname, pattern = "_\\(Intercept\\)")),]
muni_fx$muniname <- gsub(x=muni_fx$muniname, pattern = "_redvote", replacement = "")
muni_fx$muniname <- stringCleaning(muni_fx$muniname)

muni@data <- left_join(muni@data, muni_fx, by = "muniname")
muni2 <- subset(muni, is.na(SocDem05) == F)

muni2$muni_fx_sd <- muni2$muni_fx*(sd(new.df$redvote, na.rm = T) / sd(new.df$full.SocDem, na.rm = T))

surMap <- tm_shape(muni2) +
  tm_polygons("muni_fx_sd", style = "quantile",
              #breaks=c(-0.1, 0, 0.1, 0.15, 0.2, 0.25, 0.3),
              n = 8, 
              palette = "Reds",
              legend.hist = TRUE, frame = TRUE,
              title = "Responsiveness",
              auto.palette.mapping=FALSE,
              id = "muniname") +
  tm_style_grey()


## joyplot

SocJoy <- ggplot(new.df, aes(y = factor(year), x = full.SocDem)) +
  geom_joy(alpha = 0.5) +
  theme_bw() + theme(axis.line = element_line(colour = "black"),
                     plot.title = element_text(hjust = 0.5),
                     axis.text.x = element_text(hjust = 1)) +
  labs(x = "Fiscal Social Democratism", y = NULL)

ggsave(plot = SocJoy, filename = "SocialDemocratismJOY_trend.eps", device = cairo_ps)
