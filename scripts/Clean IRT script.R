library(haven); library(R2jags); library(MCMCpack); library(reshape); library(reshape2) 
library(plyr); library(dplyr); library(ggplot2); library(runjags)

setwd("C:/Users/ex-bce/Dropbox/effects on effects/responsiveness")

df <- read_dta("collect.dta")
df <- df[, -c(37:66)]
df$spend.cap <- df$spend/df$estpop
df <- subset(df, is.na(muni)==F)

new.df <- df[,c(11, 18, 24:34,38)]
new.df <- new.df[,-9]
new.df <- as.data.frame(new.df)

for(i in 1:ncol(new.df)){
  crnt.var <- as.data.frame(new.df[, i])
  df[,ncol(df)+1] <- as.data.frame(scale(crnt.var, center = T, scale = T))
  names(df)[ncol(df)] <- paste(names(new.df)[i], "_scale", sep = "")
}

new.df <- subset(df, year > 1973)
new.df <- new.df[order( new.df$muni, new.df$year ),]
# 
# for(i in c(41:42,49:53)){
#   new.df[,i] <- min(new.df[,i],na.rm = T) - new.df[,i]
# }

for(i in c(39:40,47,49:51)){
  new.df[,i] <- min(new.df[,i],na.rm = T) - new.df[,i]
}

mod <- "model{
  for (i in 1:muni_year){
      for(j in 1:Nitems){
        y[i,j] ~ dnorm(MuY[i,j], Inv_sig_e1[j])

        MuY[i,j]<-beta[j]*Z[i] + alpha[j] 

      }
  }

#prior distribution of latent variable

      Z[34] <- -1.0  ## Brøndby 1974 on the left
      Z[100] <- 1.0   ## Gentofte 1974 on the right
 
       for(i in 1:33){
           Z[i] <- dnorm(0, Inv_sig_f1)
       }
       
       for(i in 35:99){
           Z[i] <- dnorm(0, Inv_sig_f1)
       }
       
       for(i in 101:muni_year){
           Z[i] <- dnorm(0, Inv_sig_f1)
       }


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


y2 <- as.matrix(new.df[,39:51])

muni.data2 <- list(
  
  muni_year = nrow(y2),
  Nitems = ncol(y2),
  y = y2
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

df <- data.frame(df, muni)
