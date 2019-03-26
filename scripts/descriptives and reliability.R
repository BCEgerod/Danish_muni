
pan.dat$dm_full <- plm::Within(pan.dat$correctsocdem)
pan.dat$dm_red <- plm::Within(pan.dat$socdem_reduced)
pan.dat$dm_blue <- plm::Within(pan.dat$bluevote,na.rm=T)


desc_df <- dplyr::select(pan.dat, correctsocdem, socdem_reduced, bluevote,
                         dm_full, dm_red, dm_blue,
                         log_pop, educ, immig, unemployment)



desc_df <- subset(desc_df, is.na(bluevote)==F)
desc_df$dm_full <- plm::Within(desc_df$correctsocdem)
desc_df$dm_red <- plm::Within(desc_df$socdem_reduced)
desc_df$dm_blue <- plm::Within(desc_df$bluevote,na.rm=T)

stargazer(desc_df,
          summary.stat = c("n", "mean", "sd", "min", "max"))
stargazer(desc_df,
          covariate.labels = c("Full Fiscal Scale", 
                               "Reduced Fiscal Scale",
                               "Support for Right-Wing Parties",
                               "Full Fiscal Scale (Within)", 
                               "Reduced Fiscal Scale (Within)",
                               "Support for Right-Wing Parties (Within)",
                               "Population Size (logged)", "Education", "Immigrants", 
                               "Unemployment"),
          summary.stat = c("n", "mean", "sd", "min", "max"))


################################
# descriptives of each item

desc_df <- as.data.frame(df[,c(13,19,24:30,32:35,39)])

stargazer(desc_df,
          summary.stat = c("n", "mean", "sd", "min", "max"),
          covariate.labels = c("Income Tax", "Property Tax",
                               "Public Employees", "Day Care",
                               "Food Delivery", "Nursing Home",
                               "Relief Stay", "Private Services",
                               "Private Supplier", "Public Housing",
                               "Class Size", "Spending pr Pupil",
                               "Commercial Real Estate Tax",
                               "Spending per Capita"))


##################################
# item-index correlation

#items <- df[,c(12,55,13,19,24:30,32:35,39)]

items <- df[,c(12,55,40:53)]
items <- as.data.frame(items)

cor(items[c(-1)], use = "pairwise.complete.obs")

real_name <- c("Income Tax",
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

index_plot <- list()

for(i in 3:16){
  items$crnt_var <- items[,i]
  
  crnt_mod <- lm(crnt_var ~ correctsocdem, data = items)
  index_plot[[i]] <- ggplot(items, aes(x = correctsocdem, y = crnt_var)) +
                        geom_point(colour = "gray") +
                        theme_classic() +
                        labs(x = NULL, 
                             y = real_name[i-2]) +
                        annotate(geom="text",
                                 x = 2.5, y = max(items$crnt_var, na.rm=T)-1,
                                 label = paste("Est=",round(coef(crnt_mod)[2],digits = 2),"\n",
                                               "SE=",round(sqrt(diag(vcov(crnt_mod)))[2],digits = 2),
                                               sep = "")) +
                        geom_smooth(method = "lm", se = F,
                                    color = "black") +
    scale_x_continuous(limits = c(-2.5, 3.5))

}


library(gridExtra)


all_items<-grid.arrange(index_plot[[3]], index_plot[[4]], index_plot[[5]],
             index_plot[[6]], index_plot[[7]], index_plot[[8]],
             index_plot[[9]], index_plot[[10]], index_plot[[11]],
             index_plot[[12]], index_plot[[13]], index_plot[[14]],
             index_plot[[15]], index_plot[[16]], 
             bottom = "Fiscal Conservatism")

setwd("~/GitHub/Danish_muni/images")
ggsave(all_items, filename = "ItemRest.eps",
       device = cairo_ps, height = 10, width = 10)


alpha_items <- items[,-c(1,2,17)]
ca <- psych::alpha(alpha_items,check.keys=TRUE)

ca_drop <- ca$alpha.drop

ca_drop <- ca_drop[, c(2,6)]
rownames(ca_drop) <-  c("Income Tax",
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

ca_drop <- ca_drop[,c(3,1,2)]

stargazer(ca_drop, summary=F)

na_items <- na.omit(alpha_items)


ggpairs(na_items,
        title = "", axisLabels = "show",
        lower=list(continuous=wrap("smooth", colour="grey")),
        diag=list(continuous=wrap("barDiag", fill="grey")) )



cor_mat <- cor(alpha_items, 
               use = "pairwise.complete.obs")
cor_mat[upper.tri(cor_mat, diag=T)] <- NA

stargazer(cor_mat, summary = F)


