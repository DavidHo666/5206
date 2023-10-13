#####################################################    Set Up     ###############################################################
library(tidyverse)
library(reshape2)
library(corrplot)
library(ggplot2)
theme_set(theme_light())
mutual_funds_data = read.csv("~/Downloads/Mutual Funds.csv",na.strings=c("",NA))
etfs_data = read.csv("~/Downloads/ETFs.csv",na.strings=c("",NA))
#mutual_funds_data = read.csv("/Users/david/Downloads/archive-2/Mutual Funds.csv",na.strings=c("",NA))
#etfs_data = read.csv("/Users/david/Downloads/archive-2/ETFs.csv",na.strings=c("",NA))

#################################################### Data Wrangling ###############################################################

## Select the intersection columns of two data sets and drop the columns that are not useful for our project 
#  and add a column called fund_type
mutual_funds = mutual_funds_data %>%
  select(intersect(names(mutual_funds_data), names(etfs_data))) %>% 
  select(-fund_symbol, -fund_extended_name, -fund_family, -inception_date, 
         -category, -investment_strategy, -currency, -top10_holdings) %>%
  mutate(fund_type = "mutual funds")

etfs = etfs_data %>%
  select(intersect(names(mutual_funds_data), names(etfs_data)))%>% 
  select(-fund_symbol, -fund_extended_name, -fund_family, -inception_date, 
         -category, -investment_strategy, -currency, -top10_holdings) %>%
  mutate(fund_type = "etfs")

## Rbind datasets
both_funds = rbind(mutual_funds, etfs)



#################################################### Data Visualization ###############################################################
## Barplot of the number of each investment type with different size type of Mutual Fund
ggplot(mutual_funds %>% filter(!is.na(investment_type)), 
       aes(x = investment_type, 
           fill = size_type)) + geom_bar()+
  labs(fill = "Size Type" ,
       title="The number of each investment type with different size type of Mutual Fund",
       y="Total Count", 
       x= "Investment Type")+
  theme(
    legend.title = element_text(face = "bold")
  )

## Barplot of the number of each investment type with different size type of ETFs
ggplot(etfs %>% filter(!is.na(investment_type)), 
       aes(x = investment_type, 
           fill = size_type)) + geom_bar()+
  labs(fill = "Size Type" ,
       title="The number of each investment type with different size type of ETFs",
       y="Total Count", 
       x= "Investment Type")+
  theme(
    legend.title = element_text(face = "bold")
  )

## The density plot of each variables with corresponding investment type of Mutual Funds
melt_mutual = melt(mutual_funds %>% filter(!is.na(investment_type)))
ggplot(melt_mutual, aes(
  x = value, color = investment_type, fill = investment_type)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free") +
  scale_fill_brewer(palette="Pastel1")+
  labs(title="Density distribution of each variables with corresponding investment type of Mutual Funds regarding the values",
       y="Density", 
       x= "Value")+ scale_fill_discrete(name='Investment Type')+
  scale_color_discrete(name='Investment Type')+
  theme(
    legend.title = element_text(face = "bold")
  )


## The density plot of each variables with corresponding investment type of ETFs
melt_etfs = melt(etfs %>% filter(!is.na(investment_type)))
ggplot(melt_etfs, aes(
  x = value, color = investment_type, fill = investment_type)) + 
  stat_density() +
  facet_wrap(~variable, scales = "free") +
  scale_fill_brewer(palette="Pastel1")+
  labs(title="Density distribution of each variables with corresponding investment type of ETFs regarding the values",
       y="Density", 
       x= "Value")+ scale_fill_discrete(name='Investment Type')+
  scale_color_discrete(name='Investment Type')+
  theme(
    legend.title = element_text(face = "bold")
  )





## Sector shows how much funds invest in specific areas.

#### This version provides a clearer comparision of distriibution betweeen ETFs and Mutual_funds of each sector
mutual_funds_sector = mutual_funds %>% select(starts_with("sector"))
mutual_funds_sector = na.omit(mutual_funds_sector)/100
mutual_funds_sector = mutual_funds_sector %>% mutate(type= "mutual_funds")

etfs_sector = etfs %>% select(starts_with("sector"))
etfs_sector = na.omit(etfs_sector)/100
etfs_sector = etfs_sector %>% mutate(type= "etfs")

both_funds_sector = rbind(mutual_funds_sector, etfs_sector)
both_funds_sector_new= melt(both_funds_sector,id.vars="type")

# Boxplot of comparision of distriibution betweeen ETFs and Mutual_funds of each sector
compare <- ggplot(data = both_funds_sector_new, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=type))+
  labs(title="Comparision of distribution betweeen ETFs and Mutual_funds of each sector",
       y="Value", x="Type of sector")+
  theme(legend.title = element_text(face = "bold"))+
  scale_fill_discrete(name='Investment Type',labels = c("ETFs", "Mutual Funds"))

comparison_sector = compare + facet_wrap( ~ variable, scales="free")



#### This version provides a clearer comparison of the distribution of EACH SECTOR
comparison_eachfunds = ggplot(data = both_funds_sector_new, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=type))+coord_flip()+
  labs(title="Comparison of the distribution of EACH SECTOR",
       y="Value", x="Type of sector")+
  theme(legend.title = element_text(face = "bold"))+
  scale_fill_discrete(name='Investment Type',labels = c("ETFs", "Mutual Funds"))


#### This version provides a comparision of overall distribution of each sector
comparison_overall = ggplot(data = both_funds_sector_new, aes(x=variable, y=value)) + 
  geom_boxplot()+coord_flip()+labs(title="Overall distribution of each sector",
                                   y="Value", x="Type of sector")


#################################################################################################################################
###########################################    AIC Method Modeling   ############################################################


###########################################   Model For Mutual Funds (AIC) ############################################################

## Only select the numerical columns in Mutual Funds Data and add the investment_type and size_type columns. 
mutual_reg_data = mutual_funds_data %>%
  select(where(is.numeric)) %>% 
  mutate(investment_type = mutual_funds_data$investment_type, 
         size_type = mutual_funds_data$size_type)

## Lists the number of NAs of each columns using for loop
y = c()
for (i in seq_along(names(mutual_reg_data))) {
  x = sum(is.na(mutual_reg_data[,i]))
  y = c(y, x)
}
names(y) = names(mutual_reg_data)
head(sort(y, decreasing = TRUE, 20), 50)


## Drop the columns with more NAs and create a new mutual_reg_data_new.  
mutual_reg_data_new = mutual_funds_data %>%
  select(where(is.numeric)) %>% 
  mutate(investment_type = mutual_funds_data$investment_type, 
         size_type = mutual_funds_data$size_type) %>%
  select(-starts_with("credit"),-starts_with("category"), -ends_with("q1"), -ends_with("q2"), 
         -ends_with("q3"), -ends_with("q4"), -ends_with("ytd")) %>%
  na.omit()


## Select the optimized parameter and automaticaly optimized by ols_step_both_aic() function. 
library(olsrr)
full_model = lm(fund_return_2019~.,data = mutual_reg_data_new)
step_aic_both = ols_step_both_aic(full_model, details = TRUE)
step_aic_both$predictors

## Pick the parameter from the results and create the model
mutual_model = lm(fund_return_2019 ~ fund_mean_annual_return_10years + fund_mean_annual_return_3years + 
                    fund_return_2018 + fund_return_1year + fund_return_3years + fund_standard_deviation_3years +
                    fund_return_2017 + fund_return_2016 + fund_return_2014 + investment_type + 
                    fund_return_3months + fund_return_2015 + fund_return_2011 + fund_return_2013 + 
                    fund_return_2012 + fund_return_2010 + fund_mean_annual_return_5years + 
                    fund_alpha_10years + fund_standard_deviation_10years + size_type + 
                    fund_beta_10years + fund_sharpe_ratio_3years + fund_sharpe_ratio_5years +
                    fund_treynor_ratio_5years + sector_financial_services + sector_utilities + 
                    fund_r_squared_10years + fund_r_squared_5years + sector_consumer_cyclical + 
                    fund_standard_deviation_5years + sector_real_estate + sector_basic_materials +
                    sector_technology + median_market_cap + fund_net_annual_expense_ratio + 
                    price_cashflow_ratio + price_book_ratio + quarters_down + sector_industrials + 
                    net_asset_value + fund_sharpe_ratio_10years + fund_yield + asset_bonds + 
                    bond_duration + bond_maturity + asset_others + asset_convertable + 
                    price_earnings_ratio + quarters_up + sector_consumer_defensive + 
                    price_sales_ratio + asset_preferred + sector_communication_services, 
                  data = mutual_reg_data_new)

library(lm.beta)
lm.beta(mutual_model)
standardized_mutual_beta = lm.beta(mutual_model)

## Top 30 highest parameter coefficients
standardized_mutual_beta$standardized.coefficients %>% 
  unlist() %>% sort(decreasing = TRUE) %>% head(20)

## Top 30 lowest parameter coefficients
standardized_mutual_beta$standardized.coefficients %>% 
  unlist() %>% sort(decreasing = FALSE) %>% head(20)

## Compute analysis of variance tables for the fitted model. 
summary(mutual_model)  #R-squared:  0.9568,	Adjusted R-squared:  0.9564 
anova(mutual_model)


## Correlations of each predictors
cor_mutual = cor(mutual_reg_data_new %>% select(step_aic_both$predictors) %>% select(-investment_type, -size_type)) %>% as.matrix()
library(ggcorrplot)
ggcorrplot(cor_mutual)


## Add predictions of the model and the abline.  
library(modelr)
mutual_reg_data_new_prediction = mutual_reg_data_new %>%
  add_predictions(mutual_model) %>%
  mutate(resid = fund_return_2019 - pred)
ggplot(mutual_reg_data_new_prediction, aes(fund_return_2019, pred)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = "red")

## Plot the residuals of the model
ggplot(mutual_reg_data_new_prediction, aes(fund_return_2019, resid)) +
  geom_point()

plot(mutual_model$residuals)

## Nomal QQ Plot of residuals
qqnorm(mutual_model$residuals)
qqline(mutual_model$residuals)



###############################################   Model For ETFs (AIC) ###############################################################

## Only select the numerical columns in ETFs and add the investment_type and size_type columns. 
etfs_reg_data = etfs_data %>%
  select(where(is.numeric)) %>% mutate(investment_type = etfs_data$investment_type, 
                                       size_type = etfs_data$size_type)

## Lists the number of NAs of each columns using for loop
y = c()
for (i in seq_along(names(etfs_reg_data))) {
  x = sum(is.na(etfs_reg_data[,i]))
  y = c(y, x)
}
names(y) = names(etfs_reg_data)
head(sort(y, decreasing = TRUE, 20), 50)

## Drop the columns with more NAs and create a new etfs_reg_data_new  
etfs_reg_data_new = etfs_data %>%
  select(where(is.numeric)) %>% 
  mutate(investment_type = etfs_data$investment_type, 
         size_type = etfs_data$size_type) %>%
  select(-starts_with("credit"),-starts_with("category"), -ends_with("2010"), 
         -ends_with("ytd")) %>%
  na.omit()

## Select the optimized parameter and automaticaly optimized by ols_step_both_aic() function. 
full_model2 = lm(fund_return_2019~.,data = etfs_reg_data_new)
step_aic_both2 = ols_step_both_aic(full_model2, details = TRUE)
step_aic_both2$predictors

## Pick the parameter from the results and create the model
etfs_model = lm(fund_return_2019 ~ fund_return_1year + fund_return_2018 + fund_return_5years + 
                  fund_treynor_ratio_3years + fund_return_2017 + fund_return_2016 + 
                  fund_return_2015 + fund_yield + fund_sharpe_ratio_5years + fund_return_2013 + 
                  fund_alpha_5years + fund_alpha_10years + fund_sharpe_ratio_10years + 
                  sector_energy + sector_technology + fund_return_2011 + sector_consumer_defensive + 
                  fund_treynor_ratio_5years + fund_treynor_ratio_10years + sector_utilities + 
                  fund_standard_deviation_3years + fund_return_10years + price_earnings_ratio + 
                  fund_beta_3years + fund_mean_annual_return_5years + fund_return_1month + 
                  fund_beta_5years + fund_return_3years + fund_standard_deviation_5years + 
                  fund_alpha_3years, data = etfs_reg_data_new)

standardized_etfs_beta = lm.beta(etfs_model)


## Top 30 highest parameter coefficients
standardized_etfs_beta$standardized.coefficients %>% sort(decreasing = TRUE) %>% head(20)

## Top 30 lowest parameter coefficients
standardized_etfs_beta$standardized.coefficients %>% sort(decreasing = FALSE) %>% head(20)

## Compute analysis of variance tables for the fitted model. 
summary(etfs_model) #Multiple R-squared:  0.9111,	Adjusted R-squared:  0.9043 
anova(etfs_model)




## Correlations of each predictors
cor_etfs = cor(etfs_reg_data_new %>% select(step_aic_both2$predictors)) %>% as.matrix()
ggcorrplot(cor_etfs)

## Add predictions of the model and the abline.  
etfs_reg_data_new_prediction = etfs_reg_data_new %>%
  add_predictions(etfs_model) %>%
  mutate(resid = fund_return_2019 - pred)

ggplot(etfs_reg_data_new_prediction, aes(fund_return_2019, pred)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = "red")

## Plot the residuals of the model
ggplot(etfs_reg_data_new_prediction, aes(fund_return_2019, resid))+
  geom_point()

## Nomal QQ Plot of residuals
qqnorm(etfs_model$residuals)
qqline(etfs_model$residuals)


#################################################################################################################################
###########################################    VIF Method Modeling   ############################################################

## VIF function
vif_func<-function(in_frame,thresh=10,trace=T,...){
  library(fmsb)
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    in_dat<-in_frame
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      vif_vals<-NULL
      var_names <- names(in_dat)
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      vif_max<-as.numeric(vif_vals[max_row,2])
      if(vif_max<thresh) break
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
    }
    return(names(in_dat))
  }
}


###########################################   Model For ETFs (VIF) ##############################################################

etfs_reg_data_new_vif_without_y = etfs_data %>%
  select(where(is.numeric))  %>%
  select(-starts_with("credit"),-starts_with("category"), -ends_with("2010"), 
         -ends_with("ytd")) %>% na.omit() %>% select(-fund_return_2019)

etfs_reg_data_new_vif_with_y = etfs_data %>%
  select(where(is.numeric))  %>%
  select(-starts_with("credit"),-starts_with("category"), -ends_with("2010"), 
         -ends_with("ytd")) %>% na.omit()

keep_dat = vif_func(in_frame = etfs_reg_data_new_vif_without_y, thresh = 10, trace = T)
etfs_vif_model = lm(paste('fund_return_2019 ~',paste(keep_dat,collapse='+')), 
                    data = etfs_reg_data_new_vif_with_y)

standardized_etfs_vif = lm.beta(etfs_vif_model)
standardized_etfs_vif$standardized.coefficients%>% sort(decreasing = TRUE) %>% head(20)
standardized_etfs_vif$standardized.coefficients%>% sort(decreasing = FALSE) %>% head(20)

summary(etfs_vif_model) # Multiple R-squared:  0.7192,	Adjusted R-squared:  0.6946
anova(etfs_vif_model)

cor_etfs_vif = cor(etfs_reg_data_new_vif_without_y %>% select(keep_dat)) %>% as.matrix()
ggcorrplot(cor_etfs_vif)

etfs_reg_data_new_vif_with_y_prediction = etfs_reg_data_new_vif_with_y %>%
  add_predictions(etfs_vif_model)%>%
  mutate(resid = fund_return_2019 - pred)

ggplot(etfs_reg_data_new_vif_with_y_prediction, aes(fund_return_2019, pred)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = "red")

ggplot(etfs_reg_data_new_vif_with_y_prediction, aes(fund_return_2019, resid))+
  geom_point()+
  labs(title="Residual of VIF model for ETFs",
       y="Residual", 
       x= "Fund return 2019")
qqnorm(etfs_vif_model$residuals)
qqline(etfs_vif_model$residuals)


###########################################   Model For Mutual Funds (VIF) ##############################################################
mutual_reg_data_new_vif_with_y = mutual_funds_data %>%
  select(where(is.numeric))  %>%
  select(-starts_with("credit"),-starts_with("category"), -ends_with("q1"), -ends_with("q2"), 
         -ends_with("q3"), -ends_with("q4"), -ends_with("ytd")) %>%
  na.omit()

mutual_reg_data_new_vif_without_y = mutual_funds_data %>%
  select(where(is.numeric)) %>%
  select(-starts_with("credit"),-starts_with("category"), -ends_with("q1"), -ends_with("q2"), 
         -ends_with("q3"), -ends_with("q4"), -ends_with("ytd")) %>%
  na.omit() %>% select(-fund_return_2019)

keep_dat_mutual = vif_func(in_frame = mutual_reg_data_new_vif_without_y, thresh = 10, trace = T)
mutual_vif_model = lm(paste('fund_return_2019 ~',paste(keep_dat_mutual,collapse='+')), 
                      data = mutual_reg_data_new_vif_with_y)
standardized_mutual_vif = lm.beta(mutual_vif_model)
standardized_mutual_vif$standardized.coefficients%>% sort(decreasing = TRUE) %>% head(20)
standardized_mutual_vif$standardized.coefficients%>% sort(decreasing = FALSE) %>% head(20)

summary(mutual_vif_model) # Multiple R-squared:  0.8119,	Adjusted R-squared:  0.8106 
anova(mutual_vif_model)

cor_mutual_vif = cor(mutual_reg_data_new_vif_without_y %>% select(keep_dat)) %>% as.matrix()
ggcorrplot(cor_mutual_vif)

mutual_reg_data_new_vif_with_y_prediction = mutual_reg_data_new_vif_with_y %>%
  add_predictions(mutual_vif_model)%>%
  mutate(resid = fund_return_2019 - pred)

ggplot(mutual_reg_data_new_vif_with_y_prediction, aes(fund_return_2019, pred)) +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = "red")

ggplot(mutual_reg_data_new_vif_with_y_prediction, aes(fund_return_2019, resid))+
  geom_point()+
  labs(title="Residual of VIF model for Mutual Funds",
       y="Residual", 
       x= "Fund return 2019")
qqnorm(etfs_vif_model$residuals)
qqline(etfs_vif_model$residuals)














