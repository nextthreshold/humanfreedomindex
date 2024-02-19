rm(list=ls())
hfi_cc_2018 <- read.csv("C:/Users/Owner/Desktop/hfi_cc_2018.csv/hfi_cc_2018.csv")
str(hfi_cc_2018)
head(hfi_cc_2018)
summary(hfi_cc_2018)
par(mfrow=c(2,5))
n=ncol(hfi_cc_2018)
for (i in 10:20)
{
  boxplot(hfi_cc_2018[i])
}
library(FactoMineR)
library(factoextra)
library(dplyr)
p1<-princomp(hfi_cc_2018)
cor(hfi_cc_2018)

#dropping non numeric columns 
num_df<-hfi_cc_2018 %>% select(where(is.numeric))
head(num_df)                               
ncol(num_df)
cor(num_df)
nrow(num_df)

#dropping the NA values
na_data<-na.omit(num_df)
head(na_data)
ncol(na_data)
nrow(na_data)
#this is not going to work since there are no rows with all the values available

#Finding the number of na in each row
nul_cols<-0
tot_na_in_col<-0
for (i in 1:ncol(num_df))
  (
  tot_na_in_col[i]<-sum(is.na(num_df[i]))
  )
which(is.na(num_df$pf_rol_criminal))
tot_na_in_col
head(num_df)
head(num_df[50])
num_df<-num_df[-50]
str(num_df)

#using mice imputation on the dataset 
library(mice)
library(dplyr)
library(tidyr)
head(num_df)
str(num_df)
imput_df<-mice(num_df,m=2,method = 'pmm',maxit = 5)
imput_df$imp$pf_rol_procedural
fin_data<-complete(imput_df,1)
head(fin_data)
write.csv(fin_data, "C:\\Users\\Owner\\Desktop\\hfi_cc_2018.csv\\cleaned.csv", row.names=FALSE)


