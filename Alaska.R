rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
source('Functions.R')
FileName <- 'Alaska Inputs.xlsx'
model_inputs <- read_excel(FileName)

GetComparisonData <- function(Age = 31:60,
                              DCReturn = 0.07,
                              PayoutRate = 0.0589,
                              PayrollGrowth = 0.0275,
                              RetAge = 60,
                              LifeExpectancy = 85){
  
  Data <- expand_grid(Age) %>%
    mutate(YOS = 1:30) %>%
    left_join(model_inputs, by = "YOS") %>%
    group_by(EmployeeType) %>%
    mutate(CumMult = (1+PayrollGrowth)^(YOS-1),
           Salary = StartingSalary*CumMult,
           FinalAvgSalary = rollmean(Salary, k = 5, fill = NA, align = "right"),
           DBAnnuity = ifelse(YOS >= Vesting, Multiplier*YOS*FinalAvgSalary, 0),
           DBIncReplace = DBAnnuity/Salary,
           
           DCCont = DCRate*Salary,
           DCBalance = cumFV(DCReturn, DCCont),
           DCAnnuity = ifelse(DBAnnuity > 0, 
                              PMT(r = PayoutRate, nper = LifeExpectancy - RetAge, pv = DCBalance, t = 1) / 1.016725, 0),
           DCIncReplace = DCAnnuity / Salary,
           
           DCBalanceRev = DCBalance*(1+DCReturn)^(RetAge - Age),
           DCAnnuityRev = ifelse(DBAnnuity > 0, 
                                 PMT(r = PayoutRate, nper = LifeExpectancy - RetAge, pv = DCBalanceRev, t = 1) / 1.016725, 0)) %>%
    arrange(EmployeeType) %>%
    ungroup()
}
