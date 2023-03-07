rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)
source('Functions.R')
FileName <- 'Alaska Inputs.xlsx'
model_inputs <- read_excel(FileName)

GetComparisonData <- function(EntryAge = 31,
                              YOSRange = 30,
                              DCReturn = 0.07,
                              PayoutRate = 0.0589,
                              PayrollGrowth = 0.0275,
                              LifeExpectancy = 85,
                              EmpType = 'Teachers'){
  
  Age <- EntryAge:(EntryAge + YOSRange - 1)
  Data <- expand_grid(Age) %>%
    mutate(YOS = 1:YOSRange) %>%
    left_join(model_inputs, by = "YOS") %>%
    group_by(EmployeeType) %>%
    mutate(RetAgeOther = ifelse(YOS >= 30, Age, 60),
           RetAgeTeacherPolice = ifelse(YOS >= 20, Age, 60),
           RetAge = ifelse(EmployeeType == 'All Others', RetAgeOther, RetAgeTeacherPolice),
           
           CumMult = (1+PayrollGrowth)^(YOS-1),
           Salary = StartingSalary*CumMult,
           FinalAvgSalary = rollmean(Salary, k = SalPeriod, fill = NA, align = "right"),
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
    ungroup() %>%
    filter(EmployeeType == EmpType)
  
  return(Data)
}
