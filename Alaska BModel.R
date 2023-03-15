rm(list = ls())
library("readxl")
library(tidyverse)
library(dplyr)
library(zoo)
#setwd(getwd())
#source("utility_functions.R")

FileName <- 'Alaska BModel Inputs.xlsx'
EntryYear <- 1980:2052
Years <- 1980:2154    #(why 2152? Because 120 - 18 + 2052 = 2154)
YearStart <- 2022
Age <- 18:120
YOS <- 0:70
#RetirementAge <- 20:120
RetYear <- 2005:2154


ModelPeriod <- 100    #Projection period (typically 30 years)
MinAge <- 18          #Age of the typical youngest member
MaxAge <- 120         #Max age from mortality assumptions
YearStart <- 2022     #Year of the latest val report
MinYear <- 1980       #No hard rule about this. Should get back to about 40 years from now.   
MaxYear <- YearStart + ModelPeriod + MaxAge - MinAge

EntryYear <- MinYear:(YearStart + ModelPeriod)
RetYear <- MinYear:(YearStart + ModelPeriod)
Years <- MinYear:MaxYear
Age <- MinAge:MaxAge
YOS <- 0:70
RetirementAge <- Age

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}

#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')#Updated* (to RP-2010 General)
MaleMP <- read_excel(FileName, sheet = 'MP-2017_Male') #Updated* (to MP-2019)
FemaleMP <- read_excel(FileName, sheet = 'MP-2017_Female')#Updated* (to MP-2019)
### Addition ###
SalaryGrowthYOS <- read_excel(FileName, sheet = "Salary Growth YOS")#Added* (to combine YOS & AGE increases)
SalaryEntry <- read_excel(FileName, sheet = "Salary and Headcount") %>% #Updated*
  select(entry_age, start_sal, count_start)#Updated*

TerminationRateAfter8 <- read_excel(FileName, sheet = 'Termination Rates after 8')#Updated*
TerminationRateBefore8 <- read_excel(FileName, sheet = 'Termination Rates before 8')#Updated*
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')#Updated*

##############################################################################################################################

IsRetirementEligible_Regular <- function(Age, YOS, EntryYear){
  Tier1Check = ifelse((Age >= 55 & YOS >= 8) | (YOS >= 20), TRUE, FALSE)
  Tier2Check = ifelse((Age >= 60 & YOS >= 8) | (YOS >= 20), TRUE, FALSE)
  Check = ifelse(EntryYear >= 1990, Tier2Check, Tier1Check)
  
  return(Check)
}

IsRetirementEligible_Early <- function(Age, YOS, EntryYear){
  Tier1Check = ifelse((Age >= 50 & YOS >= 8), TRUE, FALSE)
  Tier2Check = ifelse((Age >= 55 & YOS >= 8), TRUE, FALSE)
  Check = ifelse(EntryYear >= 1990, Tier2Check, Tier1Check)
  
  return(Check)
}

IsRetirementEligible <- function(Age, YOS, EntryYear){
  Check = ifelse(IsRetirementEligible_Regular(Age, YOS, EntryYear) | IsRetirementEligible_Early(Age, YOS, EntryYear), TRUE, FALSE)
  
  return(Check)
}

RetirementType <- function(Age, YOS, EntryYear){
  
  Check = ifelse(IsRetirementEligible_Regular(Age, YOS, EntryYear), 'Regular',
                 ifelse(IsRetirementEligible_Early(Age, YOS, EntryYear), 'Early','None'))
  
  return(Check)
}


SeparationType <- function(Age, YOS, EntryYear){
  Check = ifelse(IsRetirementEligible(Age, YOS, EntryYear) == T, 'Retirement',
                 ifelse(YOS > 5, 'Termination Vested', 'Termination Non-Vested'))
  
  return(Check)
}

##############################################################################################################################

#Custom function to calculate cumulative future values
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}

LinearInterpolation <- function(Data,AgeStart,AgeEnd,Columns,Increment){
  TempMatrix <- matrix(0,nrow= (AgeEnd - AgeStart + 1),Columns)
  TempMatrix[,1] <- AgeStart:AgeEnd
  colnames(TempMatrix) <- colnames(Data)
  TempMatrix <- as.data.frame(TempMatrix)
  
  for(i in 1:nrow(Data)){
    Index <- which(TempMatrix[,1] == as.double(Data[i,1]))
    for(j in 2:Columns){
      TempMatrix[Index,j] <- Data[i,j]
    }
  }
  
  for(i in 1:(nrow(Data)-1)){
    for(k in 2:Columns){
      for(j in 1:(Increment - 1)){
        BaseRowIndex <- (i-1)*Increment + 1
        Addition <- (Data[(i+1),k] - Data[i,k]) / (Data[(i+1),1] - Data[i,1])
        TempMatrix[(BaseRowIndex+j),k] <- Data[i,k] + (Addition*j)
      }
    }
  }
  
  return(TempMatrix)
}

CompoundSalaryIncrease <- function(Data){
  #Data[,1] <- 1
  for(i in 2:nrow(Data)){
    for(j in 2:ncol(Data)){
      #Column 1 is the Age label so we want to avoid computing j-1 at j=2
      if(j > 2){
        Data[i,j] <- Data[i,j]*Data[i-1,j-1]
      }
    }
  }
  
  return(Data)
}

##############################################################################################################################

MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>%        #ultimate rates = rates for the last year in the MP table
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

MaleMP_final <- expand_grid(Age, Years = 1951:MaxYear) %>% 
  left_join(MaleMP, by = c("Age", "Years")) %>% 
  left_join(MaleMP_ultimate, by = "Age") %>% 
  mutate(
    # MP_final_male = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male)   
    #Since the plan assumes "immediate convergence" of MP rates, the "ultimate rates" are used for all years
    MP_final_male = MP_ultimate_male) %>% 
  group_by(Age) %>% 
  mutate(MPcumprod_male_raw = cumprod(1 - MP_final_male),
         MPcumprod_male_adj = MPcumprod_male_raw / MPcumprod_male_raw[Years == 2014]) %>%   #Adjust the mort improvement rates for the 2014 base year
  ungroup()


FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)

FemaleMP_final <- expand_grid(Age, Years = 1951:MaxYear) %>% 
  left_join(FemaleMP, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_ultimate, by = "Age") %>% 
  mutate(
    # MP_final_female = ifelse(Years > max(FemaleMP$Years), MP_ultimate_female, MP_female)
    #Since the plan assumes "immediate convergence" of MP rates, the "ultimate rates" are used for all years
    MP_final_female = MP_ultimate_female
  ) %>%
  group_by(Age) %>% 
  mutate(MPcumprod_female_raw = cumprod(1 - MP_final_female),
         MPcumprod_female_adj = MPcumprod_female_raw / MPcumprod_female_raw[Years == 2014]) %>% 
  ungroup()

MortalityTable_int <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, Age, YOS) %>% 
  mutate(term_year = EntryYear + YOS,
         Years = EntryYear + Age - entry_age) %>% 
  filter(term_year <= Years) %>% 
  arrange(EntryYear, entry_age, YOS, Age)


#Join base mortality table with mortality improvement table and calculate the final mortality rates
MortalityTable <- MortalityTable_int %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  
  #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
  mutate(
    RetirementCond = IsRetirementEligible(Age, YOS, EntryYear),
    mort_male = ifelse(RetirementCond == T, RP_2014_Male_Healthy, 
                       RP_2014_Male_Employee * ScaleMultiple) * MPcumprod_male_adj, 
    mort_female = ifelse(RetirementCond == T, RP_2014_Female_Healthy, 
                         RP_2014_Female_Employee * ScaleMultiple) * MPcumprod_female_adj,
    mort = (mort_male + mort_female)/2)

#filter out the necessary variables
MortalityTable <- MortalityTable %>% 
  select(EntryYear, term_year, Years, entry_age, Age, YOS, mort)


#Create a second mortality table for current retirees
MortalityTable_retire <- expand_grid(Age = Age[Age >= 40], Years = Years[Years >= YearStart]) %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  mutate(base_age = Age - (Years - YearStart),
         mort_male = RP_2014_Male_Healthy * MPcumprod_male_adj,
         mort_female = RP_2014_Female_Healthy * MPcumprod_female_adj,
         mort = (mort_male + mort_female)/2) %>% 
  select(base_age, Age, Years, mort) %>% 
  filter(base_age >= 40) %>% 
  arrange(base_age)

##############################################################################################################################

#Separation Rates
SeparationRates <- expand_grid(EntryYear, Age, YOS) 
SeparationRates <- SeparationRates %>%
  mutate(entry_age = Age - YOS,
         Years = EntryYear + YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(EntryYear, entry_age, Age) %>% 
  left_join(TerminationRateAfter8, by = "Age") %>%
  left_join(TerminationRateBefore8, by = "YOS") %>%
  left_join(RetirementRates, by = "Age") %>%
  ### Additions ###
  #mutate_all(as.numeric) %>% 
  replace(is.na(.), 0)

#If you're retirement eligible, use the retirement rates, then checks YOS < 5 and use the regular termination rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_type = IsRetirementEligible_Regular(Age,YOS, EntryYear),
         SepRate_DB = ifelse(retirement_type == "Regular", Unreduced_Ret,
                             ifelse(retirement_type == "Early", Reduced_Ret,
                                    ifelse(YOS < 8, TermBefore8, TermAfter8)))) %>% 
  group_by(EntryYear, entry_age) %>% 
  mutate(RemainingProb_DB = cumprod(1 - lag(SepRate_DB, default = 0)),
         SepProb_DB = lag(RemainingProb_DB, default = 1) - RemainingProb_DB) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(EntryYear, entry_age, Years, Age, YOS, RemainingProb_DB, SepProb_DB, SepRate_DB)

##############################################################################################################################

#Create a long-form table of Age and YOS and merge with salary data
SalaryData <- expand_grid(EntryYear, Age, YOS) %>%  
  mutate(entry_age = Age - YOS,
         Years = EntryYear + YOS) %>%
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(EntryYear, entry_age, YOS)
SalaryData <- left_join(SalaryData, SalaryEntry, by = c("entry_age"))
SalaryData <- left_join(SalaryData, SalaryGrowthYOS, by = c("YOS"))

SalaryData <- SalaryData %>%
  group_by(EntryYear, entry_age) %>%
  mutate(Salary = start_sal*cumprod(1+lag(salary_increase_yos,default = 0))*(1 + payroll_growth)^(Years - YOS - YearStart),
         #Salary = start_sal*salary_increase_compound*(1 + payroll_growth)^(Years - YOS - YearStart),
         FinalAvgSalary_3YR = rollmean(lag(Salary), k = 3, fill = NA, align = "right"),
         FinalAvgSalary_5YR = rollmean(lag(Salary), k = 5, fill = NA, align = "right"),
         #DB_EEContrib = (DB_EE_cont + DC_DB_EE_cont)*Salary,
         DB_EEContrib = DB_EE_cont*Salary,
         DBEEBalance = cumFV(Interest, DB_EEContrib),
         DC_EEBalance = DC_EE_cont*Salary,
         
         #DC_ERBalance = 0.2*DC_ER_cont*Salary,
         #CBEEContAmount = CB_EE_paycredit * Salary,
         #CBERContAmount = CB_ER_paycredit * Salary,
         #CBEEBalance = cumFV(ICR, CBEEContAmount),
         #CBERBalance = cumFV(ICR, CBERContAmount),
         #CBBalance = CBEEBalance + ifelse(YOS >= CB_vesting, CBERBalance, 0),
         CumulativeWage = cumFV(ARR, Salary)) %>% 
  ungroup()


#Survival Probability and Annuity Factor for active members
AnnFactorData <- MortalityTable %>% 
  group_by(EntryYear, entry_age, YOS) %>% 
  mutate(surv = cumprod(1 - lag(mort, default = 0)),
         surv_DR = surv/(1 + ARR)^(Age - min(Age)),
         #surv_ICR = surv/(1 + ICR)^(Age - min(Age)),
         #surv_ACR = surv/(1 + ACR)^(Age - min(Age)),
         surv_DR_COLA = surv_DR * (1 + COLA)^(Age - min(Age)),
         #surv_ACR_COLA = surv_ACR * (1 + COLA)^(Age - min(Age)),
         #AnnuityFactor_ACR = rev(cumsum(rev(surv_ACR_COLA)))/surv_ACR_COLA,
         AnnuityFactor_DR = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
  ungroup()

#Survival Probability and Annuity Factor for retirees
AnnFactorData_retire <- MortalityTable_retire %>% 
  group_by(base_age) %>% 
  mutate(surv = cumprod(1 - lag(mort, default = 0)),
         surv_DR = surv/(1 + ARR)^(Age - min(Age)),
         surv_DR_COLA = surv_DR * (1 + COLA)^(Age - min(Age)),
         AnnuityFactor_DR = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA)

##############################################################################################################################

ReducedFactor <- expand_grid(Age, YOS, EntryYear) %>% 
  left_join(AnnFactorData, by = c("EntryYear" ,"Age", "YOS")) %>%
  replace(is.na(.), 0) %>% 
  filter(Age >= YOS) %>%
  group_by(EntryYear, Age, YOS) %>% 
  mutate(RetType = RetirementType(Age, YOS, EntryYear),
         RF = ifelse(RetType == "Early", AnnuityFactor_DR[Age == 60] * surv_DR[Age == 60] / surv_DR / AnnuityFactor_DR,   #Benefit for early retirement  = actuarially reduced amount of the normal benefit the employee would receive at the earliest age of normal retirement. 
                     ifelse(RetType == "None", 0, 1))) %>% 
  rename(RetirementAge = Age) %>% 
  ungroup()

BenefitsTable <- AnnFactorData %>%
  rename(RetirementAge = Age) %>%
  mutate(term_age = entry_age + YOS) %>%
  left_join(SalaryData, by = c("term_age" = "Age", "YOS", "term_year" = "Years", "entry_age", "EntryYear")) %>% 
  left_join(ReducedFactor %>% select(RetirementAge, YOS, RF, EntryYear), by = c("RetirementAge", "YOS", "EntryYear")) %>%
  rename(surv_DR_ret = surv_DR, AF_Ret = AnnuityFactor_DR) %>% 
  # YOS is in the benefit section because of graded multipliers
  mutate(GradedMult = ifelse(EntryYear >= 1990, BenMult1*pmin(YOS,10) + BenMult2*pmax(YOS-10,0), BenMult1*YOS),
         BaseBenefit1 = GradedMult*FinalAvgSalary_3YR,
         BaseBenefit2 = 25*12*YOS,
         #RF = ifelse(RetYear < 2015, RF_Before2015, RF_After2015/YOS),
         Benefit = pmax(BaseBenefit1,BaseBenefit2),
         AnnFactorAdj_DB = AF_Ret*surv_DR_ret / surv,
         
         #AnnFactorAdj_DB = AnnuityFactor_DR * surv_DR,
         DB_Benefit = RF * Benefit,
         PV_DB_Benefit = DB_Benefit*AnnFactorAdj_DB)
         #CBBalance_final = CBBalance / surv_ICR,                                                       #project the cash balance forward to the annuitization day 
         #CB_Benefit = CBBalance_final / AnnuityFactor_ACR)
# replace(is.na(.), 0)

OptimumBenefit_DB <- BenefitsTable %>% 
  group_by(EntryYear, entry_age, term_age) %>% 
  summarise(Max_PV_DB = max(PV_DB_Benefit)) %>%
  mutate(Max_PV_DB = ifelse(is.na(Max_PV_DB), 0, Max_PV_DB)) %>%
  # filter(entry_age %in% SalaryEntry$entry_age, term_age >= 20) %>%
  ungroup() %>% 
  #join the BenefitsTable to get the "optimal" retirement age
  left_join(BenefitsTable %>% filter(PV_DB_Benefit > 0), by = c("EntryYear", "entry_age", "term_age", "Max_PV_DB" = "PV_DB_Benefit")) %>%
  select(EntryYear, entry_age, term_age, RetirementAge, Max_PV_DB) %>% 
  mutate(RetirementAge = ifelse(is.na(RetirementAge), term_age, RetirementAge))    #Assume retire age = term age for non-vested members

##############################################################################################################################

source("utility_functions.R")
FinalData <- SalaryData %>% 
  left_join(OptimumBenefit_DB, by = c("EntryYear", "entry_age", "Age" = "term_age")) %>% 
  #left_join(OptimumBenefit_CB, by = c("EntryYear", "entry_age", "Age" = "term_age", "RetirementAge")) %>% 
  left_join(SeparationRates, by = c("EntryYear", "Age", "YOS", "entry_age", "Years")) %>%
  group_by(EntryYear, entry_age) %>%
  mutate(#SepType = SeparationType(Age,YOS, Years),
         #LumpSumPct = ifelse(EntryYear >= 2022, 0.2, 0.05),
         #DBWealth = ifelse(SepType == 'Retirement', pmax(DBEEBalance,Max_PV_DB), 
         #                  ifelse(SepType == 'Termination Vested', LumpSumPct*DBEEBalance + (1-LumpSumPct)*Max_PV_DB, DBEEBalance)),
         
         DBWealth = pmax(DBEEBalance,Max_PV_DB),
         #DBWealth = pmax(DBEEBalance,Max_PV_DB),
         #PenWealth = pmax(DBEEBalance,MaxBenefit),  #50% lump sum, 50% optimal retirement
         #PenWealth = 0.5*(DBEEBalance + MaxBenefit),
         RealPenWealth = DBWealth/(1 + assum_infl)^YOS,
         #PVPenWealth = DBWealth/(1 + ARR)^YOS * SepProb,
         #PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb,
         
         #CBWealth = ifelse(DBWealth == DBEEBalance, CBBalance, PV_CB_Benefit),   #mimic DB members' behavior. This is to simplify the workforce projection done later.
         Real_DBWealth = DBWealth/(1 + assum_infl)^YOS,
         #Real_CBWealth = CBWealth/(1 + assum_infl)^YOS,
         PVFB_DB = PVFB(sep_rate_vec = SepRate_DB, interest = ARR, value_vec = DBWealth),
         #PVFB_CB = PVFB(sep_rate_vec = SepRate_DB, interest = ARR, value_vec = CBWealth),
         PVFS = PVFS(remaining_prob_vec = RemainingProb_DB, interest = ARR, sal_vec = Salary),
         normal_cost_DB = PVFB_DB[YOS == 0] / PVFS[YOS == 0],
         #normal_cost_CB = PVFB_CB[YOS == 0] / PVFS[YOS == 0],
         PVFNC_DB = PVFS * normal_cost_DB) %>%
  # replace(is.na(.), 0) %>%
  ungroup()


NormalCost <- FinalData %>% 
  filter(YOS == 0) %>% 
  select(EntryYear, entry_age, normal_cost_DB)

#Calculate the aggregate normal cost for current year (for testing purposes)
NC_aggregate <- NormalCost %>% 
  left_join(SalaryEntry, by = c("entry_age")) %>%
  left_join(SalaryData %>% select(EntryYear, entry_age, Age, Salary), by = c("EntryYear", "entry_age")) %>% 
  filter(!is.na(count_start)) %>% 
  summarise(normal_cost_aggregate_DB = sum(normal_cost_DB * Salary * count_start) / sum(Salary * count_start))