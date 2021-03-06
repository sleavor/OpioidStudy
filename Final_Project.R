df = read.delim('Data/Opioid Related Death Summary, 1999-2020.txt')

df = df %>% mutate(death_per_100k = Deaths/Population*100000) %>% 
  subset(Notes!="Total") %>% subset(State!="District of Columbia") %>% 
  select(State, State.Code, Year, Deaths, Population,
                                    death_per_100k)

#Treatment Year is if they added one-day reporting
df = df %>% mutate(treatment_year = ifelse(State=="West Virginia", 2012,
                                           ifelse(State=="Michigan" | State =="Tennessee" | State=="Washington", 2016,
                                                  ifelse(State=="Kentucky", 2013, 0))))

#Treatment Year for online provider access
df = df %>% mutate(treatment_year = ifelse(State=="Kentucky", 2005,
                                           ifelse(State=="Massachusetts", 2011,
                                                  ifelse(State=="Michigan", 2003,
                                                         ifelse(State=="New York", 2010, 
                                                                ifelse(State=="North Carolina" | State=="Tennessee", 2007,
                                                                       ifelse(State=="Washington", 2012, 
                                                                              ifelse(State=="West Virginia", 2004, 0))))))))


#Treatment Year is if they legalize marijuana recreationally
df = df %>% mutate(treatment_year = ifelse(State=="Alaska" | State=="Oregon", 2014,
                                           ifelse(State=="Arizona" | State=="Montana" | State=="New Jersey", 2020,
                                                  ifelse(State=="California" | State=="Maine" | State=="Massachusetts" | State=="Nevada", 2016,
                                                         ifelse(State=="Colorado" | State=="Washington", 2012, 
                                                                ifelse(State=="Illinois", 2019,
                                                                       ifelse(State=="Michigan" | State=="Vermont", 2018, 0)))))))


#Table of Treatment Years
df %>% group_by(State) %>% summarize(Treatment_Year = mean(treatment_year)) %>% 
  subset(Treatment_Year != 0) %>% round(2) %>%  kbl(format="latex")

#Summarizing Deaths Per State
df %>% group_by(State) %>% summarize(Mean_Death_Rate = round(mean(death_per_100k),2)) %>% kbl(format="latex")
#Summarizing Deaths Per Year
df %>% group_by(Year) %>% summarize(Mean_Death_Rate = round(mean(death_per_100k),2)) %>% kbl(format="latex")

atts <- att_gt(yname = "death_per_100k", # LHS variable
               tname = "Year", # time variable
               idname = "State.Code", # id variable
               gname = "treatment_year", # first treatment period variable
               data = df, # data
               #xformla = NULL, # no covariates
               xformla = ~ Population, # with covariates
               est_method = "dr", # "dr" is doubly robust. "ipw" is inverse probability weighting. "reg" is regression
               control_group = "nevertreated", # set the comparison group which is either "nevertreated" or "notyettreated" 
               bstrap = TRUE, # if TRUE compute bootstrapped SE
               biters = 1000, # number of bootstrap iterations
               print_details = FALSE, # if TRUE, print detailed results
               clustervars = "State.Code", # cluster level
               panel = TRUE) # whether the data is panel or repeated cross-sectional

# Aggregate ATT
agg_effects <- aggte(atts, type = "group")
summary(agg_effects)

#Make agg eff table in Latex
agg_df = data.frame('Overall', agg_effects$overall.att, agg_effects$overall.se)
agg_df = setNames(agg_df ,c("Year","ATT", "SE"))
agg_df2 = data.frame(agg_effects$egt, agg_effects$att.egt, agg_effects$se.egt)
agg_df2 = setNames(agg_df2 ,c("Year","ATT", "SE"))
agg_df = rbind(agg_df, agg_df2)
agg_df$ATT = agg_df$ATT %>% round(2)
agg_df$SE = agg_df$SE %>% round(2)
agg_df %>% kbl(format="latex")

# Group-time ATTs
summary(atts)

# Plot group-time ATTs
ggdid(atts)

# Event-study
agg_effects_es <- aggte(atts, type = "dynamic")
summary(agg_effects_es)
new_df = data.frame(agg_effects_es$egt, 
                    agg_effects_es$att.egt, 
                    agg_effects_es$se.egt) %>% 
  mutate(lower_int = agg_effects_es.att.egt-1.98*agg_effects_es.se.egt,
         upper_int = agg_effects_es.att.egt+1.98*agg_effects_es.se.egt) %>% round(2)
kbl(new_df, format="latex")

# Plot event-study coefficients
ggdid(agg_effects_es)

# OLS Regression for Robustness
#Dummy for treated or not
df = df %>% mutate(treated = ifelse(treatment_year == 0, 0, ifelse(Year >= treatment_year, 1, 0)))
reg = lm(death_per_100k ~ treated + Year + Population, data=df)
summary(reg)   

#Using years from treatment instead of treated dummy
df = df %>% mutate(years_from_treat = ifelse(treatment_year == 0, 0, ifelse(Year >= treatment_year, Year-treatment_year +1, 0)))
reg = lm(death_per_100k ~ years_from_treat + Year + Population, data=df)
summary(reg)   
