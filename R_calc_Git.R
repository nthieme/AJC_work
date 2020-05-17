####Function to produce R_t estimates on the server
if (!require("pacman")) install.packages("pacman")
p_load("RMySQL", "tidyverse", "lubridate","EpiEstim","aws.s3","numbers")

D_data<-GET("https://covidtracking.com/api/states/daily.csv") %>% content
state_pops<-get_acs(geography = "state", variables = c(pop="B01003_001"))
state_key<-cbind(state.abb, state.name) %>% data.frame

covid_data<-D_data %>% left_join(state_key, by = c("state"="state.abb")) %>%
  left_join(state_pops, by = c("state.name"="NAME")) 

covid_data_f<-covid_data%>%
  mutate(date =str_c(str_sub(date, 1,4),"-",str_sub(date, 5,6),"-", 
                     str_sub(date, 7,8)) %>% ymd, 
         pop_thou=estimate/1000, 
         pos_t=positive/pop_thou,  
         neg_t=negative/pop_thou,
         pend_t=pending/pop_thou, 
         death_t=death/pop_thou, 
         tot_t=total/pop_thou,
         ifr = death/positive,
         pop_thou=pop_thou*1000) %>% 
  select(-state.name, -GEOID, -variable, -estimate, -moe) %>% 
  filter(state!="DC") %>% replace(is.na(.), 0) %>% 
  mutate(state = as_factor(state)) %>%  
  mutate(d_rate=1000*death/pop_thou)%>% 
  filter(state%in%c("GU","AS","MP", "PR","VI")==FALSE) %>% 
  select(date, state, positiveIncrease, deathIncrease, ifr, total, pop_thou)

d <- covid_data_f %>% select(DateRep = date, Cases = positiveIncrease, 
                             Deaths=deathIncrease,Country = state) %>% 
  left_join(tibble(state.name, state.abb), by = c("Country"="state.abb")) %>% 
  select(Country = state.name, DateRep, Cases, Deaths) %>% 
  mutate(Cases = ifelse(Cases<0,0, Cases),Deaths = ifelse(Deaths<0,0, Deaths))

d_f<-d %>% filter(Country=="Georgia") %>% select(dates = DateRep, I=Cases) %>% 
  arrange(dates)
#define MCMC parameters and estimate R_t accounting for si uncertainty
MCMC_seed <- 1
overall_seed <- 2
mcmc_control <- make_mcmc_control(seed = MCMC_seed, burnin = 5000)

r_t_est<-estimate_R(d_f, method="uncertain_si", 
                    config = make_config(list(
                      mcmc_control = mcmc_control,
                      seed = overall_seed, 
                      mean_si = 6.5*.62,
                      std_mean_si = .1,
                      min_mean_si = 2.5,
                      max_mean_si = 6.5,
                      std_si=sqrt(6.5*.62^2),
                      std_std_si = .3,
                      min_std_si = .1,
                      max_std_si=2.3)
                    )
) 


GT_obj <- R0::generation.time("gamma", val=c(6.5*.62,sqrt(6.5*.62^2)))

est_rt_wt <- function(ts, GT_obj) {
  end <- length(ts) - (length(GT_obj$GT)-1)
  R0::est.R0.TD(ts, GT=GT_obj, begin=1, end=end, nsim=10000)
}

#this seems a little ad-hoc, but I'm cutting off the early march dates where 
#there are no cases reported because it messes with the probability calcs
d_r_fitted<-tibble(I=d_f$I, dates=d_f$dates) %>% filter(dates>ymd("2020-03-09"))
x_ts<-d_r_fitted$I
names(x_ts)<-d_r_fitted$dates
rt_est_wt_act_fit<-est_rt_wt(x_ts, GT_obj)

plot(rt_est_wt_act_fit)
plot(r_t_est)

