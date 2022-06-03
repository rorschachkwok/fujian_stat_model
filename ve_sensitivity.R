pacman::p_load(
        tidyverse,
        survival,
        epikit,
        openxlsx,
        My.stepwise
)

linelist <- read.xlsx("cleaned_data.xlsx")

# DATA MANAGEMENT
tidy_linelist <- linelist %>% 
        mutate(delta1 = outcome) %>% 
        mutate(deltaA = replace(vac_history_st, vac_history_st == '2', 1)) %>%
        mutate(across(.cols = c(gender, age_cat, index_gender, index_age_cat,
                                index_days_onset_quar),
                      .fns = as.factor)) %>% 
        rowid_to_column("my_id")

# CATEGORISED DAYS FROM VACCINATION TO EXPOSURE
cat_linelist <- tidy_linelist %>% 
        mutate(month_cat = cut(
                days_vac_quarantine,
                breaks = c(0, 14, 31, 61, 300),
                right = F,
                labels = c('0-13', '14-30', '31-60', '61-')),
               
               ## make missing values explicit
               month_cat = fct_explicit_na(
                       month_cat,
                       na_level = "unvaccinated")) %>%   # you can specify the label
        mutate(month_cat = as.factor(month_cat),
               month_cat = fct_relevel(month_cat, 'unvaccinated', after = 0)) %>% 
        droplevels()


# WITH PARTICIPANTS ENTERING COHORT AS UNVACCINATED(initial try)
td_dat <-
        tmerge(
                data1 = cat_linelist %>% select(my_id, T1, delta1),
                data2 = cat_linelist %>% select(my_id, T1, delta1, TA, deltaA),
                id = my_id,
                infect = event(T1, delta1),
                vachis = tdc(TA)
        )


# VLOOKUP PROCEDURE
lookup <- cat_linelist %>% 
        select(my_id, age_cat, gender, month_cat, index_gender,
               index_age_cat, index_days_onset_quar)

td_dat <- td_dat %>% 
        left_join(lookup, by = c('my_id'))

td_dat <- td_dat %>% 
        mutate(month_cat = as.character(month_cat)) %>% 
        mutate(month_cat = case_when(
                vachis == 0 ~ "unvaccinated",
                vachis == 1 ~ month_cat
        )) %>% 
        mutate(month_cat = as.factor(month_cat),
               month_cat = fct_relevel(month_cat, 'unvaccinated', after = 0))

# WITH PARTICIPANTS ENTERING COHORT AS first case occurred symptoms
# all fully vaccinated
td_model = coxph(
        Surv(time = tstart, time2 = tstop, event = infect) ~ month_cat + age_cat
        + gender + index_gender + index_age_cat + index_days_onset_quar, 
        data = td_dat
)
summary(td_model) 

ph_test <- cox.zph(td_model)
ph_test
