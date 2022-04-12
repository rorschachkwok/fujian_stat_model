pacman::p_load(
        rstatix,
        openxlsx,
        coin,
        epikit,
        tidyverse
)

df <- read.xlsx('cleaned_data.xlsx')
cat_ll <- df %>% 
        mutate(
                age_cat = age_categories(             # create new column
                        age,                            # numeric column to make groups from
                        breakers = c(0, 10,  20,        # break points
                                     30, 40, 50, 60)),
                index_age_cat = age_categories(             # create new column
                        index_age,                            # numeric column to make groups from
                        breakers = c(0, 10,  20,        # break points
                                     30, 40, 50, 60))) %>% 
        mutate(vac_history = replace(vac_history, vac_history == 1, 0))


sele <- cat_ll %>% 
        filter(outcome == 1) %>% 
        filter(vac_history == 2) %>% 
        mutate(days_vac_exposure = as.numeric(days_vac_exposure)) 
        # group_by(age_cat) %>% 
        # summarise(iqr2 = median(days_vac_exposure),
        #           iqr1 = quantile(days_vac_exposure, probs = 0.25),
        #           iqr3 = quantile(days_vac_exposure, probs = 0.75))

sele_p <- cat_ll %>% 
        filter(outcome == 1) %>% 
        mutate(across(.cols = c(vac_history, gender, clnc_outcome),
                      .fns = as.factor))

median_test(age ~ vac_history, mid.score = '0.5', data = sele_p)
sele_p <- sele_p %>% 
        filter(vac_history == 2)
median_test(days_vac_exposure ~ age_cat, mid.score = '0.5', data = sele_p)

chisq.test(sele_p$gender, sele_p$vac_history)
chisq.test(sele_p$clnc_outcome, sele_p$vac_history, simulate.p.value = T)

sele_p %>% 
        filter(vac_history == 2) %>% 
        t_test(days_vac_exposure ~ age_cat)

sele_n <- cat_ll %>% 
        filter(outcome == 0) %>% 
        mutate(across(.cols = c(vac_history, gender, clnc_outcome),
                      .fns = as.factor)) 
sele_n %>% 
        t_test(age ~ vac_history)

median_test(age ~ vac_history,  data = sele_n)
sele_n <- sele_n %>% 
        filter(vac_history == 2)
median_test(days_vac_exposure ~ age_cat, data = sele_n)

chisq.test(sele_n$gender, sele_n$vac_history)

sele_n %>% 
        filter(vac_history == 2) %>% 
        # filter(age_cat != '0-9') %>% 
        t_test(age_cat ~ days_vac_exposure)





quantile(sele$days_vac_exposure, probs = seq(0, 1, 0.25))        
library(janitor)
df %>% 
        mutate(index_days_onset_quar = as.factor(index_days_onset_quar)) %>% 
        tabyl(index_days_onset_quar) %>% 
        adorn_percentages(denominator = "col") %>% 
        adorn_pct_formatting() %>% 
        adorn_ns(position = "front")# display as: "count (percent)# display as: "count (percent)"

sele <- df %>% 
        filter(outcome == 1) %>%
        mutate(at_most = replace(vac_history, vac_history == 1, 0)) %>%
        filter(at_most == 0) %>% 
        select(at_most, age)
sele %>% 
        tabyl(clnc_outcome, at_most) %>% 
        adorn_percentages(denominator = 'col') %>% 
        adorn_pct_formatting() %>% 
        adorn_ns(position = 'front')
