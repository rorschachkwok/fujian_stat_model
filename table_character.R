pacman::p_load(
        rstatix,
        openxlsx,
        coin,
        epikit,
        tidyverse,
        janitor
)

df <- read.xlsx('cleaned_data.xlsx')
index_df <- df %>% distinct(index_name, .keep_all = T) 
table(index_df$index_job)

# make age groups, vac status classify as at most partially and fully
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
        mutate(vac_history = replace(vac_history, vac_history == 1, 0)) %>% 
        mutate(across(.cols = c(vac_history, gender),
                      .fns = as.factor)) %>% 
        mutate(days_vac_exposure = as.numeric(days_vac_exposure),
               clnc_outcome      = as.numeric(clnc_outcome))
table(cat_ll$age_cat)



# select           PCR positive           close contacts
sele_p <- cat_ll %>% 
        filter(outcome == 1)

# see the age distribution of at most partially vac and PCR positive close contacts
sele_p_amp <- sele_p %>% filter(vac_history == 0)
hist(sele_p_amp$age)

# stat-test PCR positive close contacts
wilcox.test(age ~ vac_history, sele_p)
# median_test(age ~ vac_history, mid.score = '0.5', data = sele_p)
chisq.test(sele_p$gender, sele_p$vac_history)
wilcox.test(clnc_outcome ~ vac_history, sele_p)

# select PCR positive and fully vaccinated in close contacts
sele_p_f <- cat_ll %>% 
        filter(outcome == 1) %>% 
        filter(vac_history == 2)
        # group_by(age_cat) %>%
        # summarise(iqr2 = median(days_vac_exposure),
        #           iqr1 = quantile(days_vac_exposure, probs = 0.25),
        #           iqr3 = quantile(days_vac_exposure, probs = 0.75))

# test median time from fully-vac to exposure between age groups
kruskal.test(days_vac_exposure ~ age_cat, sele_p_f)
# median_test(days_vac_exposure ~ age_cat, mid.score = '0.5', data = sele_p_f)

# select PCR negative close contacts
sele_n <- cat_ll %>% 
        filter(outcome == 0) 
# stat-test PCR negative close contacts
chisq.test(sele_n$gender, sele_n$vac_history)
# median_test(age ~ vac_history, mid.score = '0.5', data = sele_n)
wilcox.test(age ~ vac_history, sele_n)
# select PCR negative and fully vaccinated in close contacts
sele_n_f <- sele_n %>% 
        filter(vac_history == 2)
kruskal.test(days_vac_exposure ~ age_cat, data = sele_n_f)
# median_test(days_vac_exposure ~ age_cat, data = sele_n_f)
# sele_n_amp <- sele_n %>% filter(vac_history == 0)
# hist(sele_n_amp$age)




quantile(sele$days_vac_exposure, probs = seq(0, 1, 0.25))        
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

sele_n %>% 
        tabyl(gender, vac_history) %>% 
        adorn_totals(where = 'both') %>% 
        adorn_percentages(denominator = 'col') %>% 
        adorn_pct_formatting() %>% 
        adorn_ns(position = 'front')


# compare ct value in different vaccination status ------------------------

sele_p <- df %>% 
        select(id, outcome, orf1ab, n_gene, vac_history) %>% 
        filter(outcome == 1) %>% 
        drop_na(orf1ab) %>% 
        mutate(vac_history = replace(vac_history, vac_history == 1, 0)) 
        
shapiro.test(sele_p$orf1ab)
shapiro.test(sele_p$n_gene)

t.test(orf1ab ~ vac_history, data = sele_p)
t.test(n_gene ~ vac_history, data = sele_p)

sele_p %>% 
        group_by(vac_history) %>% 
        summarise(SD = sd(orf1ab),
                  SD2 = sd(n_gene))

# if ct-value <25, then change it to '<25', otherwise to '25-40'
sele_p2 <- sele_p %>% 
        mutate(orf1ab = case_when(
                orf1ab < 25 ~ '<25',
                orf1ab >= 25 ~ '>=25'
        )) %>% 
        mutate(n_gene = case_when(
                n_gene < 25 ~ '<25',
                n_gene >= 25 ~ '>=25'
        ))
sele_p2 %>% 
        count(vac_history, n_gene)
