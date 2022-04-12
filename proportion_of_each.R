pacman::p_load(
        tidyverse,
        openxlsx,
        epikit
)
df <- read.xlsx('cleaned_data.xlsx')

count_df <- df %>% 
        mutate(d_since_vac = age_categories(days_vac_exposure,
                                            breakers = c(0, 14, 31, 61))) %>% 
        count(age_cat, d_since_vac) %>% 
        mutate(n_percent = n / 8969,
               n_fujian = round(n_percent * 41540086))
write.xlsx(count_df, 'pop_of_S.xlsx')


# 7 age group -------------------------------------------------------------
df <- read.xlsx('cleaned_data.xlsx')

count_df <- df %>% 
        mutate(d_since_vac = age_categories(days_vac_exposure,
                                            breakers = c(0, 14, 31, 61))) %>% 
        mutate(age_cat7    = age_categories(age,
                                            breakers = c(0, 10, 20, 30, 40, 50, 60))) %>% 
        count(age_cat7, d_since_vac) %>% 
        mutate(n_percent = n / 8969,
               n_fujian = round(n_percent * 41540086))
write.xlsx(count_df, 'pop_of_S_7age.xlsx')


