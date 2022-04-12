pacman::p_load(
        tidyverse,
        openxlsx,
        epikit,
        janitor
)
linelist <- read.xlsx('cleaned_data.xlsx', sheet = 1)
cat_ll <- linelist %>% 
        distinct(index_name, .keep_all = T) %>% 
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
cat_ll %>% 
        tabyl(vac_history, index_age_test)

table(cat_ll$age_test)
table(cat_ll$index_age_test)

(0.0265*6 + 0.0887*2 + 0.2254*1)/9
(0.0102*6 + 0.0564*3 + 0.1307*2)/11
# hongkong data
172854+81660+59204
(0.012*172854 + 0.0338*81660 + 0.1244*59204)/313718
(0.001*172854 + 0.0041*81660 + 0.0307*59204)/313718
