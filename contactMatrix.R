pacman::p_load(
        socialmixr,
        openxlsx,
        tidyverse
)


set.seed(10)
# read age-grouped population file
pop <- read.xlsx('pop_fujian.xlsx', sheet = 4)

# tidy contact data
contacts <- read.xlsx('cleaned_data.xlsx')

participants <- contacts %>% 
        select(index_name, index_age) %>% 
        distinct(index_name, index_age) %>% 
        rowid_to_column(var = 'part_id')

contacts <- rowid_to_column(contacts, "cont_id") 
contacts <- contacts %>% 
        mutate(cnt_age_exact = age) %>% 
        select(cont_id, cnt_age_exact, index_name)


# join contact and case data frame
contacts <- contacts %>% 
        left_join(participants, by = c('index_name'))%>% 
        select(- index_name, -index_age) 

# colnames(contacts)[3] <- c('part_id')


participants <- participants %>% 
        # select(-index_name) %>% 
        transmute(part_id = part_id,
                  part_age = index_age)


participants <- participants %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))

contacts <- contacts %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))

pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))
# put into a list
cntMatrix <- list(participants, contacts)
names(cntMatrix)[2] <- 'contacts'
names(cntMatrix)[1] <- 'participants'

# construct 'survey' data
new_survey <- survey(cntMatrix$participants, cntMatrix$contacts)

cnt_m <- contact_matrix(new_survey, age.limits = c(0, 10, 20, 30, 40, 50, 60), survey.pop = pop, symmetric = T, n = 100)
length(cnt_m$matrices)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)

df_cntr <- as.data.frame(cnt_mr / 4)
# df_cnt <- as.data.frame(cnt_m$matrix / 4)


# save contact matrix
write.xlsx(df_cntr, 'contactMatrix.xlsx')


# Cut school ------------------------------------------------------

set.seed(10)
# read age-grouped population file
pop <- read.xlsx('pop_fujian.xlsx', sheet = 4)

# tidy contact data
contacts <- read.xlsx('build_scale.xlsx', sheet = 'cut_school')

participants <- contacts %>% 
        select(index_name, index_age) %>% 
        distinct(index_name, index_age) %>% 
        rowid_to_column(var = 'part_id')

contacts <- rowid_to_column(contacts, "cont_id") 
contacts <- contacts %>% 
        mutate(cnt_age_exact = age) %>% 
        select(cont_id, cnt_age_exact, index_name)

# join contact and case data frame
contacts <- contacts %>% 
        left_join(participants, by = c('index_name'))%>% 
        select(- index_name, -index_age) 


participants <- participants %>% 
        # select(-index_name) %>% 
        transmute(part_id = part_id,
                  part_age = index_age)


participants <- participants %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))

contacts <- contacts %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))

pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))
# put into a list
cntMatrix <- list(participants, contacts)
names(cntMatrix)[2] <- 'contacts'
names(cntMatrix)[1] <- 'participants'

# construct 'survey' data
new_survey <- survey(cntMatrix$participants, cntMatrix$contacts)

cnt_m <- contact_matrix(new_survey, age.limits = c(0, 10, 20, 30, 40, 50, 60), survey.pop = pop, symmetric = T, n = 100)
length(cnt_m$matrices)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)

df_cntr <- as.data.frame(cnt_mr / 4)

write.xlsx(df_cntr, 'cut_school_cntm.xlsx')


# Build factory contact -----------------------------------------------------
set.seed(10)
# read age-grouped population file
pop <- read.xlsx('pop_fujian.xlsx', sheet = 4)

# tidy contact data
contacts <- read.xlsx('build_scale.xlsx', sheet = 'factory')

participants <- contacts %>% 
        select(index_name, index_age) %>% 
        distinct(index_name, index_age) %>% 
        rowid_to_column(var = 'part_id')

contacts <- rowid_to_column(contacts, "cont_id") 
contacts <- contacts %>% 
        mutate(cnt_age_exact = age) %>% 
        select(cont_id, cnt_age_exact, index_name)

# join contact and case data frame
contacts <- contacts %>% 
        left_join(participants, by = c('index_name'))%>% 
        select(- index_name, -index_age) 


participants <- participants %>% 
        # select(-index_name) %>% 
        transmute(part_id = part_id,
                  part_age = index_age)


participants <- participants %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))

contacts <- contacts %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))

pop <- pop %>% 
        mutate(across(.cols = everything(),
                      .fns = as.integer))
# put into a list
cntMatrix <- list(participants, contacts)
names(cntMatrix)[2] <- 'contacts'
names(cntMatrix)[1] <- 'participants'

# construct 'survey' data
new_survey <- survey(cntMatrix$participants, cntMatrix$contacts)

cnt_m <- contact_matrix(new_survey, age.limits = c(0, 10, 20, 30, 40, 50, 60), survey.pop = pop, symmetric = T, n = 100)
length(cnt_m$matrices)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)

df_cntr <- as.data.frame(cnt_mr / 4)

df_cntr[is.na(df_cntr)] <- 0

write.xlsx(df_cntr, 'factory_cntm.xlsx')


# cut factory -------------------------------------------------------------

factory <- read.xlsx('factory_cntm.xlsx')
whole <- read.xlsx('contactMatrix.xlsx', colNames = F)
whole <- whole[-1 , -1]
whole <- whole %>% 
        mutate(across(.cols = everything(),
                      .fns = as.numeric))
cnt_mr <- cnt_mr / 4
rd_mt <- as.matrix(whole) - as.matrix(df_cntr)
rd_mt <- as.data.frame(rd_mt)
write.xlsx(rd_mt, 'cut_factory.xlsx')


# Build school contact ------------------------------------------------------------

df_cntr[is.na(df_cntr)] <- 0
school <- read.xlsx('cut_school.xlsx', colNames = F)
school <- school[-1,]
school <- school %>% 
        mutate(across(.cols = everything(),
                      .fns = as.numeric))
whole <- read.xlsx('contactMatrix.xlsx', colNames = F)
whole <- whole[-1,]
whole <- whole %>% 
        mutate(across(.cols = everything(),
                      .fns = as.numeric))
# cnt_mr <- cnt_mr / 4
rd_mt <- as.matrix(whole) - as.matrix(df_cntr)
rd_mt <- as.data.frame(rd_mt)

rd_mt2 <- as.matrix(whole) - as.matrix(school)
rd_mt2 <- as.data.frame(rd_mt2)

write.xlsx(rd_mt, 'school_cntm.xlsx')
