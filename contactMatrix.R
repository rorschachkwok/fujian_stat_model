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

all_cntr <- as.data.frame(cnt_mr / 4)

# save contact matrix
write.xlsx(all_cntr, 'all_cntm.xlsx')


# build community and others contact --------------------------------------
# tidy contact data
contacts <- read.xlsx('cleaned_data.xlsx')

participants <- contacts %>% 
        select(index_name, index_age) %>% 
        distinct(index_name, index_age) %>% 
        rowid_to_column(var = 'part_id')

contacts <- rowid_to_column(contacts, "cont_id") 
contacts <- contacts %>% 
        filter(index_job == 0) %>% 
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
cno_data <- survey(cntMatrix$participants, cntMatrix$contacts)

cnt_m <- contact_matrix(cno_data, age.limits = c(0, 10, 20, 30, 40, 50, 60), survey.pop = pop, symmetric = T, n = 100)
length(cnt_m$matrices)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)

cno_cntr <- as.data.frame(cnt_mr / 4)

write.xlsx(cno_cntr, 'cno_cntm.xlsx')


# build school closure contact ------------------------------------------------------
# tidy contact data
contacts <- read.xlsx('cleaned_data.xlsx')

participants <- contacts %>% 
        select(index_name, index_age) %>% 
        distinct(index_name, index_age) %>% 
        rowid_to_column(var = 'part_id')

contacts <- rowid_to_column(contacts, "cont_id") 
contacts <- contacts %>% 
        filter(index_job != 1) %>% 
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
s_closure_data <- survey(cntMatrix$participants, cntMatrix$contacts)

cnt_m <- contact_matrix(s_closure_data, age.limits = c(0, 10, 20, 30, 40, 50, 60), survey.pop = pop, symmetric = T, n = 100)
length(cnt_m$matrices)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)

sc_cntr <- as.data.frame(cnt_mr / 4)

write.xlsx(sc_cntr, 'sc_cntm.xlsx')

# build school closure contact ------------------------------------------------------
# tidy contact data
contacts <- read.xlsx('cleaned_data.xlsx')

participants <- contacts %>% 
        select(index_name, index_age) %>% 
        distinct(index_name, index_age) %>% 
        rowid_to_column(var = 'part_id')

contacts <- rowid_to_column(contacts, "cont_id") 
contacts <- contacts %>% 
        filter(index_job != 2) %>% 
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
f_closure_data <- survey(cntMatrix$participants, cntMatrix$contacts)

cnt_m <- contact_matrix(f_closure_data, age.limits = c(0, 10, 20, 30, 40, 50, 60), survey.pop = pop, symmetric = T, n = 100)
length(cnt_m$matrices)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)

fc_cntr <- as.data.frame(cnt_mr / 4)

write.xlsx(fc_cntr, 'fc_cntm.xlsx')

# build school contact
all_cntm <- read.xlsx('contactMatrix.xlsx')
s_cntm <- as.matrix(all_cntm) - as.matrix(sc_cntr)
f_cntm <- as.matrix(all_cntm) - as.matrix(fc_cntr)
s_cntm <- as.data.frame(s_cntm)
f_cntm <- as.data.frame(f_cntm)


write.xlsx(s_cntm, 's_cntm.xlsx')
write.xlsx(f_cntm, 'f_cntm.xlsx')
