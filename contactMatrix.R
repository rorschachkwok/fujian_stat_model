pacman::p_load(
        socialmixr,
        openxlsx,
        tidyverse
)


# build whole contact -----------------------------------------------------
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

all_cntr <- as.data.frame(cnt_mr / 2)

# save contact matrix
write.xlsx(all_cntr, 'all_cntm.xlsx')

max_eigen <- function(m) {
        eig <- eigen(m)
        max_eig <- eig$values[[1]]
        max_eig
}
# mean contact of cases and close contact in Fujian

boot_list <- cnt_m$matrices
boot_matrix <- list()
for (i in seq_along(boot_list)) {
        boot_matrix[[i]] <- boot_list[[i]][["matrix"]]
}
boot_vector <- vector(mode = 'numeric', length = 100)
for (i in seq_along(boot_matrix)) {
        boot_vector[i] <- max_eigen(boot_matrix[[i]])
}
boot_vector <- boot_vector / 2
shapiro.test(boot_vector)
hist(boot_vector)
center <- mean(boot_vector)
stddev <- sd(boot_vector)
error <- qnorm(0.975)*stddev/sqrt(100)
lower_bound <- center - error
upper_bound <- center + error

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

cno_cntr <- as.data.frame(cnt_mr / 2)

write.xlsx(cno_cntr, 'cntm_cno.xlsx')

# mean contact in community and others

boot_list <- cnt_m$matrices
boot_matrix <- list()
for (i in seq_along(boot_list)) {
        boot_matrix[[i]] <- boot_list[[i]][["matrix"]]
}
boot_vector <- vector(mode = 'numeric', length = 100)
for (i in seq_along(boot_matrix)) {
        boot_vector[i] <- max_eigen(boot_matrix[[i]])
}
boot_vector <- boot_vector / 2
center <- mean(boot_vector)
stddev <- sd(boot_vector)
error <- qnorm(0.975)*stddev/sqrt(100)
lower_bound <- center - error
upper_bound <- center + error

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

sc_cntr <- as.data.frame(cnt_mr / 2)

write.xlsx(sc_cntr, 'cntm_cut_school.xlsx')
# mean contact in school closure

boot_list <- cnt_m$matrices
boot_matrix <- list()
for (i in seq_along(boot_list)) {
        boot_matrix[[i]] <- boot_list[[i]][["matrix"]]
}
boot_vector <- vector(mode = 'numeric', length = 100)
for (i in seq_along(boot_matrix)) {
        boot_vector[i] <- max_eigen(boot_matrix[[i]])
}
boot_vector <- boot_vector / 2
center <- mean(boot_vector)
stddev <- sd(boot_vector)
error <- qnorm(0.975)*stddev/sqrt(100)
lower_bound <- center - error
upper_bound <- center + error

# build factory closure contact ------------------------------------------------------
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

fc_cntr <- as.data.frame(cnt_mr / 2)

write.xlsx(fc_cntr, 'cntm_cut_factory.xlsx')
# mean contact in factory closure

boot_list <- cnt_m$matrices
boot_matrix <- list()
for (i in seq_along(boot_list)) {
        boot_matrix[[i]] <- boot_list[[i]][["matrix"]]
}
boot_vector <- vector(mode = 'numeric', length = 100)
for (i in seq_along(boot_matrix)) {
        boot_vector[i] <- max_eigen(boot_matrix[[i]])
}
boot_vector <- boot_vector / 2
center <- mean(boot_vector)
stddev <- sd(boot_vector)
error <- qnorm(0.975)*stddev/sqrt(100)
lower_bound <- center - error
upper_bound <- center + error

# build school contact ---------------------------------------------------------------
contacts <- read.xlsx('cleaned_data.xlsx')

select <- dplyr::select
participants <- contacts %>% 
        select(index_name, index_age) %>% 
        distinct(index_name, index_age) %>% 
        rowid_to_column(var = 'part_id')

contacts <- rowid_to_column(contacts, "cont_id") 
contacts <- contacts %>% 
        filter(index_job == 1) %>% 
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
s_data <- survey(cntMatrix$participants, cntMatrix$contacts)

cnt_m <- contact_matrix(s_data, age.limits = c(0, 10, 20, 30, 40, 50, 60), survey.pop = pop, symmetric = T, n = 100)
length(cnt_m$matrices)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)

s_cntr <- as.data.frame(cnt_mr / 2)
write.xlsx(s_cntr, 'cntm_school.xlsx')

# mean contact in school
boot_list <- cnt_m$matrices
boot_matrix <- list()
for (i in seq_along(boot_list)) {
        boot_matrix[[i]] <- boot_list[[i]][["matrix"]]
}
boot_vector <- vector(mode = 'numeric', length = 100)
for (i in seq_along(boot_matrix)) {
        boot_vector[i] <- max_eigen(boot_matrix[[i]])
}
boot_vector <- boot_vector / 2
center <- mean(boot_vector)
stddev <- sd(boot_vector)
error <- qnorm(0.975)*stddev/sqrt(100)
lower_bound <- center - error
upper_bound <- center + error

# build factory contact ---------------------------------------------------------------
contacts <- read.xlsx('cleaned_data.xlsx')

select <- dplyr::select
participants <- contacts %>% 
        select(index_name, index_age) %>% 
        distinct(index_name, index_age) %>% 
        rowid_to_column(var = 'part_id')

contacts <- rowid_to_column(contacts, "cont_id") 
contacts <- contacts %>% 
        filter(index_job == 2) %>% 
        mutate(cnt_age_exact = age) %>% 
        select(cont_id, cnt_age_exact, index_name)

# join contact and case data frame
contacts <- contacts %>% 
        left_join(participants, by = c('index_name'))%>% 
        select(- index_name, -index_age) 


participants <- participants %>% 
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
f_data <- survey(cntMatrix$participants, cntMatrix$contacts)

cnt_m <- contact_matrix(f_data, age.limits = c(0, 10, 20, 30, 40, 50, 60), survey.pop = pop, symmetric = T, n = 100)
length(cnt_m$matrices)
cnt_mr <- Reduce("+", lapply(cnt_m$matrices, function(x) {x$matrix})) / length(cnt_m$matrices)

f_cntr <- as.data.frame(cnt_mr / 2)
write.xlsx(f_cntr, 'cntm_factory.xlsx')

# mean contact in factory
boot_list <- cnt_m$matrices
boot_matrix <- list()
for (i in seq_along(boot_list)) {
        boot_matrix[[i]] <- boot_list[[i]][["matrix"]]
}
boot_vector <- vector(mode = 'numeric', length = 100)
for (i in seq_along(boot_matrix)) {
        boot_vector[i] <- max_eigen(boot_matrix[[i]])
}
boot_vector <- boot_vector / 2
center <- mean(boot_vector)
stddev <- sd(boot_vector)
error <- qnorm(0.975)*stddev/sqrt(100)
lower_bound <- center - error
upper_bound <- center + error