pacman::p_load(
        socialmixr,
        openxlsx,
        tidyverse
)

set.seed(10)
# read age-grouped population file
pop <- read.xlsx('pop_fujian.xlsx', sheet = 4)
contacts <- read.csv('contact_fujian.csv')
participants <- read.csv('participant_fujian.csv')

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
cnt_mr / 4 
df_cntr <- as.data.frame(cnt_mr / 4)

write.xlsx(df_cntr, 'contactMatrix.xlsx')
