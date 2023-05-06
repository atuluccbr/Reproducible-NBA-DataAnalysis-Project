library(tidyverse)
library(dplyr)
library(broom)
library(ggplot2)

df1 <- read_csv("data/raw/2018-19_nba_team-statistics_1.csv")
df2 <- read_csv("data/raw/2018-19_nba_team-statistics_2.csv")
df3 <- read_csv("data/raw/2018-19_nba_player-statistics.csv")
df4 <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
team_pyrl <- read_csv("data/raw/2019-20_nba_team-payroll.csv")

df1 <- select(df1, - Rk)

##remove the last 3 columns of df1 as they looks like empty columns.

df1 <- df1[, 1:(ncol(df1)-3)]

##Every team played 82 games, the G column can be deleted.
##Ranking column Rk can be deleted as well

df2 <- select(df2, -c(Rk, G))

##In df3 dataframe player stats are listed. There is duplicate rows issue here.
##We need to first fix the duplicate rows for the players who were 
##traded during the season.
##These players have the row with 'TOT' in team column. This row is their total.
## I am deleting rows with TOT in team and then add duplicate rows.

df3 <- df3[!df3$Tm == 'TOT',]

##fixing the position variable (Pos)

df3$Pos[df3$Pos == "PG"] <- "1"
df3$Pos[df3$Pos == "SG"] <- "2"
df3$Pos[df3$Pos == "SG-PF"] <- "2"
df3$Pos[df3$Pos == "SG-SF"] <- "2"
df3$Pos[df3$Pos == "SF"] <- "3"
df3$Pos[df3$Pos == "SF-SG"] <- "3"
df3$Pos[df3$Pos == "PF"] <- "4" 
df3$Pos[df3$Pos == "PF-C"] <- "4"
df3$Pos[df3$Pos == "PF-SF"] <- "4"
df3$Pos[df3$Pos == "C"] <- "5" 
df3$Pos[df3$Pos == "C-PF"] <- "5"

##Remove column Tm

df3 <-select(df3, -c(Tm))

##add all numbers of duplicate rows and show a one unique row.
##remeber we have already deleted the rows where Tm == TOT

df3<- df3 %>%
  group_by(player_name, Pos) %>%
  summarise(across(everything(), sum))

## check if the function to sum all variables for duplicate rows working fine
## I checked the data and Ryan Anderson total sum of 3PA is 40

my_chk <- sum(subset(df3, player_name == "Ryan Anderson")$'3PA')
my_chk

## The check complete the code is working fine. 3PA total is 40.

##join data

nba_team_data <- left_join(df1, df2, by = c("Team"))

###replace 3 with letter three
names(nba_team_data) <- str_replace_all(string = names(nba_team_data),
                                             pattern = "3",
                                             replacement = "three_")

###replace 2 with letter two
names(nba_team_data) <- str_replace_all(string = names(nba_team_data),
                                             pattern = "2",
                                             replacement = "two_")


##remove special chr in the col names

names(nba_team_data) <- str_replace_all(string = names(nba_team_data),
                                      pattern = "%",
                                      replacement = "_pct")

##make all varibale names lowercase
names(nba_team_data) <- tolower(names(nba_team_data))

## same exercise for player stats but first join the player stat and their salary dataframes

player_stat <- left_join(df3, df4, by = c("player_name"))

#player_stat <- left_join(df3, df4, by = c("player_name"))

###replace 3 with letter three
names(player_stat) <- str_replace_all(string = names(player_stat),
                                        pattern = "3",
                                        replacement = "three_")

###replace 2 with letter two
names(player_stat) <- str_replace_all(string = names(player_stat),
                                        pattern = "2",
                                        replacement = "two_")


##remove special chr in the col names

names(player_stat) <- str_replace_all(string = names(player_stat),
                                        pattern = "%",
                                        replacement = "_pct")

##make all varibale names lowercase
names(player_stat) <- tolower(names(player_stat))

###Check the Nas
sum(is.na(nba_team_data))

###no Nas

##head
head(nba_team_data)

##tail
tail(nba_team_data)

##str

str(nba_team_data)

###above all code checked and working

################################EDA
################################





##############################################

# player_stat %>%
#   group_by(Pos) %>%
#   summarize(across(.cols = everything(), .fns = sum), .groups = "drop")
# 
# pos_player <- player_stat %>% 
#   group_by(player_name) %>% 
#   summarise(median = median(Pos))

###correlation

# calculate the correlation matrix
cor_matrix <- cor(nba_team_data[,c(3:42)])

# display the correlation matrix
print(cor_matrix)

# load the corrplot library
library(corrplot)

# create a correlation matrix plot
corrplot(cor_matrix, method="color", type="lower", order="hclust", tl.cex=0.7, tl.col="black", addCoef.col="black")

##is offensive rebounds %age is related to outcome of the match


##is defensive rebounds %age is related to outcome of the match

## does Blocks are factors in the winning of the game

## does steal are factors in the winning of the game


## Does turnover are factors in the winning of the game


## more field goal mean winning


## more 2 pointers means winning



## more 3 pointers means winning



### how is pace helping the team win the game


### does shot attempts have any relation in winning


### does team that have more budget gurranteed the success

### more salary mean more win

###how age is affecting


################################################

#cor(x = combined_team_stat$age, y = combined_team_stat$w, method = "pearson")
## 0.621794
##linear regression

# lm_plot <- ggplot(combined_team_stat, aes(x = drb_percage, y = w)) +
#   geom_point(colour = "dodgerblue") +
#   geom_smooth(method = "lm", colour = "magenta")
# 
# lm_plot
# 
# 
cor(x = nba_team_data$three_p, y = nba_team_data$w, method = "pearson")
# ##0.4838773
# 
# cor(x = combined_team_stat$`drb_percage`, y = combined_team_stat$w, method = "pearson")
# ## 0.44833
# 
# cor(x = combined_team_stat$`tov_percage`, y = combined_team_stat$w, method = "pearson")
# ## -0.3068
# 
# cor(x = combined_team_stat$stl, y = combined_team_stat$w, method = "pearson")
# ## 0.10202
# 
# cor(x = combined_team_stat$blk, y = combined_team_stat$w, method = "pearson")
# ## 0.44488
# 
# cor(x = combined_team_stat$ast, y = combined_team_stat$w, method = "pearson")
# ## 0.50187
# 
# ##linear regression
# lm_plot <- ggplot(combined_team_stat, aes(x = ast, y = l)) +
#   geom_point(colour = "dodgerblue") 
#   #geom_smooth(method = "lm", colour = "magenta")
# 
# lm_plot
# 
# ##use pairs to check correlation
# 
# pairs(formula = w ~ blk + stl + tov + ast, data = combined_team_stat)
# 
# pairs(formula = w ~ fg + three_p + two_p, data = combined_team_stat)
# 
# lm_blk <- lm(pts ~ blk + ast, data = combined_team_stat)
# tidy(lm_blk, conf.int = TRUE)

# How many more points will a team score if they increase the number 
# of field goals, but keep three pointers fixed?

##multiple linear regression

fit <- lm(w ~ pts + blk + three_p + fg + ast + tov, data = nba_team_data)
tidy(fit, conf.int = TRUE)

summary(fit)
## predicted value

head(predict(fit))


# The response variable should be continuous (i.e. ratio/interval)
# There are two or more explanatory variables that are either continuous or categorical
# There should be independence of observations
# There should be a linear relationship between the response variable and each explanatory variable
# There should be no significant outliers, influential or high leverage points
# The data needs to show homoscedasticity
# The residuals should be normally distributed
# There should not be multicollinearity between the explanatory variables

car::avPlots(fit)

## check multicollenearity
# 
# Multicollinearity occurs when two or more of your explanatory variables are highly 
# related with each other
# It can lead to changes in the coefficient estimates and confusion around which 
# variable is explaining the variance in the response variable

pairs(formula = ~ pts + blk + three_p + fg + ast + tov, data = nba_team_stat)

## variance inflation factor
car::vif(fit)

sqrt(car::vif(fit))

## the stand error of the coeff for blk is 1.25 times larger than if this variable
## had 0 correlation with other predictors



##outliers

###Detecting outliers

std_res <- rstandard(fit)
points <- 1:length(std_res)

##visualise it

ggplot(nba_team_data, aes(x = points, y = std_res)) +
  geom_point() +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

res_labels <- if_else(abs(std_res) >= 2.5, paste(points), "")
##replot it
ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  geom_text(aes(label = res_labels), nudge_y = 0.3) +
  ylim(c(-4, 4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

###leverage points

hats <- hatvalues(fit)

ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()

hat_labels <- if_else(hats >= 0.01, paste(points), "")

ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point() +
  geom_text(aes(label = hat_labels), nudge_y = 0.0005)

###Influence

###cooks distance

cook <- cooks.distance(fit)

ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point()

cook_label <- if_else(cook >= 0.015, paste(points), "")

ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text(aes(label  = cook_label), nudge_y = 0.001)


##independence of observations

library(car)

durbinWatsonTest(fit)



#####################################################
########################################################

###########Model Testing

#### Expected runs player metric

## remove the categorical variable player name and use player id instead

new_data <- nba_team_data %>% 
  select(pts, blk, three_p, fg, ast, tov)

#new_data$player_name <- NULL

new_data <- mutate(new_data, exp_pts = predict(fit, newdata = new_data))

new_data

##Selecting players by exp_Runs
new_ply_df <- player_stat %>% 
  select(player_id, pos, salary, pts, blk, three_p, fg, ast, tov)

##code breaking i need to delete character type column

new_ply_df$player_name <- NULL

##fit the model

new_ply_df <- mutate(new_ply_df, exp_pts_player = predict(fit, newdata = new_ply_df))


##new_ply_df <- left_join(new_ply_df, df4, by = c("player_name"))


new_ply_df %>%
  select(player_id, pos, salary, exp_pts_player) %>% 
  arrange(desc(exp_pts_player, salary)) %>% 
  top_n(10)

############################code worked till here
#############################################################

players %>% 
  select(player_name, player_id, pos, salary, exp_Runs) %>% 
  arrange(desc(exp_Runs), salary) %>%
  top_n(10)

#####working

# display the data summary
summary(nba_team_stat)

# plot a scatter plot of Wins vs. Points
plot(nba_team_stat$w, nba_team_stat$pts, xlab="Wins", ylab="Points", main="NBA Teams Wins vs Points")

# calculate the correlation between Wins and Points
cor(nba_team_stat$w, nba_team_stat$pts)

## more points more wins 66% chances of winning if you score 