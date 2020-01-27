rm(list = ls())
.rs.restartR()

library(gh) 
library(tidyverse)
library(foreach) 
library(tidytext) 
library(stringr) 
library(tibble)
library(jsonlite)
library(dplyr)
library(repurrrsive) 
library(lubridate) 
library(ggplot2)


my_token = "18a65c305fad526b4a552fb910127202001e3b73" 
Sys.setenv(GITHUB_TOKEN = my_token)

# Repos of github user
isteves_repo <- gh("/users/isteves/repos", .token = my_token,.limit =Inf)

foreach(i = 1:length(isteves_repo)) %do% 
{write_json(isteves_repo[[i]], 
           (path = str_c("~/gsu/CIS8392/repo/", 
                         isteves_repo[[i]][["name"]], ".json")),
           pretty = TRUE)
}

# Followers of github user
isteves_follower <- gh("/users/isteves/followers", .token = my_token,
                    .limit =Inf)

foreach(i = 1:length(isteves_follower)) %do% 
{write_json(isteves_follower[[i]], 
            (path = str_c("~/gsu/CIS8392/follower/", 
                          isteves_follower[[i]][["login"]], ".json")),
            pretty = TRUE)
}


## Issues of each repository
foreach(i = 1:length(isteves_repo)) %do% 
  {write_json((gh("/repos/:owner/:repo/issues?state=all", 
                owner = "isteves", repo = isteves_repo[[i]][["name"]])), 
            (path = str_c("~/gsu/CIS8392/issue/",
                          isteves_repo[[i]][["name"]], "_issue.json")), 
            pretty = TRUE)
}


##______________________________________________________________________________
follower <- "follower/" #followers folder in your working directory
f_names <- list.files(follower, recursive = T) #get all filenames under follower
follower_path <- str_c(follower, f_names) # set follower file path

#create empty dataframe and name columns
#follower_df <- data.frame(matrix(ncol = 3, nrow = 0))
#colnames(follower_df) <- c("follower_login", "follower_id", "follower_url")

#foreach(i = 1:length(f_names)) %do% 
#{value <- fromJSON(follower_path[i])
#follower_df <- rbind(follower_df, tibble(follower_login = value$login, 
 #                                        follower_id = value$id, 
 #                                        follower_url = value$url))
#}

follower_df <-foreach(i = 1:length(f_names), .combine = rbind) %do% 
{value <- fromJSON(follower_path[i])
tibble(follower_login = value$login, follower_id = value$id, 
       follower_url = value$url)
}
follower_df
##-------------------------------------------------------------
isteves <- gh("/users/isteves", .limit = Inf)

isteves_df <- tibble(user_name = isteves$name, user_login = isteves$login, 
                     user_id = isteves$id, repos = isteves$public_repos,
                     followers = isteves$followers, date_created = isteves$created_at)
isteves_df

##-------------------------------------------------------------
#repo <- "repo/" #repo folder in your working directory
#r_names <-list.files(repo, recursive = TRUE) # get all filenames under repo
#repo_path <- str_c(repo, r_names) #set repo file path

#create empty dataframe and name columns
#repo_df <- data.frame(matrix(ncol = 7, nrow = 0))
#colnames(repo_df) <- c("repo_name", "language", "size", "fork_count", 
#                       "stargazers_count", "watcher_count", "open_issue_count")

repo <- "repo/" #repo folder in your working directory
r_names <-list.files(repo, recursive = TRUE) # get all filenames under repo
repo_path <- str_c(repo, r_names) #set file path

repo_df <- foreach(i = 1:length(r_names), .combine = rbind) %do% 
{value <- fromJSON(repo_path[i])
if(is.list(value$language) == TRUE) {
  value$language <- "unknown"
}
tibble(repo_name = value$name, language = value$language, 
       size = value$size, fork_count = value$forks_count,
       stargazers_count = value$stargazers_count, 
       watcher_count = value$watchers_count,
       open_issue_count = value$open_issues_count,
       created = as.Date(value$created_at))
}

#---------------------------------------------------

##__issue_df_______________________________________________________________
issue <- "issue/" #issue folder in working directory
issue_names <- list.files(issue, recursive = T) #get all filenames under issue
issue_path <- str_c(issue, issue_names)

issue_df <- foreach(i = 1:length(issue_names), .combine = rbind) %do% 
  {if(is.list(fromJSON(issue_path[i])) == TRUE){
    i_value <- fromJSON(issue_path[i])
    r_value <- fromJSON(repo_path[i])
    tibble(repo_name = r_value$name, 
           issues_all = max(as.numeric(i_value$number)),
           no_of_open_issues = r_value$open_issues, 
           no_of_closed_issues = (issues_all - no_of_open_issues))
              
        }
  }
#_duration_df_________________________________________________________
issue <- "issue/" #issue folder in working directory
issue_df_names <- as.list(issue_df$repo_name) #get all filenames under issue
issue_files <- str_c(issue, issue_df_names, "_issue.json")


df <- foreach(i = 1:length(issue_files), .combine = rbind) %do%
{file <- fromJSON(issue_files[i])
df2 <-  foreach(j = 1:length(file$state), .combine = rbind) %do%
    {if(file$state[j] == "closed"){
      open <- as.Date(unlist(file$created_at[j]))
      closed <-as.Date(unlist(file$closed_at[j]))
      repo_url <- unlist(file$repository_url[j])
      tibble(repo_name = str_sub(repo_url, start = 38), open = open, 
             closed = closed, duration = (closed - open))
    }
    }
  df2
}
df

duration_df <- df %>%
  group_by(repo_name) %>%
  summarise(avg_days_to_close = mean(duration)) 

issues_final <- issue_df %>% left_join(duration_df)

#------------------------------------------------------------
typeof(duration_df)
fromJSON(issue_files[10])
         
         
closed_df <- data.frame(matrix(unlist(file$closed_at)))
is_null(file$created_at[1])
typeof(file$created_at[1])
file$state[1]
date_created <- as.Date(unlist(file$created_at[1]))
date_created <- as.Date(date_created)
typeof(date_created)


#copy
issue_df <- foreach(i = 1:length(issue_names), .combine = rbind) %do% 
{if(is.list(fromJSON(issue_path[i])) == TRUE){
  i_value <- fromJSON(issue_path[i])
  r_value <- fromJSON(repo_path[i])
  tibble(repo_name = r_value$name,
         issues_all = max(as.numeric(i_value$number)),
         no_of_open_issues = r_value$open_issues, 
         no_of_closed_issues = (issues_all - no_of_open_issues))
        
}
}




foreach(i = 1:length(i_value$closed_at)) {
  if(is.NULL(i_value$closed_at[i])== FALSE){
    date_closed <- as.Date(unlist(i_value$closed_at[i]))
    date_opened <- as.Date(unlist(i_value$created_at[i]))
  }
}



duration_df <- foreach (i = 1:length(issue_names), .combine = rbind) %do%
  if(is.list(fromJSON(issue_path[i])) == TRUE){
    i_value <- fromJSON(issue_path[i])
    
      date_closed <- unlist(i_value$closed_at[i])
      date_opened <- unlist(i_value$created_at[j])
      tibble(closed = date_closed, opened = date_opened)
       
             #duration = (date_closed - date_opened))
      }


##Plots    
ggplot(issue_df, aes(repo_name, issues_all, fill = no_of_open_issues)) +
  geom_bar(stat = "identity")

ggplot(repo_df, aes(language)) + geom_bar()

options(scipen = 999)
filter_df <- filter(repo_df, size > 14000)
ggplot(filter_df, aes(created, size, color = repo_name)) + geom_point()
