#install.packages(c("httr", "devtools")) 
#devtools::install_github("r-lib/gh") #https://github.com/r-lib/gh
library(httr) 
library(gh) 
library(tidyverse)
library(foreach) 
library(jsonlite)

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

 





