library(jsonlite)
library(httpuv)
library(httr)
library(tidyverse)

# Config Variables to loop over pages - add more as required
pages <- c(1,2,3,4)

# Remove dataframe
if (exists("git_df")){
  rm("git_df")
}

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "GetAppName",
                   key = "GetKey",
                   secret = "GetSecret")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
uri <- "https://api.github.com/repos/<owner>/<repo_name>/commits?per_page=50&page="

for(page in pages){
  
  uri_page <- paste0(uri,page)
  req <- GET(uri_page, gtoken)
  
  # Take action on http error
  stop_for_status(req)
  
  # Extract content from a request
  json_content = content(req, "text")
  
  # Convert to a data.frame
  page_df <- as.data.frame(fromJSON(json_content, flatten = TRUE))
  
  page_df <- page_df %>% 
    select(commit.author.name
           ,commit.author.date
           ,commit.message)

  # Convert to a data.frame
  git_df <- rbind(page_df,if(exists("git_df")) git_df)
}

git_df
