library(jsonlite)
library(httpuv)
library(httr)
library(tidyverse)
library(RColorBrewer)

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
uri <- "https://api.github.com/repos/william-kent-stds/stds_edu/commits?per_page=50&page="

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
<<<<<<< HEAD

git_df %>% 
  mutate(Committer = case_when(`commit.author.name` %in% c("will.kent","william-kent-stds","William Kent","Will Kent") ~ "William Kent",
                   TRUE ~ "Team"),
         Commit_Date = as.Date(`commit.author.date`)) %>% 
  group_by(Committer, Commit_Date) %>% 
  summarise(Total = n()) %>% 
  ggplot() +
  geom_bar(aes(x = Commit_Date, y = Total, fill = Committer), stat = "identity") +
  ggtitle("Commits by Team Members Throughout the Project") +
  xlab("Commit Date") +
  ylab("Number of Commits") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set2")
=======
>>>>>>> d33b1766ffa58605e8c49d0c0e4e06d06f7a9a65
