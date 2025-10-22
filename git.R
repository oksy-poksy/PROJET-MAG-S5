library(usethis)
use_git_config(user.name = "oksy-poksy", user.email = "sana.ptashnyk@gmail.com")
create_github_token()
gitcreds::gitcreds_set()
use_github()

#update.packages(c("curl", "httr2", "usethis"), ask = FALSE)
#update.packages(ask = FALSE)
