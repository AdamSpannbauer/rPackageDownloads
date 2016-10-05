################################
# Takes about 5 minutes to run #
################################

cat("STARTING:",as.character(Sys.time()),"\n")
library(dplyr)
library(magrittr)
#used not loaded
# library(httr)
# library(cranlogs)

#get data frame of packages and their most recent release dates
tmp = httr::content(httr::GET("http://crandb.r-pkg.org/-/latest")) %>% 
  lapply(`[[`,"date")
releaseDf = data_frame(package=names(tmp),releaseDate = as.character(as.Date(unlist(tmp,use.names=F))))

#get download history for all packages
res = lapply(1:26,function(i){
  cat("PROCESSING: Packages that begin with '",LETTERS[i],"'\n",sep="")
  tmp = releaseDf %>% 
    filter(grepl(paste0("^[",letters[i],LETTERS[i],"]"),package))
  pkgs = tmp %>% 
    .[["package"]] %>% 
    paste(collapse=",")
  date = tmp %>% 
    .[["releaseDate"]] %>% 
    min()
  httr::content(httr::GET(sprintf("http://cranlogs.r-pkg.org/downloads/daily/%s:%s/%s",date,Sys.Date()-1,pkgs)))
})

#convert the history to first, last download dates and total counts
sumDownloadDf = lapply(res,function(a){
  lapply(a,function(b){
    tryCatch({
      package = b$package
      releaseDate = releaseDf$releaseDate[releaseDf$package == package]
      dates = lapply(b[[1]],`[[`,"day") %>% unlist()
      counts = lapply(b[[1]],`[[`,"downloads") %>% unlist()
      firstDownload = dates[which(dates >= releaseDate)[1]]
      lastDownload = b[[1]][[length(b[[1]])]]$day
      totalDownloads = sum(counts[which(dates >= releaseDate)])
      data_frame(package = package, firstDownload = firstDownload, lastDownload = lastDownload, totalDownloads = totalDownloads)
    },error=function(x){data_frame(package = NA, firstDownload = NA, lastDownload = NA, totalDownloads = NA)})
  }) %>% do.call(bind_rows,.)
}) %>% 
  do.call(bind_rows,.) %>% 
  filter(!is.na(package))

#write results to csv
write.csv(sumDownloadDf, "packageDownloadData.csv", row.names = FALSE)
cat("FINISHED:",as.character(Sys.time()),"\n")
