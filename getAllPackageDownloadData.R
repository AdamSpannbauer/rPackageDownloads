##################################
# LONG RUN TIME WARNING > 1 HOUR #
##################################

library(dplyr)
#used not loaded
# library(httr)
# library(cranlogs)

# function to get oldest release date of packages provided
getReleaseDates <- function(packages) {
  
  releaseDf <- dplyr::data_frame(package=packages, releaseDate=NA)
  
  for (i in 1:length(packages)) {
    pkgDataRaw  <- httr::GET(paste0("http://crandb.r-pkg.org/", packages[i], "/all"))
    pkgData     <- httr::content(pkgDataRaw)
    
    releaseDf$releaseDate[i] <- pkgData$timeline[[1]] %>% 
      as.Date() %>% 
      as.character()
  }
  
  releaseDf
}

# get char vector of all packages on CRAN
packages  <- names(httr::content(httr::GET("http://crandb.r-pkg.org/-/desc")))
releaseDf <- getReleaseDates(packages)

#function to handle fail download GET
safe_cran_downloads <- function(packages, from, to, silent = TRUE) {
  out <- data.frame(date=NA,count=NA, package=packages, stringsAsFactors = FALSE)
  
  try(out <- cranlogs::cran_downloads(packages, 
                                      from = from, 
                                      to   = to), silent=silent)
  
  out
}

downloadList <- lapply(packages, function(pkg) {
  #set dates to get data on packages (from relase to yesterday)
  release   <- min(releaseDf$releaseDate[which(releaseDf$package==pkg)])
  yesterday <- Sys.Date()-1
  
  safe_cran_downloads(pkg, 
                      from = release, 
                      to   = yesterday)
})

#bind results into single df
downloadDf <- bind_rows(downloadList)

#find oldest download date and most recent download date
sumDatesDf <- downloadDf %>% 
  filter(count > 0) %>% 
  group_by(package) %>% 
  summarise(lastDownload=max(date), firstDownload=min(date)) %>% 
  ungroup() %>% 
  mutate(firstDownload = as.character(firstDownload)) %>% 
  mutate(firstDownload = as.character(firstDownload)) 
  
#total downloads for package
sumDlCountDf <- downloadDf %>% 
  group_by(package) %>% 
  summarise(totalDownloads=sum(count)) %>% 
  ungroup()

#combine total downloads with download dates and 
sumDownloadDf <- left_join(sumDlCountDf, sumDatesDf, by="package") %>% 
  left_join(releaseDf, by="package") %>% 
  mutate(dateDataScraped = as.character(Sys.Date())) %>% 
  arrange(totalDownloads, desc(lastDownload))

write.csv(sumDownloadDf, "packageDownloadData.csv", row.names = FALSE)

