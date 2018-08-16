rm(list=ls())
gc()

library(dplyr)
library(rvest)
library(stringi)
library(ggplot2)

look_for_packages <- c('Rcpp', 'tictoc', 'rbenchmark', 'microbenchmark', 'profr', 'profvis', 'snow', 'doSNOW', 'parallel', 'doParallel', 'Rmpi', 'foreach', 'future', 'SparkR', 'DistributedR', 'ddR', 'sparklyr', 'batchtools', 'RcppParallel', 'threadpool', 'parallelDist', 'parallelMap', 'doMC', 'doMPI', 'partools', 'DSL')


# canonical source CRAN URL
# url_base <- "https://cran.r-project.org/src/contrib/"
# CRAN mirror
url_base <- "https://cloud.r-project.org/src/contrib/"
html_source <- read_html("https://cran.r-project.org/src/contrib/")

td <- html_source %>% html_nodes("td") %>% html_text(trim = TRUE)

n <- length(td)
package_df <- data.frame(
  #name
  td[seq(2, n, 5)],
  #package
  stri_replace_last(td[seq(2, n, 5)], '' , regex = '_[0-9.-]+\\.tar\\.gz'),
  # date
  td[seq(3, n, 5)],
  # size
  td[seq(4, n, 5)]
  , stringsAsFactors=FALSE)
names(package_df) <- c('filename', 'package', 'date', 'size')

#only want to keep R packages, not other stuff available
package_df <- package_df[grep(".tar.gz", package_df$filename),]

# new packages may have old version still in list, want to only consider newest versions
package_df <- package_df[ order(package_df$date, decreasing = TRUE), ]
package_df <- distinct(package_df, package, .keep_all = TRUE)
package_df <- package_df[ order(package_df$package), ]


local({r <- getOption("repos")
r["CRAN"] <- "https://cran.rstudio.com/"
options(repos=r)
})
package_deps <- tools:::package_dependencies(packages = look_for_packages,
                                             recursive=FALSE,
                                             reverse=TRUE,
                                             which = c("Depends","Imports","LinkingTo", "Suggests"))
#   - count how many use each
#   - count total number of packages using a testing package
package_df$package_deps <- 'none'
print(sort(unlist(Map(length, package_deps)), decreasing = TRUE))
result <- Map(function(pd) {
  package_df[pd] <<- FALSE
  Map(function(p) {
    package_df[package_df$package == p, ][pd] <<- TRUE
  }, package_deps[[pd]])
}, names(package_deps))

t <- tidyr::gather(package_df, key = "package_deps", value = "deps_value", look_for_packages,
       na.rm = FALSE,
       convert = FALSE,
       factor_key = FALSE)
t <- t[t$deps_value == TRUE, ]

# are there any that have more than one dependency listed?
# these are all counted multiple times in histogram
package_df[rowSums(package_df[look_for_packages]) > 1, ]
nrow(package_df[rowSums(package_df[look_for_packages]) > 1, ])

dep_freqs <- aggregate(strftime(t$date, "%Y"),
                       by=list(strftime(t$date, "%Y"), t$package_deps),
                       FUN=length)

names(dep_freqs) <- c('Year', 'Dependency', 'Count')
fplot <- ggplot(data=dep_freqs[dep_freqs$Count > 9, ], aes(x=Year, y=Count, fill=Dependency)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(type='div', palette = 'Set1') + # http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
  xlab("Year Package Last Updated") +
  labs(fill = "Dependency n > 9")
fplot
ggsave(filename = "../sourcecode/dependency_stacked_bar.png", fplot,
       width = 7.2, height = 5.5, dpi = 600, units = "in", device='png')


dep_totals <- aggregate(x=dep_freqs$Count, by = list(dep_freqs$Year), FUN = sum)
names(dep_totals)[1] <- 'Year'
Map(function(p) {
  dep_totals <<- merge(dep_totals, dep_freqs[dep_freqs$Dependency == p, ][c('Year', 'Count')], by="Year", all=TRUE)
  names(dep_totals)[length(names(dep_totals))] <<- p
  # not sure why, but dep_totals$p doesn't work except for with one row (profr), but dep_totals[p] works for all rows
  dep_totals[paste0(p, '_Pct')] <<- (dep_totals[p] / dep_totals$x)
}, look_for_packages)
dep_totals[is.na(dep_totals)] <- 0
names(dep_totals)[2] <- 'Count'
dep_totals

dep_totals[dep_totals[, grep("_Pct$", colnames(dep_totals))] >= 0.01, ]

dep_totals[dep_totals$Year > 2012, ]
dep_totals[dep_totals[grep("_Pct$", colnames(dep_totals))] > 0.05, ]
apply(dep_totals, MARGIN = 2, function(x) {
  any(x >= 0.05)
})



# chart to show dependency count and percent of totals
dep_totals[sapply(dep_totals, function(x) any(x >= 0.01))][dep_totals$Year > 2012, ]


# now look at files present and source code
untar_files <- TRUE
orig_wd <- getwd()
download_dir <- '/Users/seth/OneDrive - The University of Colorado Denver/Documents/Software Engineering Paper/survey/downloads'
if(! file.exists(download_dir)) {
  dir.create(download_dir)
}
setwd(download_dir)

files_and_dirs <- lapply(sample(package_df$filename, 50), function(s) {
  if(file.exists(s)) {
    print(paste0('File ', s, 'already downloaded'))
  } else {
    print(paste0('Attempting to download: ', s))
    tryCatch(download.file(paste0(url_base, s), s),
             error=function(e) NULL)
  }
  if(untar_files) {
    untar(s)
  }
  untar(s, list=TRUE)
})

###
### need to determine what to look for. src directory? .c/.cpp files? both? FORTRAN files?
### test this on the Linux box which has all files already downloaded...
###


