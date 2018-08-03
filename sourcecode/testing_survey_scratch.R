# TO DO
# ?? maybe use map instead of lapply ??
# probably want vector of logicals, but need to remove nested lists
# need to evaluate all the packages that don't use the above test directory
# do they use builtin R functions for testing? Or, some older testing packages?
# convert to Notebook
# generate results for paper
# next - look at optimization
#   percent of packages using C, Rcpp, C++, Fortran/Cobol?

rm(list=ls())
gc()

library(stringi)
library(magrittr)
library(stringr)

read_from_cran <- TRUE
setwd("/Users/seth/onedrive/Documents/Software Engineering Paper/survey/downloads")
sample_size <- 1000 # may want to do percent of all packages, or alternatively 100% sample

# Testing packages
test_packages <- c("svUnit", "RUnit", "testthat", "testit", "unitizer", "unittest")
# testit is a minimal tool for testing packages, but it can be used for unit testing - e.g. tinytex
# Packages considered but excluded:
#   webmockr - Could be used for a testing library - or perhaps a roll your own test suite, but isn't as far as I could tell
#   vdiffr - Is a extension for testthat, so requires testthat to work
#
# Users could use core R functionality for unit tests such as stopifnot() instead of packages
# we don't attempt to quantify the scope of software that does unit testing without relying on
# a package


if (read_from_cran) {
  library(rvest)

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

  # this line for 100% sample
  files_and_dirs <- lapply(sample(package_df$filename, nrow(package_df)), function(s) {
  #files_and_dirs <- lapply(sample(package_df$filename, sample_size), function(s) {
    # line 61 need to check and see if already downloaded
    if(file.exists(s)) {
      print(paste0('File ', s, 'already downloaded'))
    } else {
      print(paste0('Attempting to download: ', s))
      tryCatch(download.file(paste0(url_base, s), s),
               error=function(e) NULL)
    }
    untar(s)
    untar(s, list=TRUE)
  })
} else {
  packages <- list.files(getwd(), pattern=".tar.gz")
  files_and_dirs <- lapply(sample(packages, sample_size), function(s) {
    print(s)
    # dont need to untar, but is useful for manual analysis
    untar(s)
    #paths_with_test <- lapply(untar(s, list=TRUE), grepl, pattern="[Tt]est[^/]*/.+", value=TRUE)
    v <- any(grepl(pattern="[Tt]est[^/]*/.+", untar(s, list=TRUE)))
    names(v) <- s
    v
  })
}
files_and_dirs <- unlist(files_and_dirs)

# example of grep stmts to make sure it is working as expected
any(grepl("[Tt]est[^/]*/.+", c('blah', 'blah2')))
any(grepl("[Tt]est[^/]*/.+", c('unitest/myfile.R', 'something')))
any(grepl("[Tt]est[^/]*/.+", c('unitest/f.R', 'tests/b.R')))

# now, for each dirs, see if there is a directory or file named test
# find paths that have test in them
# Although most unit testing packages use test/ or tests/ as the common base directory, some
# such as svUnit recommend the directory /inst/unitTests
#
# code at https://github.com/rorynolan/exampletestr/blob/master/analysis/CRAN_test_analysis.Rmd misses some
# test library usage since it checks for "^tests/.+". As an example, R packages using svUnit (such as gsubfn)
# often call the test directory unitTests
#
paths_with_test <- unlist(lapply(files_and_dirs, grep, pattern="[Tt]est[^/]*/.+", value=TRUE))
# get name of package from base of path
packages_with_tests <- unique(unlist(lapply(paths_with_test, stri_extract_all_regex, pattern="^\\w+[^/]+")))

package_df$uses_tf <- FALSE
package_df[package_df$package %in% packages_with_tests, ]$uses_tf <- TRUE


# histogram packages that are tested by most current year package released
freqs <- aggregate(strftime(package_df$date, "%Y"),
                   by=list(strftime(package_df$date, "%Y"), package_df$uses_tf),
                   FUN=length)
names(freqs) <- c('Year', 'Uses_TF', 'Count')
ggplot(data=freqs, aes(x=Year, y=Count, fill=Uses_TF)) +
  geom_bar(stat="identity")


#
# Now calculate based on stated dependencies
#
#
#
r <- readRDS(url('https://cran.r-project.org/web/packages/packages.rds'))
r[match(c('pccc', 'Rcpp'), r[, 'Package']), 'Published']
length(r[, 'Package'])

r[r[,'Package'] == 'pccc', 'Published']
dplyr::filter(r, Package == 'pccc')



# Calculate which testing packages are most popular
# reverse dependencies don't give the whole story - e.g. Wats depends on testit, but as shown
# in the directory structure, uses testthat for unit testing
#
# since package source not provided, using cran.r-project.org by default
test_package_dependencies <- tools:::package_dependencies(packages = test_packages,
                                                          recursive=FALSE,
                                                          reverse=TRUE,
                                                          which = c("Depends","Imports","LinkingTo", "Suggests"))
# Need check for use of various testing frameworks
#   - count how many use each
#   - count total number of packages using a testing package
package_df$framework <- 'none'
print(sort(unlist(Map(length, test_package_dependencies)), decreasing = TRUE))
result <- Map(function(tpd) {
  package_df[tpd] <<- FALSE
  Map(function(p) {
    package_df[package_df$package == p, ][tpd] <<- TRUE
    package_df[package_df$package == p, ]$framework <<- tpd
  }, test_package_dependencies[[tpd]])
}, names(test_package_dependencies))

# are there any that have more than one testing framework listed?
# these are all counted multiple times in histogram
package_df[rowSums(package_df[test_packages]) > 1, ]
nrow(package_df[rowSums(package_df[test_packages]) > 1, ])

# now create histogram by year with testing packages shown
freqs <- aggregate(strftime(package_df$date, "%Y"),
                   by=list(strftime(package_df$date, "%Y"), package_df$framework),
                   FUN=length)
names(freqs) <- c('Year', 'Framework_Used', 'Count')
ggplot(data=freqs, aes(x=Year, y=Count, fill=Framework_Used)) +
  geom_bar(stat="identity")







stri_replace_last('ECOSolveR_0.4.tar.gz', '', regex = '_[0-9.-]+\\.tar\\.gz')

for (i in files_and_dirs) {
  v <- grep("test[^/]*/.+", i, value = TRUE)
  matches <- stri_extract_all_regex(grep("test[^/]*/.+", i, value = TRUE), "^\\w+[^/]")
}
unique(matches)

test <- c("utilsIPEA/tests/", "utilsIPEA/tests/testthat.R", "utilsIPEA/tests/testthat/", "utilsIPEA/tests/testthat/test_nome_de_solteira.R", "utilsIPEA/tests/testthat/test_abrevia_nome_meio.R", "utilsIPEA/tests/testthat/test_text_functions.R", "utilsIPEA/tests/testthat/test_ident_erros_munic_galileo.R")
lapply(test, stri_extract_all_regex(, "^\\w+[^/]"))

unique(stri_extract_all_regex(test, "^\\w+[^/]"))
files_and_dirs

c( "unitTests/", "tests/", "gsubfn", "likeLTD", "logging") %>% str_detect("^tests/.+")




