library(rvest)
library(future.apply)
library(stringi)

# set to TRUE if more output desired
debug <- FALSE
# set offline to TRUE if offline processing desired - essentially re-evaluate already downloaded package list
offline <- FALSE
# set parallel_processing to TRUE if parallelization desired
parallel_processing <- TRUE
num_workers <- 2 # recommend keep this low when downloading to avoid creating a denial of service style attack on chosen R mirror
#num_workers <- availableCores() # this option recommended when performing offline processing

# global ggplot2 theme
theme_set(theme_minimal())

# Make sure to update this based on where you have checked out the git repository to
path_base <- "~/SoftwareEngineeringPrinciples/"
image_base <- paste0(path_base, 'output/')

# Setup directory to save downloaded files to
download_dir <- paste0(path_base, 'downloads/')
if(! file.exists(download_dir)) {
  dir.create(download_dir)
}

# canonical source CRAN URL
# url_base <- "https://cran.r-project.org/src/contrib/"
# CRAN mirror
url_base <- "https://cloud.r-project.org/src/contrib/"
package_list_url <- "https://cran.r-project.org/src/contrib/"
package_list_file <- paste0(download_dir, 'package_list.html')

# set offline to TRUE if offline processing desired
if (! offline) {
  download.file(package_list_url, package_list_file)
}
# should probably check to see if file exists
html_source <- read_html(package_list_file)

# default plan for future.apply/future is sequential (no parallelization)
# plan(multiprocess) should pick recommended option based on OS
if (parallel_processing) {
  plan(multiprocess, workers = num_workers)
} else {
  # set back to defalt in case flag changed
  plan(sequential)
}
# on some machines download.file() default method doesn't work with parallelization
# download method may need to be modified based on OS and installed libraries/packages
download_method = 'curl'

initialize <- function(search_pattern, sample_size = '100%', untar_files = FALSE) {
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
    td[seq(4, n, 5)],
    # error flag
    FALSE
    , stringsAsFactors=FALSE)
  names(package_df) <- c('filename', 'package', 'date', 'size', 'download_error')
  
  #only want to keep R packages, not other stuff available
  package_df <- package_df[grep(".tar.gz", package_df$filename),]
  
  # new packages may have old version still in list, want to only consider newest versions
  package_df <- package_df[ order(package_df$date, decreasing = TRUE), ]
  package_df <- dplyr::distinct(package_df, package, .keep_all = TRUE)
  package_df <- package_df[ order(package_df$package), ]
  
  # this line for 100% sample
  if(sample_size == '100%') {
    s_size <- nrow(package_df)
  } else {
    s_size <- sample_size
  }
  
  
  # example test to make sure tryCatch works - occasionally there are packages that can't be downloaded
  #package_df[1, ]$filename <- 'test_for_non_existant_file.tar.gz'
  #s_size <- 10
  #search_pattern <- "src[^/]*/.+"
  
  files_and_dirs <- future_lapply(sample(package_df$filename, s_size), function(s) {
    ret <- c(s) # initalize return value so every item has at least the name of the file
    pathed_s <- paste0(download_dir, s)
    if(file.exists(pathed_s) || offline) {
      if(debug) {
        if(file.exists(pathed_s)) {
          print(paste0('File ', s, ' already downloaded'))
        } else if (offline) {
          print(paste0('File ', s, ' NOT already downloaded, but offline mode enabled.'))
        }
      }
    } else {
      print(paste0('Attempting to download: ', s))
      tryCatch({
        download.file(paste0(url_base, s), pathed_s, method = download_method, quiet = TRUE)
      },
      error=function(e) {
        print(paste(e))
        NULL
      }
      )
    }
    # prevent error if file didn't download
    if(file.exists(pathed_s)) {
      if(untar_files) {
        untar(pathed_s)
      }
      tar_list <- untar(pathed_s, list=TRUE)
      if (length(tar_list) == 0) {
        cat(paste0('File ', s, ' appears to be corrupted. Recommend deleting "', pathed_s, '" and re-running this process.\n'))
      }
      c(ret, tar_list)
    }
    else {
      cat(paste0('File ', s, ' not found at "', pathed_s ,'", cannot untar. Recommend setting offline to FALSE and re-running this process.\n'))
      ret
    }
  })
  
  # update gloabl to flag those with download errors
  error_list <- Filter(Negate(is.null), lapply(files_and_dirs, function(x) if (length(x) < 2) x ))
  if (length(error_list)) {
    package_df[package_df$filename %in% error_list, ]['download_error'] <- TRUE
  }
  
  # filter based on search_pattern
  files_and_dirs <- unlist(files_and_dirs)
  paths_found <- unlist(lapply(files_and_dirs, grep, pattern=search_pattern, value=TRUE))
  
  # get name of package from base of path
  paths_found <- unique(unlist(lapply(paths_found, stri_extract_all_regex, pattern="^\\w+[^/]+")))
  package_df$found <- FALSE
  if (length(paths_found) != 0) {
    package_df[package_df$package %in% paths_found, ]$found <- TRUE
  }
  
  return(package_df)
}

calc_dependencies <- function(package_df) {
  local({r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com/"
  options(repos=r)
  })
  ap_filename <- paste0(download_dir, 'available_packages.rds')
  
  if (offline) {
    if (file.exists(ap_filename)) {
      available_packages <- readRDS(ap_filename)
    }
    else {
      print(paste0('File ', ap_filename, ' NOT already downloaded, but offline mode enabled.'))
    }
  }
  else {
    available_packages <- available.packages()
    saveRDS(available_packages, ap_filename)
  }
  
  package_deps <- tools:::package_dependencies(packages = look_for_packages,
                                               db = available_packages,
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
      if (nrow(package_df[package_df$package == p, ]) > 0) {
        package_df[package_df$package == p, ][pd] <<- TRUE
      }
      else {
        print(paste0('Package ', p, 'in dependencies list, but not found in list of current packages.'))
        
      }
    }, package_deps[[pd]])
  }, names(package_deps))
  
  t <- transform_df(package_df)
  package_df$package_deps <- merge(package_df, t %>% group_by(package) %>% arrange(package_deps) %>%
                                     summarise(package_deps_combined = paste(package_deps, collapse =","))
                                   , by="package", all=TRUE)$package_deps_combined
  package_df[is.na(package_df$package_deps), ]['package_deps'] <- 'none'
  return(package_df)
}

transform_df <- function(package_df) {
  # can't figure out a better way to combine all values into one column
  t <- tidyr::gather(package_df, key = "package_deps", value = "deps_value", look_for_packages,
                     na.rm = FALSE,
                     convert = FALSE,
                     factor_key = FALSE)
  t <- t[t$deps_value == TRUE, ]
  t <- t[ , !(names(t) %in% 'deps_value')]
  return(t)
}

grep_table_freqs <- function(package_df) {
  # histogram packages that are tested by most current year package released
  grep_freqs <- aggregate(strftime(package_df$date, "%Y"),
                          by=list(strftime(package_df$date, "%Y"), package_df$found),
                          FUN=length)
  names(grep_freqs) <- c('Year', 'Found', 'Count')
  
  return(grep_freqs)
}


grep_table_totals <- function(grep_freqs) {
  # Table showing tested as pct of all packages
  grep_totals <- aggregate(x=grep_freqs$Count, by = list(grep_freqs$Year), FUN = sum)
  names(grep_totals) <- c('Year', 'Count')
  grep_totals <- merge(grep_totals, grep_freqs[grep_freqs$Found == TRUE, ][c('Year', 'Count')], by="Year", all=TRUE)
  names(grep_totals) <- c('Year', 'Total', 'Grep')
  
  grep_totals$Pct <- grep_totals$Grep / grep_totals$Total
  grep_totals$Pct <- round(grep_totals$Pct * 100)
  
  print('Summary Table for Grep Results')
  print(t(grep_totals))
  
  return(grep_totals)
}

grep_viz_freq <- function(grep_freqs, image_prefix, label) {
  grep_freqs$found_y_n <- c('No', 'Yes')[grep_freqs$Found + 1]
  
  # histogram of packages that are tested by most current year package released
  grep_plot <- ggplot(data=grep_freqs[grep_freqs$Year > 2007, ], aes(x=Year, y=Count, fill=found_y_n)) +
    geom_bar(stat="identity") +
    xlab("Year Package Last Updated") +
    scale_fill_viridis_d(direction = -1, option="inferno", end = 0.96) +
    labs(fill = paste(label, "Directory"))
  grep_plot
  ggsave(filename = paste0(image_base, image_prefix, '_file_analysis_stacked_bar.png'), grep_plot,
         width = 7.2, height = 5.5, dpi = 600, units = "in", device='png')
  
  return(grep_plot)
}

grep_viz_pct <- function(grep_totals, image_prefix, label) {
  grep_pct_plot <- ggplot(data=grep_totals[grep_totals$Year > 2007, ], aes(x=Year, y=Pct)) +
    geom_bar(stat="identity") +
    ylab(paste("Percent with", label, "Code")) +
    xlab("Year Package Last Updated") +
    scale_y_continuous(limits = c(0,100))
  ggsave(filename = paste0(image_base, image_prefix, "_pct_w_matching_code.png"), grep_pct_plot,
         width = 7.2, height = 4, dpi = 600, units = "in", device='png')
  
  return(grep_pct_plot)
}


dependency_table <- function(dep_freqs) {
  dep_totals <- aggregate(x=dep_freqs$Count, by = list(dep_freqs$Year), FUN = sum)
  names(dep_totals)[1] <- 'Year'
  invisible(Map(function(p) {
    dep_totals <<- merge(dep_totals, dep_freqs[dep_freqs$Dependency == p, ][c('Year', 'Count')], by="Year", all=TRUE)
    names(dep_totals)[length(names(dep_totals))] <<- p
    # not sure why, but dep_totals$p doesn't work except for with a few rows, but dep_totals[p] works for all rows
    dep_totals[paste0(p, '_Pct')] <<- round((dep_totals[p] / dep_totals$x) * 100)
  }, look_for_packages))
  dep_totals[is.na(dep_totals)] <- 0
  names(dep_totals)[2] <- 'Count'
  
  # chart to show dependency count and percent of totals, suppress low nubers
  out <- dep_totals[sapply(dep_totals, function(x) any(x >= 1))]
  return(t(out))
}

show_multiple_dependencies <- function(package_df, look_for_packages) {
  for (n in 0:(length(look_for_packages) + 1)) {
    cat(paste('Packages with', n, 'dependencies:', nrow(package_df[rowSums(package_df[look_for_packages]) == n, ]), '\n'))
    if (n > 1) {
      if(length(package_df[rowSums(package_df[look_for_packages]) == n, 'package_deps']) > 0) {
        cat(paste0('Multiple Dependency Table (dep=', n, '):'))
        # only way I can figure to not print variable name is to have full expression in-line
        print(table(package_df[rowSums(package_df[look_for_packages]) == n, 'package_deps']))
      }
    }
  }
}