# SoftwareEngineeringPrinciples

Software Engineering Principles applied to computational research - paper and associated code

Analysis code is contained in 2 R Markdown files, with shared functions stored in shared_fn.R. In order to run the analysis, all R packages on CRAN need to be downloaded. While it is possible to use services like "R Package Documentation" at https://rdrr.io, I found that downloading the packages locally is more reliable. Depending on internet speed, this can take many hours. Total size of all downloaded packages is currently about 6.6GB. If manual inspection is desired, a flag can be set so the process automatically un-tar and un-gzips the files, which takes about 20+ GB.

R Markdown documents and [sourcecode/shared_fn.R](shared_fn.R) do have some hard coded paths that assume you check out this Github repository to your home directory. If you've checked it out somewhere else, update the paths before running. See:

* [cran_optimization_survey.Rmd](sourcecode/cran_optimization_survey.Rmd) line 58
* [cran_testing_survey.Rmd](sourcecode/cran_testing_survey.Rmd) line line 43
* [shared_fn.R](sourcecode/shared_fn.R) line 8

Once all files are downloaded, analysis process runs in just a minute or two on my test machine. YMMV as performance is highly disk dependent.

In order to improve performance on machines with high-latency internet connections, there is a parallel_processing flag (defaults to TRUE) that significantly improves performance. Parallel process does also improve performance on machines with low-latency connections, though not as much. Depending on other system configuration (disk speed, # of cpus) your mileage may vary.

## Info on results of running code

As the process does require downloading over 13,000 files, I have saved the results of running in the form plain text output and images - for that, see the [output/](output/) directory. For the results of running the RMarkdown Notebooks, see the [sourcecode/](sourcecode/) directory.
