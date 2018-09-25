# How to profile your R code that includes C++ code

While there are many options for profiling C++ (or other compiled code) on Linux, many of those tools are difficult to get working correctly with R. Additionally, profiling R code with C++ is complicated due to different settings between various commonly use OSes. While there are some blog posts and presentation materials available on the Internet, many miss important steps that take some effort to determine the correct solution to resolve.

## Steps to profile C++ code being called by R code on macOS using Xcode’s Instruments

macOS users can use Xcode (freely available) for profiling of R code that calls C++ code. As Xcode has a nice GUI, it may be the preferred tool for many users. The primary profiling tool in Xcode is called [Instruments](https://help.apple.com/instruments/mac/10.0/#/dev7b09c84f5).

Here are some basic steps to get it working:

1. Configure compilation to enable debugging. In GCC and CLANG this can be accomplished by adding ‘-g’ to your compiler flags via Makevars. Depending on your other compiler settings you may also want to add -O0, though changing optimization levels may alter any gains achieved through profiling.
1. Determine what you want to profile. *The code you run needs to last sufficiently long to allow for application switching and to gather sufficient data via profiling.*
1. Create a script to run the code you are interested in. Here is an example of running PCCC with an input dataset of 100000 rows:
    ```r
    library(pccc)
    icd10_large <- feather::read_feather("../icd_file_generator/icd10_sample_large.feather")
    icd10_large <- icd10_large[1:100000, c(1:45)]

    ccc(icd10_large[1:100000, c(1:45)], # get id, dx, and pc columns
        id      = id,
        dx_cols = dplyr::starts_with("dx"),
        pc_cols = dplyr::starts_with("pc"),
        icdv    = 10)
    ```
1. Prep Instruments by selecting ‘Time Profiler’ and then identifying the necessary process. If running script via RStudio, you will need to observe the rsession process. If running via command line R, the process is R. Alternatively run Instruments with “Allocations” to see memory allocations
1. Start profiling.
1. Start your script in what ever fashion you normally run R scripts.
1. After you have gathered sufficient data (perhaps 30 seconds to 1 minute), stop the profiling process.
1. Find your custom function calls in the symbol tree. There will likely be many layers before your code is called. The call just before your code will be “do_dotcall” The symbol tree should show your custom function names and how long each took.

Xcode Instruments screenshot showing C++ code from [PCCC](https://cran.r-project.org/package=pccc) R package
![Xcode Instruments screenshot showing C++ code from PCCC R package](https://github.com/magic-lantern/SoftwareEngineeringPrinciples/raw/master/paper/instruments_screenshot.png "Xcode Instruments screenshot showing C++ code from PCCC R package")

For users on Windows, the GPL licensed [Very Sleepy](http://www.codersnotes.com/sleepy/) is an excellent GUI profilier that works almost identically to Xcode Instruments for CPU Profiling.

## Steps to profile C++ code via command line on macOS and Linux using gperftools

To profile R code that calls C++ code via command line tools requires calling the correct R binary as well as setting up correct environment variables in a OS specific fashion. 

1. Follow steps 1 – 3 as shown above.
1. Identify location of R binary. By default ‘R’ is actually a batch shell script that launches the R binary. You cannot get the desired profiling informaiton about your code from profiling a shell script.
    1. For macOS, using R installed via Homebrew, the actual R binary is located at `/usr/local/Cellar/r/3.5.1/lib/R/bin/exec/R`
    1. For Linux users, the R binary is likely located at `/usr/lib/R/bin/exec/R`

1. Set (or verify) your R_HOME environment variable.
    1. For macOS, using R installed via Homebrew, the R_HOME is `/usr/local/Cellar/r/3.5.1/lib/R`
    1. For Linux users, the R binary is likely located at `/usr/lib/R/bin/exec/R`

    If your environment variable is not set, set R_HOME via a command like (replace path with your actual R Home location). If you use bash, the command is:

    ```bash
    export R_HOME=/usr/lib/R/bin/exec/R
    ```

1. Run your test (*in this example it is called profile_sample.R*) script with the perftools libraries loaded and an environment variable CPUPROFILE that specifies the location to save the CPU profile output. Replace libprofiler path and file name with your actual filename. Replace R binary with your actual R binary with full path.
    1. For macOS, this is accomplished via the command `DYLD_INSERT_LIBRARIES=/usr/local/lib/libprofiler.dylib CPUPROFILE=sample.profile /usr/local/Cellar/r/3.5.1/lib/R/bin/exec/R -f profile_sample.R`

    1. For Linux, this is accomplished via the command `LD_PRELOAD=/usr/lib/libprofiler.so CPUPROFILE=sample.profile /usr/lib/R/bin/exec/R -f profile_sample.R`

1. View the results via pprof; again, ensure you use your actual R binary path.
    1. For macOS, this is accomplished via the command `pprof --text /usr/local/Cellar/r/3.5.1/lib/R/bin/exec/R sample.profile`
    1. For Linux, this is accomplished via (Debian based distributions may call pprof ‘google-pprof’) the command `pprof --text /usr/bin/R ./sample.profile`

    Output will be something similar to the following:
    ```
    Using local file /usr/bin/R.
    Using local file ./sample.profile.
    Total: 7399 samples
        2252  30.4%  30.4%     2252  30.4% __nss_passwd_lookup
        2172  29.4%  59.8%     4389  59.3% std::__cxx11::basic_string::compare
        982  13.3%  73.1%     5594  75.6% codes::find_match
        591   8.0%  81.1%      621   8.4% Rf_mkCharLenCE
        462   6.2%  87.3%      482   6.5% R_BadLongVector
        223   3.0%  90.3%      223   3.0% std::vector::operator[] (inline)
        151   2.0%  92.4%      151   2.0% std::__once_callable
            98   1.3%  93.7%       98   1.3% SET_STRING_ELT
            83   1.1%  94.8%       83   1.1% _init@6750
            30   0.4%  95.2%      452   6.1% Rf_allocVector3
    ```
