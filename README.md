When opening the project renv should automaticaly check for all necessary packages.
After that you can run renv::restore() to install all packages that you need.
If something won't be ok renv should guide you what to do.

Run library(targets) to load targets. Then you can use various commands:
tar_make() to run the _targets.R script
tar_make_clustermg(workers = N), where N is the number of CPU cores for multicore processing, N = 2 is enough and it will speed up the pipeline
tar_load(name), where name is the name of the object, created with _targets, that you want to load 

If you load the diagnostic plot you have to run plot(diag_plot) to view it.

