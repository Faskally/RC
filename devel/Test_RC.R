

# set proxy values
httr::set_config(httr::use_proxy(url="192.168.41.8", port=80))

# load packages
devtools::install_github("faskally/gmrf")
library(gmrf)
pkg <- devtools::as.package("C:/work/repos/Faskally/rc")
devtools::load_all(pkg)

example(smooth.construct.gmrf.smooth.spec)

# -----------------------------------------------
#
# lets start 
#
# -----------------------------------------------
