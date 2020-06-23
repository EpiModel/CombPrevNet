

# Core Package Stack for Project ------------------------------------------

# Base EpiModel
install.packages("EpiModel")

# Extra Helper Packages
install.packages(c("remotes", "sessioninfo"))

# Latest Dev Versions of Statnet Packages
remotes::install_github(c("statnet/network@deff2a0",
                          "statnet/networkDynamic@14182bf",
                          "statnet/statnet.common@3307a8c",
                          "statnet/ergm@8b30e92",
                          "statnet/tergm@d3af135"),
                        upgrade = FALSE)

# Latest Dev Versions of EpiModel Packages
remotes::install_github(c("statnet/EpiModel@2c131f0",
                          "statnet/EpiModelHPC@a64dbf2",
                          "statnet/tergmLite@v1.2.0",
                          "EpiModel/EpiABC@c32ecb6",
                          "EpiModel/ARTnetData@1d8ec6e",
                          "EpiModel/ARTnet@150c631"),
                        upgrade = FALSE)

# Current Version of EpiModelHIV for Project
remotes::install_github("EpiModel/EpiModelHIV-p@CombPrev-AIDSR1",
                        upgrade = FALSE)


# Package Listing ---------------------------------------------------------

library("EpiModelHIV")
options(width = 100)
sessioninfo::package_info(pkgs = c("network", "networkDynamic", "statnet.common",
                                   "ergm", "tergm", "EpiModel", "EpiModelHPC",
                                   "tergmLite", "EpiABC", "EpiModelHIV",
                                   "ARTnetData", "ARTnet"), dependencies = FALSE)

# June 19 2020
# package        * version     date       lib source
# ARTnet           1.0.0       2020-06-19 [1] Github (EpiModel/ARTnet@150c631)
# ARTnetData       1.0         2020-06-19 [1] Github (EpiModel/ARTnetData@1d8ec6e)
# EpiABC           1.0         2019-05-16 [1] Github (EpiModel/EpiABC@c32ecb6)
# EpiModel       * 1.7.3       2020-06-19 [1] Github (statnet/EpiModel@2c131f0)
# EpiModelHIV    * 1.5.0       2020-06-19 [1] Github (EpiModel/EpiModelHIV-p@ffa3665)
# EpiModelHPC    * 2.0.2       2020-06-19 [1] Github (statnet/EpiModelHPC@a64dbf2)
# ergm           * 3.10.0-4851 2020-06-19 [1] Github (statnet/ergm@8b30e92)
# network        * 1.14-377    2020-06-19 [1] Github (statnet/network@deff2a0)
# networkDynamic * 0.10        2020-06-19 [1] Github (statnet/networkDynamic@14182bf)
# statnet.common   4.3.0-230   2020-01-14 [1] Github (statnet/statnet.common@3307a8c)
# tergm          * 3.6.0-1659  2020-06-19 [1] Github (statnet/tergm@d3af135)
# tergmLite      * 1.2.0       2020-06-19 [1] Github (statnet/tergmLite@73d2a2d)
