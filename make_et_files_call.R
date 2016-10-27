#====================================================================
#
# Make et file example call
# Written by Andrew Sommerlot <asommerlot@chesapeakebay.net>
# October 27 2016
#
#====================================================================

#********************************************************************
#********************************************************************

#====================================================================
#
# Requires Evapotranspiration and tidyr
# method refers to the type of Ra calculation used, referced from
#  et_hargraves_cpb "type" option. Can be one of the following:
#     "empirical" -> default, uses purely empirical estiamte of Ra
#     "measured" -> replaces empirical estimate with converted Rs input
#     "meas_cor" -> same as measured with mean ratio Ra/Rs correction
#     "bhatt" -> Gopal's custom correction, includes bs = 0.45
#
#====================================================================

library(Evapotranspiration)
library(tidyr)

# set this to location of the "et_cpb_hargraves_functions.R" file
source('~/Desktop/evapotranspiration/et_cpb_hargraves_functions.R')

# Setup for one landseg
land_seg = 'N24031'
elevation = 130
latitude = 37.12
read_loc = '~/Desktop/evapotranspiration/et_data/base' # set to proper location
save_loc = '~/Desktop/evapotranspiration/output' # set to proper location
start_date = '1985-01-01'
end_date = '2011-12-31'
method = 'bhatt'

# write the et file according to CPB format
output = make_hargraves_file(land_seg = land_seg,
  elevation = elevation,
  latitude = latitude,
  read_loc = read_loc,
  save_loc = save_loc,
  start_date = start_date,
  end_date = end_date,
  method = method
  )