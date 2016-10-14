### f hamon hargraves function call 

# if needed. 
#install.packages('Evapotranspiration')
#install.pacakges('tidyr')

path_to_function = '~/Desktop/evapotranspiration/et_hamon_hargraves_correction.R'
source(path_to_function)

# set up the paths accordingly before running. 

et_hamon_hargraves_correction(
  land_seg = 'N24031',
  elevation = 107,
  latitude = 37.139, 
  read_loc_base = '~/Desktop/evapotranspiration/et_data/base',
  read_loc_s25 = '~/Desktop/evapotranspiration/et_data/s25',
  read_loc_aux = '~/Desktop/evapotranspiration/et_data/aux',
  save_loc = '~/Desktop/evapotranspiration/et_data/out'
 )