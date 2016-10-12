library(Evapotranspiration)
data(climatedata)
data(constants)
data(defaultconstants)

head(climatedata)

###################
#
# adding the et data from the climate scenarios ... . 
#
####################
library(tidyr)

# load the data
setwd('~/Desktop/evapotranspiration')

base_tmp = read.csv('et_data/base_n24031_tmp.csv')
s25_tmp = read.csv('et_data/2025_n24031_tmp.csv', head = FALSE)
s50_tmp = read.csv('et_data/2050_n24031_tmp.csv', head = FALSE)
base_rad = read.csv('et_data/base_n24031_rad.csv', head = FALSE)
base_wnd = read.csv('et_data/base_n24031_wnd.csv', head = FALSE)
base_dpt = read.csv('et_data/base_n24031_dpt.csv', head = FALSE)

# get min/max 
base_tmp_ag = tidyr::unite_(base_tmp, paste(colnames(base_tmp)[c(2,3,4)], collapse="-"), colnames(base_tmp)[c(2,3,4)])
base_tmp_ag[,2] = as.Date(gsub('_', '-', base_tmp_ag[,2]))
base_tmp_min = aggregate.data.frame(base_tmp_ag[,4], by = list(base_tmp_ag[,2]), FUN = 'min')
base_tmp_max = aggregate.data.frame(base_tmp_ag[,4], by = list(base_tmp_ag[,2]), FUN = 'max')
base_tt = data.frame(date = base_tmp_min[,1], tmin =  base_tmp_min[,2], tmax = base_tmp_max[,2])

# get the julian day
jday <- as.POSIXlt(base_tmp_ag[,2], format = "%d%b%y")$yday

# get daily tdew for daily RH 
base_dpt_ag = tidyr::unite_(base_dpt, paste(colnames(base_dpt)[c(1,2,3)], collapse="-"), colnames(base_dpt)[c(1,2,3)])
base_dpt_ag[,1] = as.Date(gsub('_', '-', base_dpt_ag[,1]))
base_dpt_ave = aggregate.data.frame(base_dpt_ag[,3], by = list(base_dpt_ag[,1]), FUN = 'mean')
dpt_ave = data.frame(date = base_dpt_ave[,1], dpt.day =  base_dpt_ave[,2]) 



# get sunshine hours from solar radiation data.
base_rad_ag = tidyr::unite_(base_rad, paste(colnames(base_rad)[c(1,2,3)], collapse="-"), colnames(base_rad)[c(1,2,3)])
base_rad_ag[,1] = as.Date(gsub('_', '-', base_rad_ag[,1]))

count_hours = function(x) {
  all = sum(x>0)
  return(all)
}

base_rad_hrs = aggregate.data.frame(base_rad_ag[,3], by = list(base_rad_ag[,1]), FUN = 'count_hours')
base_hrs = data.frame(date = base_rad_hrs[,1], nhrs = base_rad_hrs[,2] )

# put it all together 
df1 = base_tmp_ag
df1$Year = base_tmp[,2]
df1$Month = base_tmp[,3]
df1$Day = base_tmp[,4]
df1$Hour = base_tmp[,5]
df1$hour = NULL
df1$temperature.dC. = NULL
df1$pot.et.in. = NULL

df2 = merge(df1, base_tt, by.x = 'year-month-day', by.y = 'date', all.x = TRUE)
df3 = merge(df2, base_hrs, by.x = 'year-month-day', by.y = 'date', all.x = TRUE)
df4 = merge(df3, dpt_ave, by.x = 'year-month-day', by.y = 'date', all.x = TRUE)
df4$wnd = base_wnd[,5][1:236664]
df4$dpt = base_dpt[,5][1:236664]
df4$sdtemp = base_tmp$temperature.dC.
df4$julian = jday

# get it in final form matching the evapo library .........

rh = function(tmin, tmax, tdew) {
  e0 = function(T) {
    e0 = 0.6108*exp((17.27*T)/(T + 237.3))
    return(e0)
  }
  ea = e0(tdew)
  e01 = e0(tmin)
  e02 = e0(tmax)
  es = (e01 + e02)/2
  rh = ea/es*100
  rh[rh>100] = 100
  return(rh)
}

base_data = data.frame(Station.Numer = df3$seg,
                       Year = df4$Year,
                       Month = df4$Month,
                       Day = df4$Day,
                       Hour = df4$Hour,
                       Julian = df4$julian,
                       Temp.subdaily = df4$sdtemp,
                       Tdew.subdaily = df4$dpt, 
                       RH.subdaily = rh(df4$tmin, df4$tmax, df4$dpt.day),
                       n.daily = df3$nhrs,
                       uz.subdaily = df4$wnd,
                       Tmin.daily = df4$tmin,
                       Tmax.daily = df4$tmax
                       )




# update constants ..
data("constants")
#  for mont county md
constants$lat = 39.1287
constants$lat_rad = 0.68292
constants$as = 0.23        #(Roderick, 1999, page 181) 
constants$bs = 0.5  # I left these for now, not sure if they are good or not ....
constants$Elev = 137 # meters gaithersburg md
constants$z = 10 # meters
#skipping the constants for estimating sunshine hours from cloud cover, as sunshine hours are used as
  # input
# ones for crae
#constants$PA = left this as well ...
# skipping constant for Morton's procedure 
  
base_input <- ReadInputs(base_data, constants,
                   stopmissing=c(1,1,1),
                   timestep="subdaily"
                   #interp_missing_days = TRUE,
                   #interp_missing_entries = TRUE,
                   #interp_abnormal = TRUE,
                   #missing_method = "DoY average",
                   #abnormal_method = "DoY average"
                   )


setwd("/Users/grad/Desktop/evapotranspiration")

ab = ET.Abtew(base_data, constants, ts="daily", solar="sunshine hours")

bc = ET.BlaneyCriddle(base_data, constants, ts="daily", solar="sunshine hours", height = F)

bs = ET.BrutsaertStrickler(base_data, constants, ts="daily", solar="sunshine hours", alpha=0.23)

ca = ET.ChapmanAustralian(data, constants, ts="daily", PenPan= T, solar="sunshine hours", alpha=0.23)

gg = ET.GrangerGray(data, constants, ts="daily", solar="sunshine hours",
               windfunction_ver=1948, alpha=0.23)


#####################################################################################
# of interest 

hn = ET.Hamon(base_input, constants = NULL, ts="daily")

hs = ET.HargreavesSamani(base_input, constants, ts="daily")

jh = ET.JensenHaise(base_input, constants, ts="daily", solar="sunshine hours")

la = ET.Linacre(base_input, constants, ts="daily")

mk = ET.Makkink(base_input, constants, ts="daily", solar="sunshine hours")

pt = ET.PriestleyTaylor(base_input, constants, ts="daily", solar="sunshine hours", alpha=0.23)

pm = ET.PenmanMonteith(base_input, constants, ts="daily", solar="sunshine hours", wind="yes", crop="short")

######################################################################################################


ms = ET.MattShuttleworth(data, constants, ts="daily", solar="sunshine hours", alpha=0.23, r_s=70, CH=0.12)

mb = ET.McGuinnessBordne(data, constants, ts="daily")

#mca = ET.MortonCRAE(data, constants, ts="daily", est="potential ET",
#                   solar="sunshine hours", Tdew= T, alpha = NULL)

#mcw = ET.MortonCRWE(data, constants, ts="monthly", est="potential ET",
#                    solar="sunshine hours", Tdew= T, alpha = NULL)

p = ET.Penman(data, constants, ts="daily", solar="sunshine hours",
               wind="yes", windfunction_ver=1948, alpha = 0.08, z0 = 0.001)

pm = ET.PenmanMonteith(data, constants, ts="daily", solar="sunshine hours", wind="yes", crop="short")

pp = ET.PenPan(data, constants, ts="daily", solar="sunshine hours",
               alpha=0.23, est="potential ET", pan_coeff=0.71, overest= F)

pt = ET.PriestleyTaylor(data, constants, ts="daily", solar="sunshine hours", alpha=0.23)

r = ET.Romanenko(data, constants = NULL, ts="daily")

sj = ET.SzilagyiJozsa(data, constants, ts="daily", solar="sunshine hours", wind="yes",
                      windfunction_ver=1948, alpha=0.23, z0=0.2)
t = ET.Turc(data, constants, ts="daily", solar="sunshine hours", humid= F)


#######################################################
#
# get the plots 
#
######################################################
dev.off()
source('et_compare_plot_functions.R')
par(mfrow = c(2,2))

one <- hn
two <- hs
three <- jh
four <- la
five <- mk
six <- pt
seven <- pm

et_multi_plot(one, two, three, four, five, six, seven, type = 'Daily') 

et_multi_plot_3(one, two, seven, type = 'Daily') 

#dev.off()
ETForcings(base_input, one, forcing = 'Tmax')
ETForcings(base_input, two, forcing = 'Tmax')
ETForcings(base_input, three, forcing = 'Tmax')
ETForcings(base_input, four, forcing = 'Tmax')
ETForcings(base_input, five, forcing = 'Tmax')
ETForcings(base_input, six, forcing = 'Tmax')
ETForcings(base_input, seven, forcing = 'Tmax')


ETComparison(jh, la, mk, ms, mb, p, pm,
             labs = c('', '', '', '', '', '', ''),
             Sdate = NULL, Edate = NULL,
             type = "Daily", ylim = c(-2,25))

ETComparison(pp, pt, r, sj, t,
             labs = c('', '', '', '', ''),
             Sdate = NULL, Edate = NULL,
             type = "Daily", ylim = c(-10,30))




