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

# get min/max 
s25_tmp_ag = tidyr::unite_(s25_tmp, paste(colnames(s25_tmp)[c(1,2,3)], collapse="-"), colnames(s25_tmp)[c(1,2,3)])
s25_tmp_ag[,1] = as.Date(gsub('_', '-', s25_tmp_ag[,1]))
s25_tmp_min = aggregate.data.frame(s25_tmp_ag[,3], by = list(s25_tmp_ag[,1]), FUN = 'min')
s25_tmp_max = aggregate.data.frame(s25_tmp_ag[,3], by = list(s25_tmp_ag[,1]), FUN = 'max')
s25_tt = data.frame(date = s25_tmp_min[,1], tmin =  s25_tmp_min[,2], tmax = s25_tmp_max[,2])

# get min/max 
s50_tmp_ag = tidyr::unite_(s50_tmp, paste(colnames(s50_tmp)[c(1,2,3)], collapse="-"), colnames(s50_tmp)[c(1,2,3)])
s50_tmp_ag[,1] = as.Date(gsub('_', '-', s50_tmp_ag[,1]))
s50_tmp_min = aggregate.data.frame(s50_tmp_ag[,3], by = list(s50_tmp_ag[,1]), FUN = 'min')
s50_tmp_max = aggregate.data.frame(s50_tmp_ag[,3], by = list(s50_tmp_ag[,1]), FUN = 'max')
s50_tt = data.frame(date = s50_tmp_min[,1], tmin =  s50_tmp_min[,2], tmax = s50_tmp_max[,2])

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
df1 = base_tmp_ag[52585:140256,]
df1$Year = base_tmp[52585:140256,2]
df1$Month = base_tmp[52585:140256,3]
df1$Day = base_tmp[52585:140256,4]
df1$Hour = base_tmp[52585:140256,5]
df1$hour = NULL
df1$temperature.dC. = NULL
df1$pot.et.in. = NULL


df2 = merge(df1, base_tt, by.x = 'year-month-day', by.y = 'date', all.x = TRUE)
df3 = merge(df2, base_hrs, by.x = 'year-month-day', by.y = 'date', all.x = TRUE)
df4 = merge(df3, dpt_ave, by.x = 'year-month-day', by.y = 'date', all.x = TRUE)
df4$wnd = base_wnd[96433:184104,5]
df4$dpt = base_dpt[96433:184104,5]
df4$rad = base_rad[96433:184104,5]
df4$sdtemp = base_tmp$temperature.dC.[52585:140256]
df4$julian = jday[52585:140256]

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

# 25 scenario
df_25 = df4
df_25_1 = merge(df_25, s25_tt, by.x = 'year-month-day', by.y = 'date', all.x = TRUE)
df_25_1$sdtemp = s25_tmp[,5]

# 50 scenario
df_50 = df4
df_50_1 = merge(df_50, s50_tt, by.x = 'year-month-day', by.y = 'date', all.x = TRUE)
df_50_1$sdtemp = s50_tmp[,5]


base_data = data.frame(Station.Numer = df3$seg,
                       Year = df4$Year,
                       Month = df4$Month,
                       Day = df4$Day,
                       Hour = df4$Hour,
                       Julian = df4$julian,
                       Temp.subdaily = df4$sdtemp,
                       Tdew.subdaily = df4$dpt, 
                       RH.subdaily = rh(df4$tmin, df4$tmax, df4$dpt.day),
                       Rs.subdaily = df4$rad/24,
                       n.daily = df3$nhrs,
                       uz.subdaily = df4$wnd*0.44704
                       #Tmin.daily = df4$tmin,
                       #Tmax.daily = df4$tmax
                       )

base_data_m10 = data.frame(Station.Numer = df3$seg,
                       Year = df4$Year,
                       Month = df4$Month,
                       Day = df4$Day,
                       Hour = df4$Hour,
                       Julian = df4$julian,
                       Temp.subdaily = df4$sdtemp - 10,
                       Tdew.subdaily = df4$dpt, 
                       RH.subdaily = rh(df4$tmin, df4$tmax, df4$dpt.day),
                       Rs.subdaily = df4$rad/24,
                       n.daily = df3$nhrs,
                       uz.subdaily = df4$wnd*0.44704
                       #Tmin.daily = df4$tmin,
                       #Tmax.daily = df4$tmax
)

base_data_p10 = data.frame(Station.Numer = df3$seg,
                           Year = df4$Year,
                           Month = df4$Month,
                           Day = df4$Day,
                           Hour = df4$Hour,
                           Julian = df4$julian,
                           Temp.subdaily = df4$sdtemp + 10,
                           Tdew.subdaily = df4$dpt, 
                           RH.subdaily = rh(df4$tmin, df4$tmax, df4$dpt.day),
                           Rs.subdaily = df4$rad/24,
                           n.daily = df3$nhrs,
                           uz.subdaily = df4$wnd*0.44704
                           #Tmin.daily = df4$tmin,
                           #Tmax.daily = df4$tmax
)



s25_data = data.frame(Station.Numer = df3$seg,
                       Year = df_25_1$Year,
                       Month = df_25_1$Month,
                       Day = df_25_1$Day,
                       Hour = df_25_1$Hour,
                       Julian = df_25_1$julian,
                       Temp.subdaily = df_25_1$sdtemp,
                       Tdew.subdaily = df_25_1$dpt, 
                       RH.subdaily = rh(df_25_1$tmin.y, df_25_1$tmax.y, df_25_1$dpt.day),
                       Rs.subdaily = df4$rad/24,
                       n.daily = df3$nhrs,
                       uz.subdaily = df_25_1$wnd*0.44704
                       #Tmin.daily = df_25_1$tmin.y,
                       #Tmax.daily = df_25_1$tmax.y
)


s50_data = data.frame(Station.Numer = df3$seg,
                      Year = df_50_1$Year,
                      Month = df_50_1$Month,
                      Day = df_50_1$Day,
                      Hour = df_50_1$Hour,
                      Julian = df_50_1$julian,
                      Temp.subdaily = df_50_1$sdtemp,
                      Tdew.subdaily = df_50_1$dpt, 
                      RH.subdaily = rh(df_50_1$tmin.y, df_50_1$tmax.y, df_50_1$dpt.day),
                      Rs.subdaily = df4$rad/24,
                      n.daily = df3$nhrs,
                      uz.subdaily = df_50_1$wnd*0.44704
                      #Tmin.daily = df_50_1$tmin.y,
                      #Tmax.daily = df_50_1$tmax.y
)


# update constants ..
data(constants)
#  for mont county md
constants$lat = 39.1287
constants$lat_rad = 0.68292
constants$as = 0.23        #(Roderick, 1999, page 181) 
constants$bs = 0.5  # I left these for now, not sure if they are good or not ....
constants$Elev = 107 # meters gaithersburg md
constants$z = 10 # meters
#skipping the constants for estimating sunshine hours from cloud cover, as sunshine hours are used as
  # input
# ones for crae
constants$PA = 1183
# skipping constant for Morton's procedure 
  
base_input <- ReadInputs(base_data, constants,
                   stopmissing=c(1,1,1),
                   timestep="subdaily",
                   interp_missing_days = TRUE,
                   interp_missing_entries = TRUE,
                   interp_abnormal = TRUE,
                   missing_method = "DoY average",
                   abnormal_method = "DoY average"
                   )

p10_input <- ReadInputs(base_data_p10, constants,
                        stopmissing=c(1,1,1),
                        timestep="subdaily") 

m10_input <- ReadInputs(base_data_m10, constants,
                        stopmissing=c(1,1,1),
                        timestep="subdaily") 


s25_input <- ReadInputs(s25_data, constants,
                         stopmissing=c(1,1,1),
                         timestep="subdaily")

s50_input <- ReadInputs(s50_data, constants,
                        stopmissing=c(1,1,1),
                        timestep="subdaily")

setwd("/Users/grad/Desktop/evapotranspiration")

ab = ET.Abtew(base_data, constants, ts="daily", solar="sunshine hours")

bc = ET.BlaneyCriddle(base_data, constants, ts="daily", solar="sunshine hours", height = F)

bs = ET.BrutsaertStrickler(base_data, constants, ts="daily", solar="sunshine hours", alpha=0.23)

ca = ET.ChapmanAustralian(data, constants, ts="daily", PenPan= T, solar="sunshine hours", alpha=0.23)

base_gg = ET.GrangerGray(base_input, constants, ts="daily", solar="data",
               windfunction_ver=1956, alpha=0.23)


#####################################################################################
# of interest 

base_hn = ET.Hamon(base_input, constants = NULL, ts="daily")
base_hs = ET.HargreavesSamani(base_input, constants, ts="daily")
base_jh = ET.JensenHaise(base_input, constants, ts="daily", solar="data")
base_la = ET.Linacre(base_input, constants, ts="daily")
base_mk = ET.Makkink(base_input, constants, ts="daily", solar="data")
base_pt = ET.PriestleyTaylor(base_input, constants, ts="daily", solar="data", alpha=0.23)
base_pm = ET.PenmanMonteith(base_input, constants, windfunction_ver=1956, ts="daily", solar="data", wind="yes", crop="short")
base_bc = ET.BlaneyCriddle(base_input, constants, ts="daily", solar="sunshine hours", height = F)


s25_hn = ET.Hamon(s25_input, constants = NULL, ts="daily")
s25_hs = ET.HargreavesSamani(s25_input, constants, ts="daily")
s25_jh = ET.JensenHaise(s25_input, constants, ts="daily", solar="data")
s25_la = ET.Linacre(s25_input, constants, ts="daily")
s25_mk = ET.Makkink(s25_input, constants, ts="daily", solar="data")
s25_pt = ET.PriestleyTaylor(s25_input, constants, ts="daily", solar="data", alpha=0.23)
s25_pm = ET.PenmanMonteith(s25_input, constants, ts="daily", solar="data", windfunction_ver=1956, wind="yes", crop="short")
s25_bc = ET.BlaneyCriddle(s25_input, constants, ts="daily", solar="sunshine hours", height = F)

s50_hn = ET.Hamon(s50_input, constants = NULL, ts="daily")
s50_hs = ET.HargreavesSamani(s50_input, constants, ts="daily")
s50_jh = ET.JensenHaise(s50_input, constants, ts="daily", solar="data")
s50_la = ET.Linacre(s50_input, constants, ts="daily")
s50_mk = ET.Makkink(s50_input, constants, ts="daily", solar="data")
s50_pt = ET.PriestleyTaylor(s50_input, constants, ts="daily", solar="data", alpha=0.23)
s50_pm = ET.PenmanMonteith(s50_input, constants, ts="daily", solar="data", wind="yes", crop="short")
s50_bc = ET.BlaneyCriddle(s50_input, constants, ts="daily", solar="sunshine hours", height = F)

m10_hn = ET.Hamon(m10_input, constants = NULL, ts="daily")
m10_hs = ET.HargreavesSamani(m10_input, constants, ts="daily")
m10_jh = ET.JensenHaise(m10_input, constants, ts="daily", solar="data")
m10_la = ET.Linacre(m10_input, constants, ts="daily")
m10_mk = ET.Makkink(m10_input, constants, ts="daily", solar="data")
m10_pt = ET.PriestleyTaylor(m10_input, constants, ts="daily", solar="data", alpha=0.23)
m10_pm = ET.PenmanMonteith(m10_input, constants, ts="daily", solar="data", wind="yes", crop="short")
m10_bc = ET.BlaneyCriddle(m10_input, constants, ts="daily", solar="sunshine hours", height = F)


p10_hn = ET.Hamon(p10_input, constants = NULL, ts="daily")
p10_hs = ET.HargreavesSamani(p10_input, constants, ts="daily")
p10_jh = ET.JensenHaise(p10_input, constants, ts="daily", solar="data")
p10_la = ET.Linacre(p10_input, constants, ts="daily")
p10_mk = ET.Makkink(p10_input, constants, ts="daily", solar="data")
p10_pt = ET.PriestleyTaylor(p10_input, constants, ts="daily", solar="data", alpha=0.23)
p10_pm = ET.PenmanMonteith(p10_input, constants, ts="daily", solar="data", wind="yes", crop="short")
p10_bc = ET.BlaneyCriddle(p10_input, constants, ts="daily", solar="sunshine hours", height = F)


######################################################################################################


ms = ET.MattShuttleworth(data, constants, ts="daily", solar="sunshine hours", alpha=0.23, r_s=70, CH=0.12)
mb = ET.McGuinnessBordne(data, constants, ts="daily")

#mca = ET.MortonCRAE(data, constants, ts="daily", est="potential ET",
#                   solar="sunshine hours", Tdew= T, alpha = NULL)

#mcw = ET.MortonCRWE(data, constants, ts="monthly", est="potential ET",
#                    solar="sunshine hours", Tdew= T, alpha = NULL)

base_p = ET.Penman(base_input, constants, ts="daily", solar="sunshine hours",
               wind="yes", windfunction_ver=1956, alpha = 0.7, z0 = 0.4)

s25_p = ET.Penman(s25_input, constants, ts="daily", solar="sunshine hours",
              wind="yes", windfunction_ver=1956, alpha = 0.7, z0 = 0.4)

s50_p = ET.Penman(s50_input, constants, ts="daily", solar="sunshine hours",
              wind="yes", windfunction_ver=1956, alpha = 0.7, z0 = 0.4)

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

one <- base_hn
two <- base_hs
three <- base_jh
four <- base_la
five <- base_bc
six <- base_pt
seven <- base_pm

et_multi_plot(two, seven, one, four, five, six, three, type = 'Monthly') 

et_multi_plot_mon(two, seven, one, four, five, six, three, type = 'Monthly') 


et_multi_plot_3(seven, seven, seven, type = 'Daily', Sdate = '1992-01-01', Edate = '1992-12-31') 

et_multi_plot_1(seven, type = 'Daily', Sdate = '1992-01-01', Edate = '1992-12-31') 



one <- s25_hn
two <- s25_hs
three <- s25_jh
four <- s25_la
five <- s25_mk
six <- s25_pt
seven <- s25_pm

et_multi_plot(two, seven, one, four, five, six, three, type = 'Daily') 

et_multi_plot_3(two, seven, one, type = 'Daily')

one <- s50_hn
two <- s50_hs
three <- s50_jh
four <- s50_la
five <- s50_mk
six <- s50_pt
seven <- s50_pm

et_multi_plot(two, seven, one, four, five, six, three, type = 'Daily') 

et_multi_plot_3(two, seven, one, type = 'Daily') 


one <- base_hn
two <- s25_hn
three <- s50_hn
four <- base_pm
five <- s25_pm
six <- s50_pm
seven <- base_jh

et_multi_plot(one, two, three, four, five, six, seven, type = 'Daily') 

et_multi_plot_3(four, five, six, type = 'Daily') 



dev.off()

#plot some of my own
library(ggplot2)
library(reshape2)
hamon_aa = data.frame(year = seq(1991, 2000), base = base_hn$ET.AnnualAve[1:10], s25 =  s25_hn$ET.AnnualAve[1:10], s50 = s50_hn$ET.AnnualAve[1:10])

har_aa = data.frame(year = seq(1991, 2000), base = base_hs$ET.AnnualAve[1:10], s25 =  s25_hs$ET.AnnualAve[1:10], s50 = s50_hs$ET.AnnualAve[1:10])

pm_aa = data.frame(year = seq(1991, 2000), base = base_pm$ET.AnnualAve[1:10], s25 =  s25_pm$ET.AnnualAve[1:10], s50 = s50_pm$ET.AnnualAve[1:10])

bc_aa = data.frame(year = seq(1991, 2000), base = base_bc$ET.AnnualAve[1:10], s25 =  s25_bc$ET.AnnualAve[1:10], s50 = s50_bc$ET.AnnualAve[1:10])

pt_aa = data.frame(year = seq(1991, 2000), base = base_pt$ET.AnnualAve[1:10], s25 =  s25_pt$ET.AnnualAve[1:10], s50 = s50_pt$ET.AnnualAve[1:10])

jh_aa = data.frame(year = seq(1991, 2000), base = base_jh$ET.AnnualAve[1:10], s25 =  s25_jh$ET.AnnualAve[1:10], s50 = s50_jh$ET.AnnualAve[1:10])


all_comp = data.frame(year = h_melt$year,
                      variable = h_melt$variable,
                      hamon = h_melt$value,
                      hargraves = har_melt$value,
                      penman = pm_melt$value)

all_melt = melt(all_comp[,c(1,3,4,5)], 1)
all_melt$scenario = c(rep('base', 10), rep('s25', 10), rep('s50', 10))

ggplot(all_melt) + 
  geom_boxplot(aes(x=scenario, y=value, color = variable)) +
  ylab('average annual pet mm/day')




ggplot(har_melt) + 
  geom_boxplot(aes(x=variable, y=value, color = variable))

ggplot(pm_melt) + 
  geom_boxplot(aes(x=variable, y=value, color = variable))



hamon_base_ave = mean(hamon_aa$base)
hamon_s25_ave = mean(hamon_aa$s25)
hamon_s50_ave = mean(hamon_aa$s50)

hamon_change = c(hamon_base_ave, hamon_s25_ave, hamon_s50_ave)

har_base_ave = mean(har_aa$base)
har_s25_ave = mean(har_aa$s25)
har_s50_ave = mean(har_aa$s50)

hargraves_change = c(har_base_ave, har_s25_ave, har_s50_ave)

pm_base_ave = mean(pm_aa$base)
pm_s25_ave = mean(pm_aa$s25)
pm_s50_ave = mean(pm_aa$s50)

penman_change = c(pm_base_ave, pm_s25_ave, pm_s50_ave)

bc_base_ave = mean(bc_aa$base)
bc_s25_ave = mean(bc_aa$s25)
bc_s50_ave = mean(bc_aa$s50)

bc_change = c(bc_base_ave, bc_s25_ave, bc_s50_ave)

pt_base_ave = mean(pt_aa$base)
pt_s25_ave = mean(pt_aa$s25)
pt_s50_ave = mean(pt_aa$s50)

pt_change = c(pt_base_ave, pt_s25_ave, pt_s50_ave)

jh_base_ave = mean(jh_aa$base)
jh_s25_ave = mean(jh_aa$s25)
jh_s50_ave = mean(jh_aa$s50)

jh_change = c(jh_base_ave, jh_s25_ave, jh_s50_ave)


comp_line = data.frame(scenario = as.factor(c('base', 's25', 's50')),
                       hamon = hamon_change, 
                       hargraves = hargraves_change,
                       penman = penman_change, 
                       bcriddle = bc_change, 
                       prestaylor = pt_change,
                       jenhies = jh_change
                       
  )

melt_line = melt(comp_line, 1)

theme_set(theme_gray(base_size = 18))

ggplot(data=melt_line, aes(x=scenario, y=value, group=variable, color=variable))+ 
  geom_line() + ylab('average annual pet mm/day')





##### plus minus thing ... ... 

hamon_aa = data.frame(year = seq(1991, 2000), base = base_hn$ET.AnnualAve[1:10], p10 =  p10_hn$ET.AnnualAve[1:10], m10 = m10_hn$ET.AnnualAve[1:10])

har_aa = data.frame(year = seq(1991, 2000), base = base_hs$ET.AnnualAve[1:10], p10 =  p10_hs$ET.AnnualAve[1:10], m10 = m10_hs$ET.AnnualAve[1:10])

pm_aa = data.frame(year = seq(1991, 2000), base = base_pm$ET.AnnualAve[1:10], p10 =  p10_pm$ET.AnnualAve[1:10], m10 = m10_pm$ET.AnnualAve[1:10])

bc_aa = data.frame(year = seq(1991, 2000), base = base_bc$ET.AnnualAve[1:10], p10 =  p10_bc$ET.AnnualAve[1:10], m10 = m10_bc$ET.AnnualAve[1:10])

pt_aa = data.frame(year = seq(1991, 2000), base = base_pt$ET.AnnualAve[1:10], p10 =  p10_pt$ET.AnnualAve[1:10], m10 = m10_pt$ET.AnnualAve[1:10])

jh_aa = data.frame(year = seq(1991, 2000), base = base_jh$ET.AnnualAve[1:10], p10 =  p10_jh$ET.AnnualAve[1:10], m10 = m10_jh$ET.AnnualAve[1:10])




hamon_base_ave = mean(hamon_aa$base)
hamon_p10_ave = mean(hamon_aa$p10)
hamon_m10_ave = mean(hamon_aa$m10)

hamon_change = c((hamon_m10_ave - hamon_base_ave)/hamon_base_ave, 0, (hamon_p10_ave- hamon_base_ave)/hamon_base_ave)

har_base_ave = mean(har_aa$base)
har_p10_ave = mean(har_aa$p10)
har_m10_ave = mean(har_aa$m10)

hargraves_change = c((har_m10_ave - har_base_ave)/har_base_ave, 0, (har_p10_ave- har_base_ave)/har_base_ave)


pm_base_ave = mean(pm_aa$base)
pm_p10_ave = mean(pm_aa$p10)
pm_m10_ave = mean(pm_aa$m10)

penman_change = c((pm_m10_ave - pm_base_ave)/pm_base_ave, 0, (pm_p10_ave- pm_base_ave)/pm_base_ave)


bc_base_ave = mean(bc_aa$base)
bc_p10_ave = mean(bc_aa$p10)
bc_m10_ave = mean(bc_aa$m10)

bc_change = c((bc_m10_ave - bc_base_ave)/bc_base_ave, 0, (bc_p10_ave- bc_base_ave)/bc_base_ave)

pt_base_ave = mean(pt_aa$base)
pt_p10_ave = mean(pt_aa$p10)
pt_m10_ave = mean(pt_aa$m10)

pt_change = c((pt_m10_ave - pt_base_ave)/pt_base_ave, 0, (pt_p10_ave- pt_base_ave)/pt_base_ave)


jh_base_ave = mean(jh_aa$base)
jh_p10_ave = mean(jh_aa$p10)
jh_m10_ave = mean(jh_aa$m10)

jh_change = c((jh_m10_ave - jh_base_ave)/jh_base_ave, 0, (jh_p10_ave- jh_base_ave)/jh_base_ave)


comp_line = data.frame(scenario = as.factor(c('1_minus_10', '2_zero', '3_plus_10')),
                       hamon = hamon_change, 
                       hargraves = hargraves_change,
                       penman = penman_change, 
                       bcriddle = bc_change, 
                       prestaylor = pt_change,
                       jenhies = jh_change
                       
)

melt_line = melt(comp_line, 1)

theme_set(theme_gray(base_size = 18))

ggplot(data=melt_line, aes(x=scenario, y=value, group=variable, color=variable))+ 
  geom_line() + ylab('Relative % Change')








ETForcings(base_input, one, forcing = 'Tmax')
ETForcings(base_input, two, forcing = 'Tmax')
ETForcings(base_input, seven, forcing = 'Tmax')
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



delta_t = 
