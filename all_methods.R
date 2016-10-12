library(Evapotranspiration)
data(climatedata)
data(constants)
data(defaultconstants)

head(climatedata)

data <- ReadInputs(climatedata, constants,
                   stopmissing=c(10,10,3),
                   timestep="subdaily",
                   interp_missing_days = TRUE,
                   interp_missing_entries = TRUE,
                   interp_abnormal = TRUE,
                   missing_method = "DoY average",
                   abnormal_method = "DoY average")


setwd("/Users/grad/Desktop/evapotranspiration")

ab = ET.Abtew(data, constants, ts="daily", solar="sunshine hours")

bc = ET.BlaneyCriddle(data, constants, ts="daily", solar="sunshine hours", height = F)

bs = ET.BrutsaertStrickler(data, constants, ts="daily", solar="sunshine hours", alpha=0.23)

ca = ET.ChapmanAustralian(data, constants, ts="daily", PenPan= T, solar="sunshine hours", alpha=0.23)

gg = ET.GrangerGray(data, constants, ts="daily", solar="sunshine hours",
               windfunction_ver=1948, alpha=0.23)

hn = ET.Hamon(data, constants = NULL, ts="daily")

hs = ET.HargreavesSamani(data, constants, ts="daily")

jh = ET.JensenHaise(data, constants, ts="daily", solar="sunshine hours")

la = ET.Linacre(data, constants, ts="daily")

mk = ET.Makkink(data, constants, ts="daily", solar="sunshine hours")

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

par(ask=FALSE)
devAskNewPage(FALSE)
options(device.ask.default = FALSE)
grDevices::devAskNewPage(ask=FALSE)

par(mfrow = c(2,2))
et_compare_all(ab, bc, bs, ca, gg, hn, hs,
             labs = c('', '', '', '', '', '', ''),
             Sdate = NULL, Edate = NULL,
             type = "Daily", ylim = c(-3,30))

ETComparison(jh, la, mk, ms, mb, p, pm,
             labs = c('', '', '', '', '', '', ''),
             Sdate = NULL, Edate = NULL,
             type = "Daily", ylim = c(-2,25))

ETComparison(pp, pt, r, sj, t,
             labs = c('', '', '', '', ''),
             Sdate = NULL, Edate = NULL,
             type = "Daily", ylim = c(-10,30))




