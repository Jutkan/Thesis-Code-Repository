tuntidata_2021_alustava$date <- paste(tuntidata_2021_alustava$Month, tuntidata_2021_alustava$Day, tuntidata_2021_alustava$Hour, sep="_")

#Tehdään datasta aikasarjat
tscons = ts(tuntidata_2021_alustava$`Consumption (MWh)`)
tshydro = ts(tuntidata_2021_alustava$`Hydro Power (MWh)`)
tssolar = ts(tuntidata_2021_alustava$`Solar Power (MWh)`)
tswind = ts(tuntidata_2021_alustava$`Wind Power (MWh)`)
tsnuclear =ts(tuntidata_2021_alustava$`Nuclear Power (MWh)`)
tsother = ts(tuntidata_2021_alustava$`CHP (MWh)`+tuntidata_2021_alustava$`Separate Thermal Power (MWh)`)
tsnetimport=ts(tuntidata_2021_alustava$`Import-export (MWh)`)
tsimport=ts(tuntidata_2021_alustava$`Import (MWh)`)
tsexport=ts(tuntidata_2021_alustava$`Export (MWh)`*(-1))

#Lasketaan keskimääräinen uusiutuvan osuus muille tuotantomuodoille
ren = 15.6
fos = 10.4
tot = ren+fos
perc_of_fos=fos/tot #ei uusiutuvat/uusiutuvat
perc_of_ren=1-perc_of_fos
other_r=perc_of_ren*tsother
other_f=perc_of_fos*tsother

#Lasketan tuotanto yhteensä kuvaajia varten
total_w_hydro=(tsnuclear+tshydro)
total_w_wind=(total_w_hydro+tswind)
total_w_solar=(total_w_wind+tssolar)
total_W_other_r=(total_w_solar+other_r)
total_w_other_f=(total_W_other_r+other_f)
total_w_imports=(total_w_other_f+tsnetimport)

#Energiantuotanto ja kulutuskuvaaja
plot(tscons, col="black", ylim = c(600,14500), xlab="Time (h)", ylab="MWh")
lines(total_w_imports, col="purple")
polygon(tscons, col="purple")
lines(total_w_other_f,col="grey")
polygon(total_w_other_f, col="grey")
lines(total_W_other_r, col="green")
polygon(total_W_other_r, col="green")
lines(total_w_solar, col="yellow")
polygon(total_w_solar, col="yellow")
lines(total_w_wind, col="light blue")
polygon(total_w_wind, col="light blue")
lines(total_w_hydro, col="blue")
polygon(total_w_hydro, col="blue")
lines(tsnuclear, col="orange")
polygon(tsnuclear, col="orange")
legend(-40, 15050, legend=c("Nuclear", "Hydro", "Wind", "Solar", "Other CO2 neutral", "Other non-CO2 neutral", "Net imports"), 
       fill=c("orange", "blue", "light blue", "yellow", "green", "grey", "purple"), lty=1:2, cex=0.55)

#Pelkät ei-hiilineutraalit ja kulutus kuvaaja
totalnonr=other_f+tsnetimport
plot(tscons, col="black", ylim = c(800,14500), xlab="Time (h)", ylab="MWh")
lines(totalnonr, col="purple")
polygon(totalnonr, col="purple")
lines(other_f, col="grey")
polygon(other_f, col="grey")
legend(-40, 14500, legend=c("Total consumption", "Other non-CO2 neutral", "Net imports"), 
       fill=c("black", "grey", "purple"), lty=1:2, cex=0.55)

#Lasketaan prosentuaalinen osuus ei-co2-neutraaleista
#Tähän vois lisää ehkä nollaviivan
percnonr=(totalnonr/tscons)*100
plot(percnonr, xlab="Time (h)", ylab="Ratio (%)") #Percentage of non-CO2 neutral and imported energy from total consumption

#Lasketaan tunneittainen vaihtelu
newpercs=lag(percnonr, k=1) #Tunti 1->Tunti 2
hdifference=(newpercs-percnonr) #Tunti 2-Tunti 1
hdifferenceabs=abs(hdifference)
plot(hdifference, xlim=c(2, 1464), xlab="Time (h)", ylab="Change (% points)") #Difference between two sequential hours
plot(hdifferenceabs, Xlim=c(2, 1464), xlab="Time (h)", ylab="Change, absolute value (% points)") #Difference between two sequential hours, absolute value

#Lasketaan vaihtelun osuus koko ei-co2-neutraalien määrästä
ratio_d_nr=hdifferenceabs/percnonr*100
plot(ratio_d_nr, ylab="Ratio (%)") #Difference / amount non-co2 neutral and imported energy
#Muutos suhteessa ei-co2 neutraaleihin

'
#Lasketaan 2h vaihtelu
newpercs2=lag(percnonr, k=2) #Tunti 1->Tunti 3
hdifference2=(newpercs2-percnonr) #Tunti 3-Tunti 1
hdifferenceabs2=abs(hdifference2)
plot(hdifference2, xlim=c(2, 1464), ylab="Difference between hours n and n+2 (%)")
plot(hdifferenceabs2, Xlim=c(2, 1464), ylab="Difference between hours n and n+2, absolute value (%)")

#Lasketaan 2h vaihtelun osuus koko ei-co2-neutraalien määrästä
ratio_d_nr2=hdifferenceabs2/percnonr
plot(ratio_d_nr2, ylab="Difference / amount non-co2 neutral and imported energy (%)")
'

