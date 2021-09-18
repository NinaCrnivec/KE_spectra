library(Rfa3)

ectoplot_local=function (data, domainsize, add = FALSE, ...)
{
    ect <- ecto(data)
    maxMN <- length(ect) - 1
    if (add)
        points(domainsize/(1:maxMN), ect[-1], type = "o", ...)
    else plot(domainsize/(1:maxMN), ect[-1], log = "xy", type = "o", xlim=c(domainsize,domainsize/maxMN), ...)
    #else plot(domainsize/(1:maxMN), ect[-1], log = "xy", type = "o", ...)
}


#============
#Set domain:
domain="SI13"
#============

fn=paste0("/home/bstrajnar/exp/rundir_forman/nwcruc/as2020061800_nwcruc_prod/nwcruc/prod/forecast/ICMSHSI13+0006")

file=FAopen(fn)

numPoints=file$gridpar[1]
dx=file$geopar["EDELX"]
dx=dx/1000. #Covert [m]-> [km]
domainsize=numPoints*dx

run_year =file$time[1]
run_month=file$time[2]
run_day  =file$time[3]
run_hour =file$time[4]
run_min  =file$time[5]
fc_hour  =file$time[7]

date=paste0(run_year,"/",run_month,"/",run_day," z",run_hour,":",run_min," +",fc_hour,"h")
#2020/6/18 z0:0 +6h

fig_name=paste0("fig_",domain,"_KEspectrum.png")
png(fig_name, width=800, height=600, type="cairo")

#Set figure parameters:
stitle=paste0("Horizontal kinetic energy spectrum\n", domain, ": ", date)
sxlab="Wavelength [km]"
sylab="Spectral kinetic energy [J/kg]"

sm=2.0
sl=2.0
sa=1.5
sy=c(10e-9,10e2)

c1="red"
c2="green"
c3="blue"

#Select three vertical levels:
lev1="21" #250 mb
lev2="36" #500 mb
lev3="87" #lowest level

lev_arr=c(lev1, lev2, lev3)

var_u="WIND.U.PHYS."
var_v="WIND.V.PHYS."


for (lev in lev_arr){

   fieldname_u <- paste0("S0", formatC(lev,width=2,flag=0), var_u)
   fieldname_v <- paste0("S0", formatC(lev,width=2,flag=0), var_v)
    
   field_u=FAdec(file, fieldname_u, outform="S", clip=F)
   field_v=FAdec(file, fieldname_v, outform="S", clip=F)

   field_wind=sqrt(field_u**2 + field_v**2)

   #Plot various levels at the same figure:
   if(lev==lev1) ectoplot_local(field_wind, domainsize, main=stitle, xlab=sxlab, ylab=sylab, cex.main=sm, cex.lab=sl, cex.axis=sa, ylim=sy, col=c1)
   if(lev==lev2) ectoplot_local(field_wind, domainsize, main=stitle, xlab=sxlab, ylab=sylab, cex.main=sm, cex.lab=sl, cex.axis=sa, ylim=sy, col=c2, add=TRUE)
   if(lev==lev3) ectoplot_local(field_wind, domainsize, main=stitle, xlab=sxlab, ylab=sylab, cex.main=sm, cex.lab=sl, cex.axis=sa, ylim=sy, col=c3, add=TRUE)
   ylim=sy
   
   #Add theoretical slope limits:
   abline(-4.4, 5./3., col='black', lty=2, lwd=2)
   abline(-9.0, 3.,    col='black', lty=2, lwd=2)
   text(150, 0.001, expression(paste("k"^"-3")),   col='black', cex=2.0)
   text(15,  0.001, expression(paste("k"^"-5/3")), col='black', cex=2.0)

   l1=paste0("level = ", lev1)
   l2=paste0("level = ", lev2)
   l3=paste0("level = ", lev3)

   legend_inp=c(l1, l2, l3)
   col_inp=c(c1, c2, c3)
   legend("topright", legend=legend_inp, col=col_inp, lty=1:1, cex=2.0, lwd=3.0)  
}

dev.off()




