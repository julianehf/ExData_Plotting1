rm(list=ls())
library(data.table)

a=list.files()
i=grep('.txt',a)
file = a[i]


data = fread(file,header=TRUE,sep=";",select='Date')
dates = data$Date
dates = as.Date(dates,"%d/%m/%Y")
toskip = which(!(dates>=as.Date('2007-02-01',"%Y-%m-%d") & dates<=as.Date('2007-02-02',"%Y-%m-%d")))
datap = read.table(file,header=TRUE,sep=';')[-toskip,]

# check for missing values 
any(apply(datap,2,function(x){  x=='?'})) # no missing values?
any(apply(datap,2,is.na))

datap$Date = as.Date(datap$Date,'%d/%m/%Y')
datap[,3:9]=apply(datap[,3:9],2,function(x) {as.numeric(as.character(x))})

#plot 4
par(mfrow=c(2,2))
par(mar=c(5,5,2,2))
par(cex=0.7)
with(datap,{
        plot(c(1:length(datap$Time)),Global_active_power,
             type='l',lty=1,lwd=1,axes=FALSE,ylab='Global Active Power (kilowatts)',xlab='')
        axis(1,at=c(which(weekdays(datap$Date)=='Jeudi')[1], which(weekdays(datap$Date)=='Vendredi')[1], length(datap$Date)   ),labels=c("Thu","Fri","Sat"))
        axis(2)
        box(which='plot')
})

with(datap, {
        plot( c(1:length(datap$Time)),Voltage,
              type = 'l',xlab='datetime',axes=F)
        axis(1,at=c(which(weekdays(datap$Date)=='Jeudi')[1], which(weekdays(datap$Date)=='Vendredi')[1], length(datap$Date)   ),labels=c("Thu","Fri","Sat"))
        axis(2)
        box(which='plot')
})

with(datap,{
        plot( c(1:length(datap$Time)) , Sub_metering_1,
              type='l'  , axes=FALSE, col = 'black',ylab='Energy sub metering',xlab='')
        lines( c(1:length(datap$Time)) , Sub_metering_2,
               type='l',col='red'  )
        lines( c(1:length(datap$Time)) , Sub_metering_3,
               type='l',col='blue'  )
        
        legend('topright',legend=names(datap)[7:9],col=c('black','red','blue'),lty=1,bty='n')
        axis(1,at=c(which(weekdays(datap$Date)=='Jeudi')[1], which(weekdays(datap$Date)=='Vendredi')[1], length(datap$Date)   ),labels=c("Thu","Fri","Sat"))
        axis(2)
        box(which='plot')
})

with(datap,{
        plot(c(1:length(datap$Time)) , Global_reactive_power,
             type='l',axes=F,xlab='datetime')
        axis(1,at=c(which(weekdays(datap$Date)=='Jeudi')[1], which(weekdays(datap$Date)=='Vendredi')[1], length(datap$Date)   ),labels=c("Thu","Fri","Sat"))
        axis(2)
        box(which='plot')
})

dev.copy(png,file='plot4.png',width=480,height=480,units='px')
dev.off()

