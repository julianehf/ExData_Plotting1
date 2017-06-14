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



# plot 1
par(mfrow=c(1,1))
par(mar=c(5,5,2,2))
hist(datap$Global_active_power,col='red1',xlab='Global Active Power (kilowatts)',main='Global Active Power')
dev.copy(png,file='plot1.png',width=480,height=480,units='px')
dev.off()
