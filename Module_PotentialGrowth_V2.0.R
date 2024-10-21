#################################
# SOLANUM MODEL
################################



climate <- as.data.frame(weather)
climate$Date <- as.Date(climate$YYYYMMDD)
climate$Tmin <- climate$TMIN
climate$Tmax <- climate$TMAX
climate$sunsh <- climate$photoperiod
climate$Rad <- climate$SRAD

#sowing <- sDate
#harvest <- sowing + 90
#Eday = 14




##################        INICIALIZACION DE VARIABLES           ############################
v=0.00  # variablity
TDM = 0.0
cTII = 0.0
Tac = 0.0
tt<-0.0
PDay<--1
ttfixed<--1
a = (log(2))/log((Tu_1-Tb_0)/(To_0-Tb_0))
#############################################################################################
#                                                                                           #
#                                 Inicio de la simulacion                                   #
#                                                                                           #
#############################################################################################
rnd<-runif(1)
rnd<-2*rnd-1
rdm = (log((1+rnd)/(1-rnd)))/1.82 ## log: es logaritmo natural
# levantamos toda la base de datos de clima en un data frame


#changed (below line)
  # abrir un archivo ascii
# juntamos la fecha y se agrega como una nueva columna
#Date<-as.Date(paste(climate$Year,"-",climate$Month,"-",climate$Day,sep=""))
#climate<-data.frame(Date,climate)


#changed (below line)

# seleccionamos datos de clima correspondiente al periodo de simulacion
climate <- climate[climate$Date >= as.Date(sowing) & climate$Date <= as.Date(harvest), ]
# retiro las columnas Date, Tmin y Tmax del dataframe y los guardo en vectores por separado
DATE<-as.Date(climate$Date)
TMIN<-as.numeric(climate$Tmin)
TMAX<-as.numeric(climate$Tmax)
TT<-thermalTime(DATE,TMIN,TMAX,sowing,harvest,EDay,c(0, 12, 24, 35)) # se calcula el TT
climate<-data.frame(climate,TT=TT$tt) # agrego la columna TT calculada al df climate
timeDuration<-as.numeric(as.Date(harvest)-as.Date(sowing) +1)

# OUTPUTS
tdm <- vector(mode="numeric", length=timeDuration)
dty <- vector(mode="numeric", length=timeDuration)
fty <- vector(mode="numeric", length=timeDuration)
cc <- vector(mode="numeric", length=timeDuration)


# OUTPUTS
Pindex<-vector()
Tindex<-vector()
TII<-vector()
Part<-vector()
ctii<-vector()


for(i in 1:timeDuration)
{
  N = climate$sunsh[i]
  Tmax = climate$Tmax[i]
  Tmin = climate$Tmin[i]
  Tav = (Tmax+Tmin)/2
  tt = climate$TT[i];
  Tindex[i] = ifelse(Tav<Tb_0,0,ifelse(Tav>Tu_1,0,(2*((Tav-Tb_0)^a)*((To_0-Tb_0)^a)-((Tav-Tb_0)^(2*a)))/((To_0-Tb_0)^(2*a))))
  Pindex[i] = ifelse(N>Pc,exp(-1*(w*(N-Pc))),1)
  TII[i] = Tindex[i]*Pindex[i]
  cTII = cTII+TII[i];
  ctii[i]<-cTII
  if(cTII>20 && ttfixed==-1) ttfixed<-tt
  Tac<-tt
  Part[i] = A_0*exp(-1*(exp((-1*(Tac-Tu_0))/b)))
  #print(cTII)
}


if(cTII>=20){
  dato<-ttfixed
}else{
  dato<-climate$TT[timeDuration];
}
Tu_cTII=(dato+b)/Tu_0

# OUTPUTS
Part_cTII<-vector()
HI_cTII<-vector()
dW<-vector()
dty1<-vector()
dty2<-vector()

for(i in 1:timeDuration)
{
  tt = climate$TT[i];
  Tac<-tt
  Part_cTII[i] = A_0*exp(-1*(exp((-1*(Tac-Tu_0*Tu_cTII))/b)))
  HI_cTII[i] = ifelse(Tu_cTII<=1,Part[i],Part_cTII[i])
  
  canopy = wmax*exp(-1*(tm/(Tac*plantDensity)))*(1+(te-Tac)/(te-tm))*((Tac/te)^(te/(te-tm)))
  canopy1 = rdm*v*canopy+canopy
  PDay=PDay+1
  DAE = ifelse(PDay>=EDay,PDay-EDay,0)
  CC = ifelse(DAE<=0,0,ifelse(canopy1>0,canopy1,10^(-6)))  
  PAR = climate$Rad[i]*0.5
  dW[i] = (RUE*CC*PAR)/100
  TDM = TDM+dW[i];
  
  DTY1 = TDM*Part[i]
  DTY2 = TDM*HI_cTII[i]
  DTY = ifelse(Tu_cTII<=1,DTY1,DTY2)
  FTY = DTY/DMcont
  
  tdm[i]<-TDM
  dty[i]=DTY;
  fty[i]=FTY;
  cc[i]=CC;  
  dty1[i]=DTY1;
  dty2[i]=DTY2;
}
df<-data.frame(Fecha=climate$Date,TT=climate$TT,cc,Tmin=climate$Tmin,Tmax=climate$Tmax,N=climate$sunsh,sr=climate$Rad,dW,tdm,Tindex,Pindex,TII,ctii,Part,Part_cTII,HI_cTII,dty1,dty2,dty,fty,tdm,check.names=FALSE)
#write.csv(df,"out_cameroon.csv")
#write.csv(df,"out_congo.csv")
#write.csv(df,"out_kenia.csv")





