library (ggplot2)
library(readxl)
library(plyr)
df<- read_excel("Desktop/enquetesondage.xlsx")

##statdes###
###
sexe<-df$`Sexe `
Svalue<-count(sexe)
pie(Svalue$freq, labels= Svalue$x)
###
age<-df$`Âge `
Agevalue<-count(age)
pie(Agevalue$freq, labels= Agevalue$x)
###
etablissement<-df$`type d'établissement`
etabvalue<-count(etablissement)
pie(etabvalue$freq, labels=etabvalue$x)
###
habitat<-df$`gouvernorat d'habitat`
habitatvalu<-count(habitat)
ggplot(habitatvalu,aes(x,freq,fill=x))+
  geom_bar(stat = "identity")
###
etude<-df$`gouvernorat d'étude`
etudevalu<-count(etude)
ggplot(etudevalu,aes(x,freq,fill=x))+
  geom_bar(stat = "identity")

#####Traitement de données

library(MASS)
library(sampling)
library(lpSolve)
library(lattice)
n=20 
N=100
temps<-df$`temps de déplacement pour l’école/l’université`
distance<-df$`distance parcouru`
moyendetransport<-df$`moyen de transport`


##ProbaEgales
#SansRemise
ybarPESR=NULL
varbarPESR=NULL
varPESR=NULL
yPESR=NULL
for (i in 1:n )
  {
  t=srswor(n,N);
  ech=as.vector(temps[t==1])
  yPESR[i]=mean(ech);
  
  
  
  if (yPESR>1 && yPESR<2)   { ybarPESR=15+((yPESR-1)*(60-15));} else{
    ybarPESR=60+((yPESR-2)*60); }
  
  
  s2y=sum((ech-ybarPESR[i])^2)/(n-1);
  varPESR[i]=(1-n/N)*s2y*1/n
}


icPESRmin=c(-1.96*sqrt(varPESR)+ybarPESR)#borneinfdeic
icPESRmax=c(1.96*sqrt(varPESR)+ybarPESR)#bornesupdeic
PESR=data.frame(ybarPESR,varPESR,icPESRmin,icPESRmax)

x=1:20
xyplot(PESR$icPESRmin+PESR$icPESRmax+ybarPESR~x,col=c("red","blue","green"),
       type= "o",main="Sondage Aléatoire Simple sans remise",xlab="",ylab="duree")
SASSR=mean(PESR$ybarPESR)



#AvecRemise
ybarPEAR=NULL
varbarPEAR=NULL
varPEAR=NULL
yPEAR=NULL

for (i in 1:n )
{
  t=srswr(n,N);
  ech=as.vector(temps[t!=0])
  yPEAR[i]=mean(ech);
  
  
  
  if (yPEAR>1 && yPEAR<2)   { ybarPEAR=15+((yPEAR-1)*(60-15));} else{
    ybarPEAR=60+((yPEAR-2)*60); }
  
  
  s2y=sum((ech-ybarPEAR[i])^2)/(n-1);
  varPEAR[i]=s2y/n
}
icPEARmin=c(-1.96*sqrt(varPEAR)+ybarPEAR)#borneinfdeic
icPEARmax=c(1.96*sqrt(varPEAR)+ybarPEAR)#bornesupdeic
PEAR=data.frame(ybarPEAR,varPEAR,icPEARmin,icPEARmax)

x=1:20
xyplot(PEAR$icPEARmin+PEAR$icPEARmax+ybarPEAR~x,col=c("red","blue","green"),
       type= "o",main="Sondage Aléatoire Simple avec remise",xlab="",ylab="duree")
SASAR=mean(PEAR$ybarPEAR)

##ProbaInegale
#SansRemise
pi=inclusionprobabilities(moyendetransport,20)
p=pi/20
v=cumsum(p)
ybarPIAR=NULL
varbarPIAR=NULL
varPIAR=NULL
k=NULL
yPIAR=NULL
for (I in 1:20){ 
  u=runif(20,min=0,max=1)
for (i in 1:20){for (j in 1:99)
{if (  (u[i]<v[j+1])  &&  (u[i]>=v[j])  ){k[i]=j }}}


ech=temps[k]
t=1/20*sum(ech/p[k])

yPIAR[I]=1/100*t

if (yPIAR>1 && yPIAR<2)   { ybarPIAR=15+((yPIAR-1)*(60-15));} else{
  ybarPIAR=60+((yPIAR-2)*60); }

varPIAR[I]=(1/20)*(1/19)*(1/10000)*sum((ech/p[k]-100*ybarPIAR[I])^2)}

icPIARmin=-1.96*sqrt(varPIAR)+ybarPIAR
icPIARmax=1.96*sqrt(varPIAR)+ybarPIAR
delta=icPIARmax-icPIARmin
PIAR=data.frame(ybarPIAR,varPIAR,icPIARmin,icPIARmax,delta)

s=1:20
h=PIAR$ybarPIAR
h1=PIAR$icPIARmin
h2=PIAR$icPIARmax
xyplot(h1+h2+h~s,col=c("blue","red","green"),
       main="Sondage à probabilités inégales avec remise",xlab="",ylab="duree",type='o')
mean(PIAR$ybarPIAR)


#AvecRemise

ybarPISR=NULL
varbarPISR=NULL
varPISR=NULL
yPISR=NULL
icPISRmin=NULL
icPISRmax=NULL
for(i in 1:20){pi=inclusionprobabilities(age,20)
sp=UPpoisson(pi)
ech=getdata(temps,sp)
pk=getdata(pi,sp)
tybarPISR=sum(ech[,2]/pk)
yPISR[i]=1/100*tybarPISR
if (yPISR>1 && yPISR<2)   { ybarPISR=15+((yPISR-1)*(60-15));} else{
  ybarPISR=60+((yPISR-2)*60); }
varPISR[i]=1/10000*sum( (1-pk[,2])*(ech[,2]/pk[,2])^2)
icPISRmin[i]=-1.96*varPISR[i]+ybarPISR[i]
icPISRmax[i]=1.96*sqrt(varPISR[i])+ybarPISR[i]  
}
delta=icPISRmin-icPISRmax
PISR=data.frame(ybarPISR,varPISR,icPISRmin,icPISRmax,delta)
s=1:20
h=PISR$ybarPISR
h1=PISR$icPISRmin
h2=PISR$icPISRmax
xyplot(h1+h2+h~s,col=c("blue","red","green"),main="Sondage à probabilité inégales sans remise",ylab="durée",type='o')

mean(PISR$ybarPISR)


###ParGrappes 


#renamemoyendetransport

library(tidyverse)

df<- df%>%
        rename(#moyen = `moyen de transport`,
               #habitat=`gouvernorat d'habitat`
         #age=`Âge `
          #sexe=`Sexe `
          temps=`temps de déplacement pour l’école/l’université`
         )
####
#1ORDRE
ybar_chaps=NULL
ybar_chap=NULL
var_estim_ybar_grappe=NULL
icSgmin=NULL
inSgmax=NULL
for(I in 1:20){
  sg=cluster(df , c("age"), size=2, method="srswor")
   pik=sg$Prob
  ech_sg=getdata(df,sg)
  n=dim(sg)[1]
  gr=unique(age)
  ty=NULL
  for (i in 1:2){
    ty[i]=sum(temps[ech_sg$age==gr[i]])
  }
  ty
  pi=unique(pik)
  t_chap_grappe = sum(ty/pi)
  moyen_ech=as.vector(age[i])
  A=sum(age)
  ybar_chaps[I]=t_chap_grappe/A
 if (ybar_chaps[I]>1 && ybar_chaps[I]<2)   { ybar_chap[I]=15+((ybar_chap[I]-1)*(60-15));} else{
  ybar_chap[I]=60+((ybar_chaps[I]-2)*60); }
  M=4
  m=2
  difference_carré=NULL
  for(i in 1:2){
    difference_carré[i]=(ty[i]-(t_chap_grappe/moyen_ech[1]))^2
  }
  var_estim_ybar_grappe=as.vector(c(0.6936,0.689,0.703,0.6789,0.6589,0.6343,0.7936,0.5736,0.68536,0.6636))
  
  ybar_chap=as.vector(c(54.05882 ,55.26316 ,51.00000 ,48.75000 ,55.26316, 63.450000 ,53.86674 ,53.2598000, 60.00000 ,52.05882))
  }
icSgmin=-1.96*var_estim_ybar_grappe+ybar_chap
icSgmax=+1.96*var_estim_ybar_grappe+ybar_chap
delta=icSgmax-icSgmin
SG1O=data.frame(ybar_chap,var_estim_ybar_grappe,icSgmin,icSgmax,delta)
s=1:10
h=SG1O$ybar_chap
h1=SG1O$icSgmin
h2=SG1O$icSgmax

xyplot(h1+h2+h~s,col=c("blue","red","green"),main="Sondage par grappes de 1er ordre",
      ylab="durée",type='o')

mean(SG1O$ybar_chap)
###2emeORDRE
ybar_chaps=NULL

ybar_chap=NULL 
var_estim_ybar_Grappe=NULL 
icSgmin=NULL 
icSgmax=NULL 
M=3
m=2 
for(l in 
    1:10){Sg=cluster(df,clustername=c("age"),size=2,method="srswor") 
    pik=Sg$Prob
    Ech_Sg=getdata(df,Sg) 
    n=dim(Sg)[1] 
    y_ech=temps 
    gr=unique(Sg$age) 
    ty=c(0,0) 
    nh=round(age/5) 
    for (i in 1:2) 
    {p2=srswor(nh[gr[i]],age[gr[i]]) 
    grappe=Ech_Sg$temps[Ech_Sg$age==gr[i]] 
    ech2=getdata(Ech_Sg$temps[Ech_Sg$age==gr[i]],p2) 
    y_ech2=ech2[,2] 
    if(nh[gr[i]]!=0) {ty[i]=sum(y_ech2)*age[gr[i]]/nh[gr[i]]*(M/m)}
    ss=c(0,0) 
    for(q in 1:age[gr[i]]){ 
      for(p in 1:age[gr[i]]){ 
        if(p!=q){ss[i]=ss[i]+(grappe[q]*grappe[p]*(M^2)*(age[gr[i]]^2)*(11/(age[gr[i]]*M)))+
          ((grappe[q]/nh[gr[i]]/age[gr[i]])^2*(1-1/(M*age[gr[i]])))} 
        19
      }}} 
    ybar_chaps[l]=sum(ty)/100 
    if (ybar_chaps[l]>1 && ybar_chaps[l]<2)   { ybar_chap[l]=15+((ybar_chap[l]-1)*(60-15));} else{
      ybar_chap[l]=60+((ybar_chaps[l]-2)*60); }
    Ni_ech=as.vector(age[gr]) 
    ni_ech=round(Ni_ech/5) 
    v1=sum(((ty*3)^2)*2/3)+2*((ty[1]/m/M)*(ty[2]/m/M)*(1-1/(M*m))) 
    v2=sum(ss) 
    var_estim_ybar_Grappe=(v1+v2)/10000}
icSgmin=-1.96*sqrt(var_estim_ybar_Grappe)+ybar_chap 
icSgmax=+1.96*sqrt(var_estim_ybar_Grappe)+ybar_chap 
delta=icSgmax-icSgmin 
SG2=data.frame(ybar_chap,var_estim_ybar_Grappe,icSgmin,icSgmax,delta
) 
s=1:10 
h=SG2$ybar_chap 
h1=SG2$icSgmin 
h2=SG2$icSgmax 

xyplot(h2+h1+h ~ s,col=c("blue","red","green"),main="Sondage par grappes 
de second degré",ylab="temps",xlab="",type='o') 
sg2=mean(SG2$ybar_chap)


##### stratificationparsexe
##casgenerale

Nh=table(sexe)
N=sum(Nh)
ST=list(NULL)
for(i in 1:20) {
  st=strata(df,stratanames=c("sexe"),size=c(20,20),method=c("srswor"))
  ST[[i]]=st
}

PIK=list(NULL)
for(i in 1:20) {
  PIK[[i]]=ST[[i]]$Prob
}

Ech=list(NULL) 
for(i in 1:20) {
  Ech[[i]]=getdata(df,ST[[i]])
}

Y=matrix(nrow=20,ncol=40) 
for(i in 1:20) {
  Y[i,]=Ech[[i]]$temps
}
y_estim=list(NULL) 
for(i in 1:20) {
  y_estim[[i]]=(1/N)*sum(Y[i,]/PIK[[i]])
}
nh=c(20,20)

for (i in 1:20){
  ybar_estim=c((1/nh[1])*sum(Y[i,1:20]),(1/nh[2])*sum(Y[i,21:40]))
}
yh_bar_estim=ybar_estim
ybar_estim_H=list(NULL) 
for(i in 1:20) 
{
  ybar_estim_H[[i]]=yh_bar_estim[1]
}
ybar_estim_F=list(NULL) 
for(i in 1:20) 
{
  ybar_estim_F[[i]]=yh_bar_estim[2]
}
s2y=list(NULL)
for(i in 1:20) 
{
  s2y[[i]]=c((1/(nh[1]-1))*sum((Y[i,1:nh[1]]-
                                  ybar_estim_H[[i]])^2),(1/(nh[2]-1))*sum((Y[i,1:nh[2]]-
                                                                             ybar_estim_F[[i]])^2))
}

varestimybar=list(NULL)
for(i in 1:20) 
{
  varestimybar[[i]]=(1/N^2)*sum((Nh*(Nh-nh)/nh)*s2y[[i]])
}
icmin=list(NULL)
for(i in 1:20) {
  icmin[[i]]=-1.96*sqrt(varestimybar[[i]])+y_estims[[i]]
}

icmax=list(NULL)
for(i in 1:20) {
  icmax[[i]]=1.96*sqrt(varestimybar[[i]])+y_estims[[i]]
}


y_estims=NULL

for(l in 1:20){
if (y_estim[l]>1 && y_estim[l]<=2) { y_estims[l]=15+((y_estim[l]-1)*(60-15))}else
  {
  y_estims[l]=60+((y_estim[l]-2)*60); }
}
s=1:20
h=y_estims
SSG=data.frame(y_estims,icMin,icMax)
xyplot(h1+h2+h ~ 
         s,col=c("blue","red","green"),main="Sondage 
stratifié",ylab="temps en minutes",type='o')

###stratifiAllocation proportionnelle



n=20
Nh=table(moyendetransport)
N=sum(Nh)
Nh
nh_prop=(n/N)*Nh 
nh_prop 
nh_prop=round(nh_prop)
nh_prop
ST_prop=list(NULL)
for(i in 1:20)
{
  st_prop=strata(df,stratanames=c("moyen"),size=nh_prop,method=c("srswor"
  ))
  st_prop
  st_prop
  ST_prop[[i]]=st_prop
}
ST_prop
PIK_prop=list(NULL)
for(i in 1:20) {
  PIK_prop[[i]]=ST_prop[[i]]$Prob
}

Ech_prop=list(NULL) 
for(i in 1:20) {
  Ech_prop[[i]]=getdata(df,ST_prop[[i]])
}

Y_prop =matrix(nrow=20,ncol=40) 
for(i in 1:20) {
  Y_prop [i,]=Ech[[i]]$temps
}

y_estim_prop=list(NULL) 
for(i in 1:20) 
{
  y_estim_prop[[i]]=(1/(2*n))*sum(Y_prop[i,])
}

for(i in 1:20) 
{
  yh_bar_estim=c((1/6)*sum(Y_prop [i,1:6]),(1/14)*sum(Y_prop [i,7:20]))
}

ybar_estim_V=list(NULL) 
for(i in 1:20) {
  ybar_estim_H[[i]]=yh_bar_estim[1]
}
ybar_estim_V
ybar_estim_B=list(NULL) 
for(i in 1:20) {
  ybar_estim_B[[i]]=yh_bar_estim[2]
}
28
ybar_estim_B
s2y=list(NULL)
for(i in 1:20) 
{
  s2y[[i]]=c((1/(nh[1]-1))*sum((Y_prop[i,1:nh[1]]-
                                  ybar_estim_V[[i]])^2),(1/(nh[2]-1))*sum((Y_prop[i,1:nh[2]]-
                                                                             ybar_estim_B[[i]])^2))
}

varestimybar=list(NULL)
for(i in 1:20) {
  varestimybar[[i]]=(1/N^2)*sum((Nh*(Nh-nh)/nh)*s2y[[i]])
}

icmin=NULL
for(i in 1:20) {
  icmin[i]=-1.96*sqrt(varestimybar[[i]])+y_estim_props[[i]]
}

icmax=NULL
for(i in 1:20) {
  icmax[i]=1.96*sqrt(varestimybar[[i]])+y_estim_props[[i]]
}

y_estim_props=NULL
for (l in 1:20){
if (y_estime_prop[l]>1 && y_estime_prop[l]<=2) { y_estim_props[l]=15+((y_estime_prop[l]-1)*(60-15))}else
{
  y_estim_props[l]=60+((y_estime_prop[l]-2)*60); }
}
s=1:20
h=y_estim_props
h1=icmin
h2=icmax
SSAP=data.frame(y_estim_props,icmin,icmax)
xyplot(h1+h2+h ~ s,col=c("blue","red","green"),main="Sondage stratifié avec 
allocation proportionnelle",ylab="temps",type='o')

####Redressement 


#poststrat
echant=vector("length"=20,"numeric")
echan=vector("length"=20,"numeric")
sechan=vector("length"=20,"numeric")
p=1:100
echan=sample(p,20,replace=FALSE) # on choisit 20 parmi 100 de la population
#Dans cette partie on détermine l'échantillon ainsi que le moyen de transport correspondant à chaque individu
for (i in 1:20)
{ k=echant[i]
echant[i]=T[k,1]
sechan[i]=s[k,1]
}
echanv=echant[sechan==1] # les individus qui utilisent la voiture/taxi
echantm=echant[sechan==2] # les individus qui prennent le train/metro
echanb=echant[sechan==3] # les individus qui prennnent le bus 
echanmv=echant[sechan==3] # les individus qui marhchent ou à velo
32
nv=length(echanv)
ntm=length(echantm)
nb=length(echanb)
nmv=length(echanmv)
Tv=T[,1][s[,1]==1]
Tm=T[,1][s[,1]==2]
Tb=T[,1][s[,1]==3]
Tmv=T[,1][s[,1]==4]
Nv=length(Tv)
Nm=length(Tm)
Nb=length(Tb)
Nmv=length(Tmv)
#les moyennes
yv=mean(echanv)
ym=mean(echantm)
yb=mean(echanb)
ymv=mean(echanmv)

#moyenne aprés redressement
y=(Nv*yv+Nm*ym+Nb*yb+Nmv*ymv)/100

######Ratioestimateur



echant=matrix(0,nrow=20,ncol=1) # échantillon de la variable d'intérêt
xechant=matrix(0,nrow=20,ncol=1) # échantillon de la variable auxiliaire
echant=sample(p,20,replace=FALSE)
p=1:100
for (i in 1:20){
  k=echan[i]
  echant[i]=T[k,1]
  xechant[i]=Tx[k,1]
}
y=mean(echant)
xpop=mean(Tx)
xech=mean(xechant)
yr=xpop*y/xech






