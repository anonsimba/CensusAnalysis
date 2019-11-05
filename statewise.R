install.packages("plotly")
cen=read.csv(file.choose())

jk=cen[1:22,4:118]
hp=cen[23:34,4:118]
pjb=cen[35:54,4:118]
chdg=cen[55,4:118]
utk=cen[56:68,4:118]
hryn=cen[69:89,4:118]
del=cen[90:98,4:118]
raj=cen[99:131,4:118]
up=cen[132:202,4:118]
bih=cen[203:240,4:118]
sik=cen[241:244,4:118]
arp=cen[245:260,4:118]
nld=cen[261:271,4:118]
man=cen[272:280,4:118]
miz=cen[281:288,4:118]
trp=cen[289:292,4:118]
meg=cen[293:299,4:118]
asm=cen[300:326,4:118]
wb=cen[327:345,4:118]
jkd=cen[346:369,4:118]
ors=cen[370:399,4:118]
chth=cen[400:417,4:118]
mp=cen[418:467,4:118]
guj=cen[468:493,4:118]
dd=cen[494:496,4:118]
mhr=cen[497:531,4:118]
ap=cen[532:554,4:118]
ktka=cen[555:584,4:118]
goa=cen[585:586,4:118]
ker=cen[588:601,4:118]
lak=cen[587,4:118]
tn=cen[602:633,4:118]
pon=cen[634:637,4:118]
and=cen[638:640,4:118]

and1=apply(and,2,sum)
pon1=apply(pon,2,sum)
tn1=apply(tn,2,sum)
lak1=apply(lak,2,sum)
ker1=apply(ker,2,sum)
goa1=apply(goa,2,sum)
ktka1=apply(ktka,2,sum)
ap1=apply(ap,2,sum)
mhr1=apply(mhr,2,sum)
dd1=apply(dd,2,sum)
guj1=apply(guj,2,sum)
mp1=apply(mp,2,sum)
chth1=apply(chth,2,sum)
ors1=apply(ors,2,sum)
jkd1=apply(jkd,2,sum)
jk1=apply(jk,2,sum)
hp1=apply(hp,2,sum)
pjb1=apply(pjb,2,sum)
chdg1=apply(chdg,2,sum)
utk1=apply(utk,2,sum)
hryn1=apply(hryn,2,sum)
del1=apply(del,2,sum)
raj1=apply(raj,2,sum)
up1=apply(up,2,sum)
bih1=apply(bih,2,sum)
sik1=apply(sik,2,sum)
arp1=apply(arp,2,sum)
nld1=apply(nld,2,sum)
man1=apply(man,2,sum)
miz1=apply(miz,2,sum)
trp1=apply(trp,2,sum)
meg1=apply(meg,2,sum)
asm1=apply(asm,2,sum)
wb1=apply(wb,2,sum)


censu=data.frame(wb1,asm1,meg1,trp1,man1,nld1,arp1,sik1,bih1,up1,raj1,del1,hryn1,utk1,chdg1,pjb1,hp1,jk1,jkd1,ors1,chth1,and1,pon1,tn1,lak1,ker1,goa1,ktka1,ap1,mhr1,dd1,guj1,mp1)

cens1=as.data.frame(t(censu))

st_name=c("West Bengal","Assam","Meghalaya","Tripura","Manipur","Nagaland","Arunachal Pradesh","Sikkim","Bihar","Uttar Pradesh","Rajasthan","Delhi","Haryana","Utharkhand","Chandigarh","Punjab","Himachal Pradesh","Jammu & Kashmir","Jharkhand","Orissa","Chatthisgarh","Andaman","POndicherry","Tamil Nadu","Laksh","Kerala","Goa","Karnataka","Andhra Pradesh","Maharashtra","DAman&Diu","Gujarat","Madhya Pradesh")

#male+female pop

pop=censu[1,1:33]
mal=censu[2,1:33]
fem=censu[3,1:33]

z1=(mal/pop)*100
z2=(fem/pop)*100


In_states=st_name
zn=unlist(z1)
am=unlist(z2)
dat=data.frame(st_name,zn,am)
dat
p=plot_ly(dat, x = ~In_states, y = ~zn, type = 'bar', name = 'MALE',text=zn,textposition='auto') %>%
  add_trace(y = ~am, name = 'FEMALE') %>%
  layout(title="TOTAL POPULATION",xaxis = list(title = "STATES and UT"),yaxis = list(title = "PERCENTAGE"), barmode = 'group')
p

q=plot_ly(dat, x = ~In_states, y = ~zn, type = 'bar', name = 'MALE',text=zn,textposition='auto') %>%
  add_trace(y = ~am, name = 'FEMALE') %>%
  layout(title="TOTAL POPULATION",xaxis = list(title = "STATES and UT"),yaxis = list(title = "PERCENTAGE"), barmode = 'stack')
q




newpop1=rbind(z1,z2)
newpop=matrix(unlist(newpop1),2,15)

barplot(newpop,names.arg = st_name[1:15],main = "Male : Female Population",xlab = "States",ylab = "% of Total Population",space = 1)

#------------------------------------------------

#rura+urban households

library(plotly)

th=censu[37,1:33]
rh=censu[35,1:33]
uh=censu[36,1:33]

z1=(rh/th)*100
z2=(uh/th)*100



In_states=st_name
zn=unlist(z1)
am=unlist(z2)
dat=data.frame(st_name,zn,am)
dat
p=plot_ly(dat, x = ~In_states, y = ~zn, type = 'bar', name = 'RURAL',text=zn,textposition='auto') %>%
  add_trace(y = ~am, name = 'URBAN') %>%
  layout(title="HOUSEHOLDS",xaxis = list(title = "STATES and UT"),yaxis = list(title = "PERCENTAGE"), barmode = 'group')
p

q=plot_ly(dat, x = ~In_states, y = ~zn, type = 'bar', name = 'RURAL',text=zn,textposition='auto') %>%
  add_trace(y = ~am, name = 'URBAN') %>%
  layout(title="HOUSEHOLDS",xaxis = list(title = "STATES and UT"),yaxis = list(title = "PERCENTAGE"), barmode = 'stack')
q



newh1=rbind(z1,z2)
newh=matrix(unlist(newh1),2,33)
newh

barplot(newh,names.arg = st_name[1:33],main="Households",xlab = "States",ylab = "% of Households",space = 1,col = rainbow(2))
legend("topright",c("rural","urban"),fill = rainbow(2))



#-----------------------------------------------------------------------------------





