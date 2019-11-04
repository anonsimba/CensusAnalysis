cen=read.csv(file.choose())
cen
#test

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

a=c(ker1[1],wb1[1],asm1[1],trp1[1])
barplot(a,col=rainbow(4))
pie(a,col=rainbow(4))
hist(a)

barplot(a,b)
hist(ker1[1],wb1[1],asm1[1])

pairs(~cen$Population+cen$Literate+cen$Workers)

a=c(10,10,20,10,10,24,60,40,80,60,24,80,10,75,100)
hist(a,col=rainbow(length(a)))
boxplot(cen$Population~cen$Literate)
a=matrix(c(1,1,2,15,4,3,10,7,1),3,3)
heatmap(scale(mtcars))
co





statewise=list(c(sik,pon,goa))
class(statewise)
