# Barbu Elena, Proiect Biostatistica

install.packages("survival")
library(survival)
dataset=gbsg
attach(dataset)
names(dataset)
dataset
	# Verificam daca exista NA-uri in setul de date
sum(is.na(dataset))
# Nu exista NA-uri in setul de date

	# Verificam daca s-au strecurat greseli in variabilele binare 
sum((meno!=0&meno!=1))
sum(hormon!=0&hormon!=1)
sum(status!=0&status!=1)
#sau
any((meno!=0&meno!=1))
any(hormon!=0&hormon!=1)
any(status!=0&status!=1)
# Nu existš greseli in variabilele binare
	
	# Verificam ce valori are variabila grade

table(grade)

      # Verificam daca existš valori atipice
boxplot(age, main="Age")
jpeg("AgeBoxplot.jpeg")
boxplot(age, main="Age")
graphics.off()

boxplot(size, main="Size")
jpeg("SizeBoxplot.jpeg")
boxplot(size, main="Size")
graphics.off()

boxplot(nodes,main="Nodes")
jpeg("NodesBoxplot.jpeg")
boxplot(nodes,main="Nodes")
graphics.off()

boxplot(pgr,main="Pgr")
jpeg("pgrBoxplot.jpeg")
boxplot(pgr,main="Pgr")
graphics.off()

boxplot(er,main="Er")
jpeg("ErBoxplot.jpeg")
boxplot(er,main="Er")
graphics.off()

boxplot(rfstime,main="Rfstime")
jpeg("rfstimeBoxplot.jpeg")
boxplot(rfstime,main="Rfstime")
graphics.off()

# Valorile observate in grafice nu sunt erori, ci  valori reale posibile, deci nu se vor scoate din setul de date

	
	# EXEMPLIFICAREA UNOR TEHNICI DE STATISTICA DESCRIPTIVA
	# Analizam varsta pacientelor din studiu
summary(age)
# media varstei pacientelor din acest studiu este 53.05
# mediana varstei pacientelor din acest studiu este 53.00, ceea ce inseamna ca jumatate din paciente sunt mai tinere de 53 ani, iar jumatate sunt mai batrane
# cea mai tanara pacienta din acest studiu are 21 ani, iar cea mai varsnica 80
# 25% din paciente au mai putin de 46 ani, iar 75% au mai putin de 61 ani
install.packages("modeest")
library(modeest)
mfv(age)
sum(age==47)
# cea mai des intalnita varsta in setul nostru de date este 47 ani

install.packages("moments")
library(moments)
skewness(age)
shapiro.test(age)
# datele au o distributie aproximativ normala
plot(density(age),type="l",lwd=1.5,col="red",xlab="Varsta",ylab="Densitatea",xlim=c(0,90),ylim=c(0.00,0.04),main="Densitatea varstei")
jpeg("DensitateaVarstei.jpeg")
plot(density(age),type="l",lwd=1.5,col="red",xlab="Varsta",ylab="Densitatea",xlim=c(0,90),ylim=c(0.00,0.04),main="Densitatea varstei")
graphics.off()
hist(age,main="Histograma varstei")	
jpeg("VarstaHistograma.jpeg")
hist(age,main="Histograma varstei")	
graphics.off()


	#Relatia dintre variabila age si grade
grade=factor(grade,levels=c(1,2,3))
class(grade)
levels(grade)
boxplot(age~grade,main="Varsta in functie de gradul tumorii")
jpeg("Varsta în functie de gradul tumorii.jpeg")
boxplot(age~grade,main="Varsta in functie de gradul tumorii")
graphics.off()

	# vrem sa vedem relatia dintre variabila grade si grupurile de varsta: 20-34,35-49,50-64,65<
sum(age>20&age<35)
sum(age>34&age<50)
sum(age>49&age<65)
sum(age>64)
# am obtinut cate paciente se gasesc in fiecare grup de varsta propus in functie de gradul tumorii
sum((age>20&age<35)&grade=="1")
sum((age>20&age<35)&grade=="2")
sum((age>20&age<35)&grade=="3")

sum((age>34&age<50)&grade=="1")
sum((age>34&age<50)&grade=="2")
sum((age>34&age<50)&grade=="3")

sum((age>49&age<65)&grade=="1")
sum((age>49&age<65)&grade=="2")
sum((age>49&age<65)&grade=="3")

sum(age>64&grade=="1")
sum(age>64&grade=="2")
sum(age>64&grade=="3")

paciente=matrix(c(0,18,11,31,154,54,36,212,78,14,60,18),nrow=3,ncol=4,byrow=F)
colnames(paciente)=c("20-34","35-49","50-64",">65")
rownames(paciente)=c("1","2","3")
names(dimnames(paciente))=c("Gradul tumorii","Grupa de varsta")
paciente=as.table(paciente)
paciente
margin.table(paciente,1)
prop.table(paciente,2)
# observam ca din toate grupurile de varsta, cele mai multe paciente luate in studiu au gradul 2 de tumora
prop.table(paciente)
#cea mai mare frecventa in acest studiu o are grupul de paciente cu varsta cuprinsa intre 50-64 ani si au gradul 2 de tumora
  
	#Vrem sa vedem daca exista corelatie liniara intre varsta pacentelor si marimea tumorii ????????
plot(age,size)
cor(age,size)
#obersvam si din grafic si prin calcularea coeficientului Pearson ca nu exista corelatie liniara intre cele doua variabile


		#VERIFICAREA IPOTEZELOR STATISTICE

	#Vrem sa vedem daca la nivel de semnificatie de 0.05 cantitatea de receptori pentru estrogeni este mai crescut in grupul cu terapie hormonala decat in cel fara aceasta tarapie
fterapie=subset(dataset,hormon==0)
nrow(fterapie) 
cuterapie=subset(dataset,hormon==1)
nrow(cuterapie)
#alfa=0.05
#H0: Media receptorilor pentru estrogeni este aceeasi in cele doua grupuri
#Ha: Media receptorilor pentru estrogeni este mai mare in grupul care a luat terapia hormonala din acest studiu.

t.test(cuterapie$er,fterapie$er,mu=0,alternative="greater",paired=FALSE)

# p-valoartea(0.0003703) < 0.05 => se respinge H0
# Observam ca media receptorilor de estrogeni este mai mare in grupul care a luat terapie hormonala.

	# Vrem sa vedem daca la nivelul de semnificatie 0.05 pacientele care se afla in perioada de postmenopauza fara terapie hormonala au un nivel mai crescut de receptori pentru estrogeni decat pacientele fara terapie hormonala aflate in premenopauza
faraterapie=subset(dataset,hormon==0)
detach(dataset)
attach(faraterapie)

premenopauza=subset(faraterapie,meno==0)
postmenopauza=subset(faraterapie,meno==1)
nrow(premenopauza)
nrow(postmenopauza)
# avem suficiente date pentru a aplica un test t
# cele doua grupuri sunt independente, deci se va aplica un test t nepereche
# alfa= 0.05
# H0: Nu exista diferente semnificative intre mediile receptorilor pentru estrogeni in cele doua grupuri
# Ha: Media receptorilor pentru estrogeni la persoanele aflate in postmenopauza este mai mare decat media receptorilor pentru estrogeni la persoanele aflate in premenopauza 

t.test(postmenopauza$er,premenopauza$er,mu=0,alterfnative="greater",paired=FALSE)

# p-valoarea este < alfa(0.05) => se respinge H0
# observam ca media receptorilor pentru estrogeni este mai mare pentru pacientele din acest studiu aflate in postmenopauza
detach(faraterapie)
attach(dataset)
dataset


	# Vrem sa vedem daca la un nivel de semnificatie de 0.05 marimea tumorii difera la cele 3 grade 
# verifica daca sunt satisfacute conditiile pt anova ?????
g1=subset(dataset,grade=="1")
g2=subset(dataset,grade=="2")
g3=subset(dataset,grade=="3")
var(g1$size)
var(g2$size)
var(g3$size)
shapiro.test(g1$size)
shapiro.test(g2$size)
shapiro.test(g3$size)
# Nu se foloseste ANOVA,ci testul Kruskal-Wallis

# alfa=0.05
# H0: nu exista diferente semnificative intre grupuri in ceea ce priveste dimensiunea tumorii
# Ha: exista diferente semnificative intre grupuri in ceea ce priveste dimensiunea tumorii
kruskal.test(size~grade)
# p-valoarea: 0.0047 => se respinge H0

pairwise.wilcox.test(size,grade)
# Nu exista diferente semnificative intre dimensiunile tumorii la persoanele care au tumora de gradul 1 si 3, 2 si 3
# Exista diferente semnificative intre dimensiunile tumorii la persoanele care au tumora de gradul 1 si 2

	# Vrem sa vedem daca cei care au luat terapie hormonala au ramas vii si fara recurenta intr-o proportie mai mare decat cei care nu au luat aceasta terapie
cuterapie=subset(dataset,hormon=="1")
fterapie=subset(dataset,hormon=="0")
sum(cuterapie$status=="0")
sum(fterapie$status=="0")
sum(cuterapie$status=="1")
sum(fterapie$status=="1")

proportie=matrix(c(152,235,94,205),nrow=2,ncol=2,byrow=F)
colnames(proportie)=c(" viu fara recurenta","     mort sau cu recurenta")
rownames(proportie)=c(" cu terapie ", " fara terapie ")
as.table(proportie)
# H0: Nu exista diferente semnificative intre cele 2 proportii
# Ha: Proportia de persoane ramase vii si fara recurente este mai mare la cei care au luat terapie hormonala

prop.test(c(152,235),c(246,440),alternative="greater")

# p- valoarea ( 0.02056) <0.05 => se respinge H0
# Observam ca poportia de persoane ramase vii si fara recurente la cele care au luat terapie hormonala (0.6178862 ) este mai mare decat la cele fara terapie (0.5340909)




?????	# INCERCARE DE A FACE UN MODEL PREDICTIV CU REGRESIE LINIARA trebuiee sa mai stergi cate ceva de aici
names(dataset)
cor(age,size)
cor(age,nodes)
cor(age,er)
cor(age,pgr)
cor(age,rfstime)
cor(size,nodes)
cor(er,size)
cor(pgr,size)
cor(size,rfstime)
cor(nodes,size)
cor(er,nodes)
cor(pgr,nodes)
cor(er,pgr)
cor(size,rfstime)
cor(nodes,rfstime)

plot(nodes,size)
cor(nodes,size)

model1=lm(size~nodes)
model1
plot(nodes,size)
abline(model1)
summary(model1)
shapiro.test(model1$residuals)
scale(model1$residuals)

d1=dataset[-c(58,61,137,195,479,550,557,674),]
model2=lm(d1$size~d1$nodes)
model2
plot(d1$nodes,d1$size)
abline(model2)
summary(model2)
shapiro.test(model2$residuals)
plot(model2$fitted,model2$residuals)

plot(er,pgr)
model3=lm(pgr~er)
model3
plot(er,pgr)
abline(model3)
summary(model3)
scale(model3$residuals)
shapiro.test(model3$residuals)

d2=dataset[-c(118,674,675,676,677,679,680,681,686,682,683,684,685),]
 
plot(d2$er,d2$pgr)
model4=lm(d2$pgr~d2$er)
model4
abline(model2)
summary(model4)
shapiro.test(model4$residuals)

x=log(er)
y=log(pgr)
plot(x,y)


	# MODEL DE REGRESIE LOGISTICA
m1=glm(status~age+as.factor(meno)+size+as.factor(grade)+pgr+er+as.factor(hormon)+rfstime,family=binomial(logit),data=dataset)
summary(m1)
#scoatem er
m1=glm(status~age+as.factor(meno)+size+as.factor(grade)+pgr+as.factor(hormon)+rfstime,family=binomial(logit),data=dataset)
summary(m1)
#scoatem age
m1=glm(status~as.factor(meno)+size+as.factor(grade)+pgr+as.factor(hormon)+rfstime,family=binomial(logit),data=dataset)
summary(m1)
#scoatem hormon
m1=glm(status~as.factor(meno)+size+as.factor(grade)+pgr+rfstime,family=binomial(logit),data=dataset)
summary(m1)
# scoatem grade
m1=glm(status~as.factor(meno)+size+pgr+rfstime,family=binomial(logit),data=dataset)
summary(m1)
# scoatem meno
m1=glm(status~size+pgr+rfstime,family=binomial(logit),data=dataset)
summary(m1)
# mai ce scoatem ?????? rfstime????


exp(cbind(OR=coef(m1),confint(m1)))
# la o persoana aflata in postmenopauza exista o sansa mai mare de recurenta sau moarte decat la una aflata in premenopauza
# cresterea marimii tumorii cu o unitate creste in medie cu 1.3% sansa de recurenta sau moarte
# persoanele care prezinta tumora de gradul 2 sau 3 sau o sansa mai mare de recurenta sau moarte decat cele la care tumora este de gradul 1
# cresterea cantitatii de receptori de progesteron cu o unitate va scade cu 0.17% sansa de recurenta sau moarte
install.packages("boot")
library(boot)

cv.err=cv.glm(dataset,m1,K=10)
acuratete=1-cv.err$delta[2]
acuratete
# modelul m1 prezinta o acuratete de 80%

# dar aplici aici hoslem????
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(status,fitted(m1))

		# crearea unui model de regresie logistica
#Vrem sa creem un model care sa arate probabilitatea ca status sa fie 1 in functie de variabila size
m2=glm(status~size,family=binomial(logit),data=dataset)
m2
# logit(P(status==1))~~-0.8113+0.0188*size
# atunci cand crete size creste si logit(P(status==1)) 


exp(cbind(OR=coef(m2),confint(m2)))
# cresterea marimii tumorii cu o unitate creste cu 1.898% sansa de recurenta sau moarte???????
 	
	# Evaluarea modelului predictiv
m2.nul=glm(status~1,family=binomial(logit),data=dataset)
#H0:toti coeficientii beta sunt =0 => modelul m2 nu este cu nimic mai bun decat modelul nul
#Ha:cel putin unul dintre coeficientii beta este diferit de 0

anova(m2,m2.nul,test="Chisq")
install.packages("lmtest")
library(lmtest)
lrtest(m2,m2.nul)
# p-valoarea 0.0005889  => se respinge H0
# modelul nostru e bun

	# evaluarea calitatii predictorului 
#H0:coeficientul beta=0
#Ha:coeficientul beta este diferit de 0
summary(m2)
# p-valoarea 0.000786 => se respinge H0 
# predictorul este semnificativ

	# evaluarea concordantei
#H0:exista concordanta intre functia estimata de raspuns si functia logistica
#Ha:nu exista concordanta intre functia estimata de raspuns si functia logistica
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(status,fitted(m2))
#p-valoarea 0.1918 => Se accepta H0

	# acuratetea modelului
library(boot)
cv.err2=cv.glm(dataset,m2,K=10)
acuratetea=1-cv.err2$delta[2]
acuratetea
# modelul m2 are o acuratete de predictie de aproximativ 75%

		# predictie
datenoi=data.frame(size=15)
predict(m2,datenoi,type="response")
# probabilitatea ca pacientul sa moara sau de recurenta este 0.3706825 
datenoi=data.frame(size=55)
predict(m2,datenoi,type="response")
datenoi=data.frame(size=86)
predict(m2,datenoi,type="response")



detach(dataset)