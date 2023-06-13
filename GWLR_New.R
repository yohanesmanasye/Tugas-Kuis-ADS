library(spgwr)
library(ggplot2)
library(maptools)
library(lmtest)
library(car)
library(nortest)
library(DescTools)
library(spdep)
library(sf)
library(mapview)
library(tibble)
library(AER)
library(GWmodel)
library(pscl)


setwd("D:/All about Collage/Perkuliahan/Semester 6/Analisis Data Spasial/Tugas + Kuis")
library(openxlsx)
covidjatim = read.xlsx("Spasial Covid 1_fix.xlsx",sheet="Sheet1")
covidjatim
# covidjatim <- read.csv("Spasial Covid 1 (test).txt",sep=';')

#preprocessing
sum(is.na(covidjatim))
str(covidjatim)
covidjatim <- covidjatim[complete.cases(covidjatim), ]

#EDA
# -----GWR-----
## -----Regresi linier klasik-----
## Dataset:
## y = status risiko
## X1 = Kepadatan Penduduk
## X2 = Jumlah Fasilitas Kesehatan
## X3 = Jumlah Masyarakat Miskin (ribu)
## X4 = Persentase keluhan kesehatan masyarakat

# ============= Logistic Regression ==================


#Logistic Regression
logisticreg = glm(status_risiko  ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                  +persentase_keluhan_kesehatan,
                  data=covidjatim, family=binomial(link="logit"))
summary(logisticreg)

AIC(logisticreg)

### -----Uji Asumsi-----
err.regklasik2<- residuals(logisticreg)

# -----Normalitas-----
ad.test(err.regklasik2)
hist(err.regklasik2)
qqnorm(err.regklasik2,datax=T)
qqline(rnorm(length(err.regklasik2),mean(err.regklasik2),sd(err.regklasik2)),datax=T, col="red")

# -----Autokorelasi-----
dwtest(logisticreg)

# -----Heterogenitas-----
bptest(logisticreg)

# -----Multikolinieritas-----
vif(logisticreg)


#calculate McFadden's R-squared for model
pR2(logisticreg)['McFadden']



### =========== GLWR ==============

# Buat distance matrix berdasarkan latitude dan longitude
library(sp)
data.sp.GWR=covidjatim
coordinates(data.sp.GWR) <-10:11 #kolom menyatakan letak Long-Lat
  
DM = gw.dist(dp.locat = coordinates(data.sp.GWR))

#---- FIXED Gaussian
bw_fixgas <- bw.ggwr(logisticreg$formula,
                     data=data.sp.GWR,
                     approach="AICc",
                     kernel="gaussian",
                     adaptive=FALSE,
                     family = "binomial",
                     dMat=DM)


res_fixgas <-ggwr.basic(logisticreg$formula,
                        data=data.sp.GWR,
                        bw=bw_fixgas,
                        kernel="gaussian",
                        adaptive=FALSE,
                        family = "binomial",
                        dMat=DM)
res_fixgas

#---- FIXED Gaussian
bw_fixbi <- bw.ggwr(logisticreg$formula,
                     data=data.sp.GWR,
                     approach="AICc",
                     kernel="bisquare",
                     adaptive=FALSE,
                     family = "binomial",
                     dMat=DM)


res_fixbi <-ggwr.basic(logisticreg$formula,
                        data=data.sp.GWR,
                        bw=bw_fixbi,
                        kernel="bisquare",
                        adaptive=FALSE,
                        family = "binomial",
                        dMat=DM)
res_fixbi



#Adaptive Gaussian tidak dapat digunakan karena
#karena parameter adaptive = TRUE hanya dapat digunakan dengan kernel yang memiliki bobot non-negatif.

#Namun, ketika menggunakan kernel Gaussian dengan `adaptive = FALSE`, 
#tidak ada masalah karena dalam kasus tersebut, tidak ada pendekatan adaptif yang digunakan. 
#Bandwidth lokal tetap konstan untuk setiap observasi, 
#dan karena tidak ada perhitungan bobot adaptif yang melibatkan nilai negatif, error tidak muncul.



#----- ADAPTIVE BISQUARE
bw_adpbi <- bw.ggwr(logisticreg$formula,
                    data=data.sp.GWR,
                    approach="AICc",
                    kernel="bisquare",
                    adaptive=TRUE,
                    family = "binomial",
                    dMat=DM)

res_adpbi <- ggwr.basic(logisticreg$formula,
                        data=data.sp.GWR,
                        bw=bw_adpbi,
                        kernel="bisquare",
                        adaptive=TRUE,
                        family = "binomial",
                        dMat=DM)
                      
res_adpbi


### Evaluasi Model

model = c("LogisticReg","GWLR-Fix Gauss", "GWLR-Fix Bisquare", "GWLR-Adpv Bisquare")
R2 = c(0.1406, 0.3023, 0.3060,0.3630)
AIC = c(AIC(logisticreg),50.6924,50.4305,49.7580)

evaluasi = data.frame(model,R2,AIC)
evaluasi

## Dari evaluasi didapat model terbaik yaitu gwlr adaptive bisquare


#================ UJI SIGNIFIKANSI PARAMETER TIAP MODEL
##=== ESTIMASI PARAMETER
df_gwlr = as.data.frame(res_adpbi$SDF)
rownames(df_gwlr) = covidjatim$'KabupatenKota'

#intercept
intercept = res_adpbi$SDF$Intercept
intercept

x1 = res_adpbi$SDF$kepadatan_penduduk
x2 = res_adpbi$SDF$jumlah_faskes
x3 = res_adpbi$SDF$jumlah_miskin
x4 = res_adpbi$SDF$persentase_keluhan_kesehatan


##=== MENDAPATKAN NILAI PVALUE
pvalx1 = 2*pt(abs(x1),df=38,lower.tail = FALSE) #ubah t-value menjadi p-value
pvalx1
pvalx2 = 2*pt(abs(x2),df=38,lower.tail = FALSE)
pvalx2
pvalx3 = 2*pt(abs(x3),df=38,lower.tail = FALSE)
pvalx3
pvalx4 = 2*pt(abs(x3),df=38,lower.tail = FALSE)
pvalx4

##=== EXPORT HASIL DALAM FORMAT XLSX
setwd("D:/All about Collage/Perkuliahan/Semester 6/Analisis Data Spasial/Tugas + Kuis")
library(writexl)
outputGWLR = data.frame(covidjatim$KabupatenKota,intercept,x1,x2,x3,x4,pvalx1,pvalx2,pvalx3,pvalx4)
writexl::write_xlsx(outputGWLR,"outputGWLR.xlsx")





