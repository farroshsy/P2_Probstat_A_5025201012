# Poin 1a
# Carilah Standar Deviasi dari Data Selisih Pasangan Pengamatan Tabel Diatas

# Data Sebelum melakukan aktivitas
before <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)

# Data Setelah melakukan aktivitas
after <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)

# Check Data
my_data <- data.frame(
  group = rep(c("before", "after"), each = 9),
  saturation = c(before, after)
)

# Print Data
print(my_data)

# Standar Devisiasi before activity
sd_before <- sd(before)
sd_before

# Standar Devisiasi after activity
sd_after <- sd(after)
sd_after

# Poin 1b
# Carilah Nilai t (p-value)

# Menggunakan t-test
t.test(before, after, alternative = "greater", var.equal = FALSE)

# Poin 1c
# tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas A jika diketahui tingkat signifikansi a = 5% serta H0 : "tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas A

var.test(before, after)

t.test(before, after, mu = 0, alternative = "two.sided", var.equal = TRUE)

# Poin 2a

install.packages("BSDA")
library(BSDA)

sum.test(mean.x = 23500, sigma.x = 3900, n.x = 100,
          alternative = "greater", mu = 20000,
          conf.level = 0.95)
#Setuju

# Poin 2b 

tsum.test(mean.x=23500, sd(3900), n.x=100)

# Poin 2c
# Kesimpulan yang didapat yaitu dapat diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun


# Poin 3a
# H0 dan H1
# H0 = 9.50 && H1 = 10.98


# Poin 3b 
# Hitung Sampel Statistik

tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, 
          mean.y =2.79 , s.y = 1.32, n.y = 27, 
          alternative = "greater", var.equal = TRUE)

# Poin 3c
# Lakukan Uji Statistik (df = 2)

install.packages("mosaic")
library(mosaic)

plotDist(dist='t', df=2, col="dark blue")

# Poin 3d
# Nilai Kritikal

qchisq(p = 0.05, df = 2, lower.tail=FALSE)

# Poin 3e
# Teori keputusan adalah teori formal pengambilan keputusan di bawah ketidakpastian. 

# Poin 3f
# Kesimpulan yang didapatkan yaitu perbedaan rata-rata yang terjadi tidak ada jika dilihat dari uji statistik dan akan ada tetapi tidak signifikan jika dipengaruhi nilai kritikal.

# Poin 4a
# Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1, grup 2, grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.

dataoneway <- read.table("onewayanova.txt",h=T)
attach(dataoneway)
names(dataoneway)

dataoneway$Group <- as.factor(dataoneway$Group)
dataoneway$Group = factor(dataoneway$Group,labels = c("grup 1", "grup 2", "grup 3"))

class(dataoneway$Group)

Group1 <- subset(dataoneway, Group == "grup 1")
Group2 <- subset(dataoneway, Group == "grup 2")
Group3 <- subset(dataoneway, Group == "grup 3")

qqnorm(Group1$Length)
qqline(Group1$Length)

qqnorm(Group2$Length)
qqline(Group2$Length)

qqnorm(Group3$Length)
qqline(Group3$Length)

# Poin 4b
# Mencari homogenity of variances
bartlett.test(Length ~ Group, data = dataoneway)

# Poin 4c 
# Uji anova satu arah
model1 = lm(Length ~ Group, data = dataoneway)
anova(model1)

# Poin 4d 
# nilai p adalah 0.8054, maka H0 ditolak

# Poin 4e
# Post-hoc test Tukey HSD
TukeyHSD(aov(model1))

# Poin 4f
# Visualisasikan data dengan ggplot2

library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")


# Poin 5a
# Buatlah plot sederhana untuk visualisasi data

install.packages("multcompView")
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

GTL <- read_csv("GTL.csv")
head(GTL)

str(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

# Poin 5b
# Uji ANOVA dua arah

GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

# Poin 5c
# Tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan

data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

# Poin 5d
# Uji Tukey

tukey <- TukeyHSD(anova)
print(tukey)

# Poin 5e
# Compact letter display untuk menunjukkan perbedaan signifikan

antara uji Anova dan uji Tukey

tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)

write.csv("GTL_summary.csv")
