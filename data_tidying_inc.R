rm(list=ls())
library(readr)

url_2019 <- "https://datahub.io/sports-data/spanish-la-liga/r/season-1819.csv"
football <- read_csv(url_2019)
#football <- read_csv("/Users/cynthiabuenomedeiros/Downloads/season-1819_csv.csv")
View(football)
#1 cleaning


matrix=matrix(nrow=2*nrow(football), ncol=15,)
colnames(matrix)=c("Team","Round", "GS", "GC", "DG",
                   "YC", "RC", "Corners","Shoots", "FC", "STG", "W", "D", "L", "PPM")

teams=unique(football$HomeTeam)
nrow(football)
View(matrix)
#Teams in order.
i=seq(length(teams),nrow(matrix), length(teams))
j=seq(1,nrow(matrix)-(length(teams)-1),length(teams))

for ( k in 1:length(i)){
  matrix[j[k]:i[k], c("Team")]=teams
  
}


#Round

for ( k in 1:length(i)){
  matrix[j[k]:i[k], c("Round")]=rep(k,length(teams))
  
}


#Get the goals scored per team each round.

matrix=data.frame(matrix, stringsAsFactors=FALSE)

a=c()
b=c()
ca=c()

for ( k in 1:length(teams)){
  a=football[football$HomeTeam==teams[k],c("FTHG", "Date")]
  b=football[football$AwayTeam==teams[k],c("FTAG", "Date")]
  names(a)=names(b)
  ca=rbind(a,b)
  ca=data.frame(ca)
  ca=ca[order(ca[,2], ca[,1]), ]
  matrix[matrix$Team==teams[k], c("GS")]=ca[,1]
}

#Get the goals received per team each round
for ( k in 1:length(teams)){
  a=football[football$HomeTeam==teams[k],c("FTAG", "Date")]
  b=football[football$AwayTeam==teams[k],c("FTHG", "Date")]
  names(a)=names(b)
  ca=rbind(a,b)
  ca=data.frame(ca)
  ca=ca[order(ca[,2], ca[,1]), ]
  matrix[matrix$Team==teams[k], c("GC")]=ca[,1]
}

#get the yellow cards,red cards, corners and faults per team each round.


for ( k in 1:length(teams)){
  a=football[football$HomeTeam==teams[k],c("HY","HR","HC", "HF","HS", "Date")]
  b=football[football$AwayTeam==teams[k],c("AY","AR","AC", "AF","AS", "Date")]
  names(a)=names(b)
  ca=rbind(a,b)
  ca=data.frame(ca)
  ca=ca[order(ca[,6],ca[,5], ca[,4], ca[, 3], ca[, 2], ca[,1]), ]
  matrix[matrix$Team==teams[k], c("YC")]=ca[,1]
  matrix[matrix$Team==teams[k], c("RC")]=ca[,2]
  matrix[matrix$Team==teams[k], c("Corners")]=ca[,3]
  matrix[matrix$Team==teams[k], c("FC")]=ca[,4]
  matrix[matrix$Team==teams[k], c("Shoots")]=ca[,5]
}

for (k in 1:length(teams)){
  a=matrix[matrix$Team==teams[k], c("YC", "RC", "GS", "GC","Corners","Shoots", "FC")]
  for (l in 1:7){
    for (q in 2:26){
      a[, ] = sapply(a[,], as.numeric)
      a[q,l]=a[q-1,l]+a[q,l]
    }
  }
  matrix[matrix$Team==teams[k], c("YC", "RC", "GS", "GC","Corners","Shoots", "FC")]=a
}

#DG
matrix[,-1 ] = sapply(matrix[,-1], as.numeric)
matrix$DG=matrix$GS-matrix$GC

#Win, Draw and losses

for ( k in 1:length(teams)){
  a=football[football$HomeTeam==teams[k],c("FTR", "Date")]
  b=football[football$AwayTeam==teams[k],c("FTR", "Date")]
  for ( h in 1:nrow(a)){
    if (a[h,1]=="H"){
      a[h,1]="W"
    }else{
      if (a[h,1]=="A")
        a[h,1]="L"
    }
  }
  
  for ( h in 1:nrow(b)){
    if (b[h,1]=="A"){
      b[h,1]="W"
    }else{
      if (b[h,1]=="H")
        b[h,1]="L"
    }
  }
  names(a)=names(b)
  ca=rbind(a,b)
  ca=data.frame(ca)
  ca=ca[order(ca[,2], ca[,1]), ]
  matrix[matrix$Team==teams[k], c("W")]=ca[,1]
}


vec=matrix$W
matrix$W=matrix$D=matrix$L=0
for ( q in 1:nrow(matrix)){
  if (vec[q]=="W"){
    matrix[q,c("W")]=1
  }else{
    if (vec[q]=="L"){
      matrix[q, c("L")]=1
    }else{
      if (vec[q]=="D"){
        matrix[q, c("D")]=1
      }
    }
  }
}

for (k in 1:length(teams)){
  a=matrix[matrix$Team==teams[k], c("W", "D", "L")]
  for (l in 1:3){
    for (q in 2:26){
      a[, ] = sapply(a[,], as.numeric)
      a[q,l]=a[q-1,l]+a[q,l]
    }
  }
  matrix[matrix$Team==teams[k], c("W", "D", "L")]=a
}


#STG

matrix$STG=matrix$Shoots/matrix$GS
matrix[which(matrix$STG==Inf), c("STG")]=NA
#PPM
matrix$PPM=(3*matrix$W+1*matrix$D)/matrix$Round

library(ggplot2)
a="Real Madrid"
b="Sevilla"
x=matrix[matrix$Team=="Sevilla", c("Team", "Round", "Shoots")]
n=matrix[matrix$Team=="Barcelona", c("Team", "Round", "Shoots")]
xn=rbind(x,n)
View(xn)
ggplot(xn)+aes(x=Round, y=STG)+
  geom_line(aes(group=xn$Team, colour=xn$Team))

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

data <- matrix[-c(1:501,520:560),]

data$longitude <- ifelse(data$Team == "Betis", "-5.99", 
                         ifelse(data$Team == "Girona", "-2.821111", 
                                ifelse(data$Team == "Barcelona", "-2.183333", 
                                       ifelse(data$Team=="Celta", "-8.712447", 
                                              ifelse(data$Team=="Villarreal", "-0.101389",
                                                     ifelse(data$Team=="Eibar", "-2.466667",
                                                            ifelse(data$Team=="Real Madrid", "-3.716667",
                                                                   ifelse(data$Team=="Ath Bilbao", "-2.923611",
                                                                          ifelse(data$Team=="Valencia", "-0.375",
                                                                                 ifelse(data$Team=="Getafe","-3.731111", 
                                                                                        ifelse(data$Team=="Leganes","-3.764444", 
                                                                                               ifelse(data$Team=="Alaves", "-2.683333", 
                                                                                                      ifelse(data$Team=="Ath Madrid", "-3.716667",
                                                                                                             ifelse(data$Team=="Valladolid", "-4.723611", 
                                                                                                                    ifelse(data$Team=="Espanol", "2.183333",
                                                                                                                           ifelse(data$Team=="Sevilla","-5.99",
                                                                                                                                  ifelse(data$Team=="Levante", "-0.375", 
                                                                                                                                         ifelse(data$Team=="Huesca", "-0.416667", "-1.985556"))))))))))))))))))

data$latitude <- ifelse(data$Team == "Betis", "37.39", 
                        ifelse(data$Team == "Girona", "41.984444", 
                               ifelse(data$Team == "Barcelona", "41.383333", 
                                      ifelse(data$Team=="Celta", "42.231356", 
                                             ifelse(data$Team=="Villarreal", "39.937778",
                                                    ifelse(data$Team=="Eibar", "43.183333",
                                                           ifelse(data$Team=="Real Madrid", "40.383333",
                                                                  ifelse(data$Team=="Ath Bilbao", "43.256944",
                                                                         ifelse(data$Team=="Valencia", "39.466667",
                                                                                ifelse(data$Team=="Getafe","40.304722", 
                                                                                       ifelse(data$Team=="Leganes","40.328056", 
                                                                                              ifelse(data$Team=="Alaves", "42.85", 
                                                                                                     ifelse(data$Team=="Ath Madrid", "40.383333",
                                                                                                            ifelse(data$Team=="Valladolid", "41.652778", 
                                                                                                                   ifelse(data$Team=="Espanol", "41.383333",
                                                                                                                          ifelse(data$Team=="Sevilla","37.39",
                                                                                                                                 ifelse(data$Team=="Levante", "39.466667", 
                                                                                                                                        ifelse(data$Team=="Huesca", "42.133333", "43.321389"))))))))))))))))))
data$longitude = as.numeric(data$longitude)
data$latitude = as.numeric(data$latitude)

data$depth_type <- ifelse(data$STG <= 9, "shallow", ifelse(data$STG <= 15 | data$STG >9, "intermediate", ifelse(data$STG > 15, "deep", "other")))
