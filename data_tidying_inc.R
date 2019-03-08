

rm(list=ls())
library(readr)
football <- read_csv("C:/Users/Usuario/Downloads/season-1819_csv.csv")
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
ggplot(Admission, aes(Admission$Chance.Admit)) + geom_density(aes(group=Admission$Research, colour=Admission$Research, fill=Admission$Research), alpha=0.1)  
library(ggplot2)
a="Real Madrid"
b="Sevilla"
x=matrix[matrix$Team==a, c("Team", "Round", "STG")]
n=matrix[matrix$Team==b, c("Team", "Round", "STG")]
xn=rbind(x,n)
ggplot(xn)+aes(x=Round, y=STG)+
  geom_line(aes(group=xn$Team, colour=xn$Team))

#Graficos de series comparando dos equipos que el usuario elija comparando puntos por partido y disparos para hacer un gol.

#Graficos de clusters, el usuario podra elegir los clusters asi como la jornada.
  

