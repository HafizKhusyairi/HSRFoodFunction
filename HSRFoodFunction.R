library(readr)

#loading the four relevant tables from Health Star Rating - Guide For Industry document

VPoints <- read_csv("VPoints.csv",col_names=F)
VPoints = data.frame(VPoints[-1,])
VPointsConcentrated = parse_number(VPoints[,2])
VPointsNonConcentrated = parse_number(VPoints[,3])

BaselinePoints = read_csv("BaselinePoints.csv",col_names = F)
BaselinePoints = data.frame(BaselinePoints[-1,])
BaselineEnergy = parse_number(BaselinePoints[,2])
BaselineSaturatedFat = parse_number(BaselinePoints[,3])
BaselineTotalSugar = parse_number(BaselinePoints[,4])
BaselineSodium = parse_number(BaselinePoints[,5])

PandFPoints = read_csv("PandFPoints.csv",col_names = F)
PandFPoints = data.frame(PandFPoints[-1,])
ProteinPoint = parse_number(PandFPoints[,2])
FibrePoint = parse_number(PandFPoints[,3])

StarTable = read_csv("HSRStar.csv",col_names = F)
Category = data.frame(StarTable[,2])
Star = data.frame(StarTable[,1])

# The HSR Star is based on HSR Point
# The HSR Point is calculated using HSR Baseline and HSR Modifying
# All these points are calculated using lookup tables

HSRBaselinePoints = function(Energy,SatFat,TotalSugar,Sodium){
  sum(Energy>BaselineEnergy,na.rm = T)+sum(SatFat>BaselineSaturatedFat,na.rm = T)+
    sum(TotalSugar>BaselineTotalSugar,na.rm=T)+sum(Sodium>BaselineSodium,na.rm=T)
}

# HSR Modifying Points consist of Vegetable, Protein, and Fibre Points
# HSR V Point is the only one with condition for the lookup

HSRVPoints = function(concentrated, nonconcentrated){
  if(nonconcentrated==0){
    sum(concentrated>=VPointsConcentrated,na.rm = T)
  }
  else if(concentrated==0){
    sum(nonconcentrated>VPointsNonConcentrated,na.rm = T)
  }
  else{
    totalfvnl = (nonconcentrated+2*concentrated)/(nonconcentrated+2*concentrated+(100-concentrated-nonconcentrated))*100
    sum(totalfvnl>VPointsNonConcentrated,na.rm = T)
  }
}


# HSR Score is calculated differently for items with Baseline point >= 13 and V Point <5

HSRFinalScore = function(Energy,SatFat,TotalSugar,Sodium,concentrated, nonconcentrated,Protein,Fibre){ 
  if(HSRBaselinePoints(Energy,SatFat,TotalSugar,Sodium)>=13 & HSRVPoints(concentrated, nonconcentrated)<5){
    HSRBaselinePoints(Energy,SatFat,TotalSugar,Sodium)-HSRVPoints(concentrated, nonconcentrated)-
      sum(Fibre>FibrePoint,na.rm = T)
  }
  else{
    HSRBaselinePoints(Energy,SatFat,TotalSugar,Sodium)-HSRVPoints(concentrated, nonconcentrated)-sum(Protein>ProteinPoint,na.rm = T)-
      sum(Fibre>FibrePoint,na.rm = T)
  }
}


# HSR Star is determined using lookup table

HSRStar = function(Energy,SatFat,TotalSugar,Sodium,concentrated, nonconcentrated,Protein,Fibre){
  index = sum(HSRFinalScore(Energy,SatFat,TotalSugar,Sodium,concentrated, nonconcentrated,Protein,Fibre)<=Category,na.rm=T)
  index = 10-index
  ifelse(index==10,0.5,Star[index,1])
}

