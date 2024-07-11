setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Desktop/Social Changes/GEO")
library(rio)
library("openxlsx")
library("dplyr")


#IMPORTA I DATABASE NECESSARI A PARTIRE: UNITA' ISTAT, NOMI DEI COMUNI, MERGIA 
# IMPORTA I RISULTATI ELETTORALI
cap_istat = import("italy_cap.xlsx")
comuni_istat = import("italy_cities.xlsx")
comuni_control = import("Control_communes.xlsx")

comuni_istat$comune = toupper(comuni_istat$comune)
comuni_control$Comune = toupper(comuni_control$Comune)


cap_comune = merge(x=cap_istat, y=comuni_istat, by="istat", all.y=TRUE)
cap_comune[cap_comune$comune == "MONTEBELLO IONICO", ]$comune = "MONTEBELLO JONICO"

setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Desktop/Social Changes/Dataset cap and vote")

#DATI GREZZI NON PARSATI 

ris_2022 = import("Camera_Italia_LivComune.txt")

#AGGIUSTA EVENTUALMENTE COMUNI CHE NON CORRISPONDONO ESATTAMENTE COI NOMI 
# (SONO MOLTI DI PIU' MA NON LI HO SGAMATI TUTTI)

ris_2018[ris_2018$COMUNE == "MONTEBELLO IONICO", ]$COMUNE = "MONTEBELLO JONICO"
ris_2018[ris_2018$COMUNE == "CASTELLANIA", ]$COMUNE = "CASTELLANIA COPPI"



#AGGIUSTA LA GRAFIA DEI COMUNI PER FAR FUNZIONARE IL MERGE, QUINDI SCRIVI GLI
#ACCENTI CON GLI APOSTROFI O COMUNQUE CONTROLLA COME SONO SCRITTI IN MODO DA
# SEGUIRE LA STESSA CONVENZIONE

i=1

while(i<=nrow(cap_comune))
{
  cap_comune[i, ]$comune = gsub("È", "E'", cap_comune[i, ]$comune)
  cap_comune[i, ]$comune = gsub("É", "E'", cap_comune[i, ]$comune)
  cap_comune[i, ]$comune = gsub("À", "A'", cap_comune[i, ]$comune)
  cap_comune[i, ]$comune = gsub("Á", "A'", cap_comune[i, ]$comune)
  cap_comune[i, ]$comune = gsub("Ì", "I'", cap_comune[i, ]$comune)
  cap_comune[i, ]$comune = gsub("Ò", "O'", cap_comune[i, ]$comune)
  cap_comune[i, ]$comune = gsub("Ó", "O'", cap_comune[i, ]$comune)
  cap_comune[i, ]$comune = gsub("Ù", "U'", cap_comune[i, ]$comune)
  
  i=i+1
}


#CREA LE COLONNE NEL DATASET DEI COMUNI PER METTERE I VOTI PER OGNI LISTA

a = unique(ris_2022$DESCRLISTA)

a

i=1

while(i<=length(a))
{
  cap_comune[i+10] = NA #setto a meno uno per vedere quante sostituzioni
  #hanno effettivamente successo
  names(cap_comune)[i+10] = a[i]
  
  i=i+1
}


#INSERISCI I VOTI PER OGNI LISTA NELLA RIGA DEL COMUNE CORRISPONDENTE NELLA CELLA
# APPARTENENTE ALLA COLONNA DELLA LISTA NEL NOSTRO DATASET

i=1

while(i<=nrow(ris_2022))
{
  namelist = ris_2022$DESCRLISTA[i]
  cap_comune[cap_comune$comune == ris_2022$COMUNE[i], namelist] = ris_2022$VOTILISTA[i]
  
  i=i+1 
}

#CREA LA COLONNA DEI VOTANTI PER CALCOLARE CORRETTAMENTE LE PERCENTUALI (OPZIONALE)


i=1
cap_comune$votanti = NA
while(i<=nrow(cap_comune))
{
  cap_comune$votanti[i] = sum(cap_comune[i, 11:33], na.rm=T)
  i=i+1
}





#CALCOLO DELLA PERCENTUALE SUL TOTALE DEI VOTANTI

i=1

while(i<ncol(cap_comune)-10)
{
  cap_comune[i+10] = round(cap_comune[i+10]*100/cap_comune["votanti"], digits =2) #setto a meno uno per vedere quante sostituzioni
  #hanno effettivamente successo
  
  i=i+1
  
}

i=1

cap_comune$chivince = NA
cap_comune$grazieachi = NA

while(i<=nrow(cap_comune))
{
  if(cap_comune$votanti[i]>0)
  {
  cap_comune$CAMPOLARGO[i] = cap_comune$`PARTITO DEMOCRATICO - ITALIA DEMOCRATICA E PROGRESSISTA`[i]+cap_comune$`MOVIMENTO 5 STELLE`[i]+cap_comune$`ALLEANZA VERDI E SINISTRA`[i]+cap_comune$`+EUROPA`[i]+cap_comune$`IMPEGNO CIVICO LUIGI DI MAIO - CENTRO DEMOCRATICO`[i]
  
  cap_comune$CDXUNITO[i] = cap_comune$`FRATELLI D'ITALIA CON GIORGIA MELONI`[i]+cap_comune$`LEGA PER SALVINI PREMIER`[i]+ cap_comune$`FORZA ITALIA`[i]+ cap_comune$`NOI MODERATI/LUPI - TOTI - BRUGNARO - UDC`[i]
  
  
  if(cap_comune$CAMPOLARGO[i]>cap_comune$CDXUNITO[i])
  {
   cap_comune$chivince="Campolargo"
   ifelse(cap_comune$`PARTITO DEMOCRATICO - ITALIA DEMOCRATICA E PROGRESSISTA`[i]>cap_comune$`MOVIMENTO 5 STELLE`[i], 
          cap_comune$grazieachi = "PD",
          cap_comune$grazieachi = "M5S")
  }
  if(cap_comune$CAMPOLARGO[i]<cap_comune$CDXUNITO[i])
  {
    cap_comune$chivince="Centrodestra"
    ifelse(cap_comune$`FRATELLI D'ITALIA CON GIORGIA MELONI`[i]>cap_comune$`LEGA PER SALVINI PREMIER`[i],cap_comune$grazieachi = "FDI",cap_comune$grazieachi = "LEGA")
  }
}
  i=i+1
}



cap

ris_2022aus = ris_2022 |> 
  select(COMUNE, COLLUNINOM)

ris_2022aus = unique(ris_2022aus)

i=1

cap_comune$collegiouninominale = NA

while(i<=nrow(cap_comune))
{
  comune = cap_comune[i, ]$comune
  cap_comune$collegiouninominale[i] = ris_2022aus[ris_2022aus$COMUNE == comune, "COLLUNINOM"]
  
  i=i+1
}

#SALVO SOLO LE LISTE CHE MI INTERESSANO

analisi = data.frame(cap_comune$istat, cap_comune$LEGA, cap_comune$`FORZA ITALIA`, cap_comune$`FRATELLI D'ITALIA CON GIORGIA MELONI`, cap_comune$`MOVIMENTO 5 STELLE`, cap_comune$`PARTITO DEMOCRATICO`, cap_comune$`LIBERI E UGUALI`)



#INSERISCO LA quantità DEI VOTANTI UOMINI


i=1

cap_comune$votanti_maschi = -1
while(i<=nrow(ris_2018))
{
  comune = ris_2018$COMUNE[i]
  cap_comune[cap_comune$comune == comune, "votanti_maschi"] = ris_2018$VOTANTI_MASCHI[i]
  
  i=i+1 
}

#INSERISCO LA QUANTITA' DI VOTANTI DONNE

cap_comune$votanti_femmine = cap_comune$votanti - cap_comune$votanti_maschi

####percentuali di votanti maschi e femmine

cap_comune$votanti_maschi = cap_comune$votanti_maschi*100/cap_comune$votanti

cap_comune$votanti_femmine = 100-cap_comune$votanti_maschi

##salva db

analisi = data.frame(cap_comune$istat, cap_comune$LEGA, cap_comune$`FORZA ITALIA`, cap_comune$`FRATELLI D'ITALIA CON GIORGIA MELONI`, cap_comune$`MOVIMENTO 5 STELLE`, cap_comune$`PARTITO DEMOCRATICO`, cap_comune$`LIBERI E UGUALI`, cap_comune$votanti_maschi, cap_comune$votanti_femmine)

############# DATASET
areeint = import("aree interne.xlsx")

data = merge(cap_comune, areeint, by.x="istat", by.y = "PROCOM_N")

data1 = import("ris2018_lezione.RDS")

data2 = cbind(data1, data["FRATELLI D'ITALIA CON GIORGIA MELONI"] ) 

names(data2)[16] = "Fdi"

v = data2$Areeint

data2$Areeint = NULL

data2$areeint = v

saveRDS(data2, "ris2022_lezione.RDS")

write.xlsx(analisi, "prova per mappa elettorale2.xlsx")
