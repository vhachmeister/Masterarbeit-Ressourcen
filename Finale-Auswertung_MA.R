#### BEISPIEL-SKRIPT zur Berechnung der UFF1 #######

# Es wird vorrausgesetzt, dass "Conda" oder "Miniconda" installiert ist

# Hier wird eine Conda Umgebung ausgewählt,
# welche python zur Verfügung stellt,
# um die Packages num2words und jiwer entweder
# zu installieren und/oder zu importieren
# 
# Aus der Variable myenvs entsprechenden Index für ein gültiges
# Environment entnehmen und in Zeile 15 in myenvs$name[index]
# eintragen
myenvs = reticulate::conda_list()
envname = myenvs$name[2]
reticulate::use_condaenv(envname, required = TRUE)

# nachfolgende Zeile auskommentieren, falls die Packages
# num2words und jiwer installiert werden müssen
#py_install(envname, packages = c("num2words","jiwer"), pip = TRUE)

# die Pakete für die Vorverarbeitung/Preprocessing importieren und in
# entsprechenden Variablen speichern
num2words <- reticulate::import("num2words")
jiwer <- reticulate::import("jiwer")

### Datensatz einlesen
pfadDatensatz = "~/Desktop/Masterarbeit/Transkripte/Bsp_Datensatz_PNER.csv"
dataFrame <- read.csv(sep = ";", pfadDatensatz, stringsAsFactors=FALSE, encoding = "UTF-8" )

### Funktionen

# Konvertieren der Zahlen in Wörter
for(i in 1:nrow(dataFrame)) {
 
  # Text aus der Spalte "Hypothesentranskript" 
  # und der Reihe mit dem aktuell in "i" 
  # gespeichertem Index in Variable "text" speichern
  text <- dataFrame[i,]$Hypothesentranskript
  
  # suche in dem Text alle für die Zahlen die
  # für die Umwandlung in Frage kommen (z.B. 27, 27., 27.03.2021)
  eintraege = stringr::str_extract_all(text, '(\\d*[.]?)*')
  
  # schaue in der gesamten Liste
  # und wende die Funktion zum Umwandeln des
  # Python-Packages "num2words" auf entsprechenden
  # Eintrag der Liste an
  for (eintrag in eintraege[[1]]) {
    # falls das Element leer ist, gehe zum nächsten Eintrag
    if (eintrag == "") { next } 
    
    # regular expression zum Identifizieren, ob aktueller Eintrag
    # dem Format eines Datums entspricht
    regDatum <- "^(3[01]|[12][0-9]|0?[1-9])\\.(1[012]|0?[1-9])\\.(\\d{2}|\\d{4})$"
    match <- stringr::str_match(eintrag,regDatum)
    
    # Für den Fall, dass ein Datum gefunden wurde
    if(!is.na(match[[1]])) {
      converted = paste(
        paste(num2words$num2words(as.integer(match[1,2]), ordinal = TRUE, lang="de"),"n", sep = ''),
        paste(num2words$num2words(as.integer(match[1,3]), ordinal = TRUE, lang="de"),"n", sep = ''),
        num2words$num2words(as.integer(match[1,4]), ordinal = FALSE, lang="de"),
        sep = " ")
      text = stringr::str_replace(text,eintrag,paste(converted, collapse = ' '))
      next
    }
    # Ist die Zahl des Eintrags ordinal
    if(endsWith(eintrag, '.')) {
      sub_eintrag = substr(eintrag,1,nchar(eintrag)-1)
      int_sub_eintrag <- as.integer(sub_eintrag)
      converted <- num2words$num2words(int_sub_eintrag, to = 'ordinal', lang="de")
      text = stringr::str_replace(text,eintrag,paste(converted,"n", sep = ''))
      next
    }
    converted <- num2words$num2words(as.integer(eintrag), lang="de")
    text = stringr::str_replace(text,eintrag,converted)
    dataFrame[i,"Hypothesentranskript"] <- text
  }
}

### Konfiguration des Paketes "jiwer"
# Jeder Character wird kleingeschrieben,
# Punktuation wird entfernt sowie überflüssige Leerzeichen 
transformation = jiwer$Compose(c(
  jiwer$ToLowerCase(),
  jiwer$RemoveMultipleSpaces(),
  jiwer$RemoveWhiteSpace(replace_by_space=FALSE),
  jiwer$RemovePunctuation()
  ))


# Berechnung der Word Error Rate für 
# Wörter, Personen, Orte und Zahlen
for(i in 1:nrow(dataFrame)) {
  # Berechnung der WER
  if(!is.na(dataFrame[i,"Goldtranskript"]) && !is.na(dataFrame[i,"Hypothesentranskript"]) 
     && "Goldtranskript" %in% names(dataFrame) && "Hypothesentranskript" %in% names(dataFrame)
     ){
    WER = jiwer$wer(dataFrame[i,"Goldtranskript"], dataFrame[i,"Hypothesentranskript"],
          truth_transform=transformation,  hypothesis_transform=transformation
    )
    dataFrame[i,"WER"] <- WER * 100
  }
  # Berechnung der PNER für Personen
  if(!is.na(dataFrame[i,"Personen_Gold"]) && !is.na(dataFrame[i,"Personen_Hypothese"]) 
     && "Personen_Gold" %in% names(dataFrame) && "Personen_Hypothese" %in% names(dataFrame)
     ){
    PNER_Personen = jiwer$wer(dataFrame[i,"Personen_Gold"], dataFrame[i,"Personen_Hypothese"],
            truth_transform=transformation,  hypothesis_transform=transformation
    )
    dataFrame[i,"PNER_Personen"] <- PNER_Personen * 100
  }
  # Berechnung der PNER für Länder und Orte
  if(!is.na(dataFrame[i,"Orte_Gold"]) && !is.na(dataFrame[i,"Orte_Hypothese"])
     && "Orte_Gold" %in% names(dataFrame) && "Orte_Hypothese" %in% names(dataFrame)
     ){
    PNER_Orte = jiwer$wer(dataFrame[i,"Orte_Gold"], dataFrame[i,"Orte_Hypothese"],
                truth_transform=transformation, hypothesis_transform=transformation
    )
    dataFrame[i,"PNER_Orte"] <- PNER_Orte * 100
  }
  # Berechnung der Number Error Rate für Zahlen
  if(!is.na(dataFrame[i,"Zahlen_Gold"]) && !is.na(dataFrame[i,"Zahlen_Hypothese"])
    && "Zahlen_Gold" %in% names(dataFrame) && "Zahlen_Hypothese" %in% names(dataFrame)
    ){
    PNER_Zahlen = jiwer$wer(dataFrame[i,"Zahlen_Gold"], dataFrame[i,"Zahlen_Hypothese"],
                truth_transform=transformation,  hypothesis_transform=transformation
    )
    dataFrame[i,"PNER_Zahlen"] <- PNER_Zahlen * 100
  }
}

# Berechnung des Mittelwertes für
# Wörter, Personen, Orte und Zahlen
# über alle ATS

# Konvertiere alle Error Rate Spalten zum Datentyp "numeric",
# da die Berechnung mit entsprechender Funktion des Mittelwertes dies vorraussetzt
dataFrame$WER <- as.numeric(dataFrame$WER)
dataFrame$PNER_Personen <- as.numeric(dataFrame$PNER_Personen)
dataFrame$PNER_Orte <- as.numeric(dataFrame$PNER_Orte)
dataFrame$PNER_Zahlen <- as.numeric(dataFrame$PNER_Zahlen)

ATS_List <- unique( dataFrame$ATS )
for( ats in ATS_List){
  dataFrame$Mean_WER[dataFrame$ATS == ats] <- with(dataFrame, mean(WER[ATS == ats]))
  dataFrame$Mean_PNER_Personen[dataFrame$ATS == ats] <- with(dataFrame, mean(PNER_Personen[ATS == ats]))
  dataFrame$Mean_PNER_Orte[dataFrame$ATS == ats] <- with(dataFrame, mean(PNER_Orte[ATS == ats]))
  if("PNER_Zahlen" %in% names(dataFrame)){
    dataFrame$Mean_PNER_Zaheln[dataFrame$ATS == ats] <- with(dataFrame, mean(PNER_Zahlen[ATS == ats]))
  }
}

# Berechnung der Vaianzanalyse für die UFF1
results <- aov(WER ~ ATS,data = dataFrame)
summary(results)
pairwise.t.test(dataFrame$WER,dataFrame$ATS, p.adj = "bonf")
