library(httr)

### Konfiguration der Anfrage an IBMs Speech to Text service
apiKey <- "HIER API-KEY EINTRAGEN"
url <- "https://api.eu-gb.speech-to-text.watson.cloud.ibm.com/instances/7f1ed77a-5def-4b90-8484-ba8d60031ccc/v1/recognize"

# Weitere Einstellungsmöglichkeiten
# Komplette Liste der Parameter unter
# https://cloud.ibm.com/apidocs/speech-to-text#recognize
# einzusehen
params = list(
  `timestamps` = 'false',
  `max_alternatives` = '0',
  `model` = 'de-DE_NarrowbandModel'
)

# `Content-Type` abändern, wenn die Audiodatei ein anderes Format (.wav, .flac) hat
headers = c(
  `Content-Type` = 'audio/mp3'
)
data = upload_file('~/Downloads/Probe_Tagesschau_11.12.2020_gekuerzt.mp3')

### Anfrage senden
res <- httr::POST(url = url, httr::add_headers(.headers=headers), 
                  query = params, body = data, 
                  httr::authenticate('apikey', apiKey))

### Antwort in Datei speichern
resText<-content(res,as="text") 
write(resText, file = '~/Downloads/IbmWatsonResponse.txt')


