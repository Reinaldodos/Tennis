library(data.table)
require(tidyverse)
library(PlayerRatings)
library(RSelenium)

rD <-
  rsDriver(
    # port = 4567L,
    browser = "chrome",
    version = "latest",
    # chromever = "latest",
    # geckover = "latest",
    # iedrver = NULL,
    # phantomver = "2.1.1",
    verbose = TRUE,
    check = TRUE)

remDr <- rD[["client"]]

url = "https://mon-espace-tennis.fft.fr/"
remDr$navigate(url)
#Connexion au compte
remDr$findElement("css", "#edit-name")$sendKeysToElement(list("Reinaldo"))
remDr$findElement("css", "#edit-pass")$sendKeysToElement(list("DTUPHQpP28"))
remDr$findElement("css", "#edit-submit")$clickElement()

PALMARES_ANNEE = function(URL)
{
  remDr$navigate(URL)
  #Récupérer les résultats

  doc <- readHTMLTable(
    htmlParse(remDr$getPageSource()[[1]]),
    header = TRUE, as.data.frame = TRUE, stringsAsFactors = FALSE
  )
  palmares = data.table(doc[[2]])
  palmares = palmares[Wo == "Non"]
  palmares = subset(palmares,!(palmares$Adversaire %in% DONE$V2))
  palmares = cbind(Joueur$V2[1], palmares)

  toto = remDr$findElements("xpath","//td//a")
  urls = rbindlist(lapply(toto, function(x)
    x$getElementAttribute(attrName = "href")))
  adversaires = cbind(urls, palmares$Adversaire)

  JOUEURS <<- rbind(JOUEURS, adversaires)
  FICHIER <<- rbind(FICHIER, palmares)
}

DONE = NULL
FICHIER = NULL
Joueur = data.table("https://mon-espace-tennis.fft.fr/palmares/1065827", "DOS SANTOS Reinaldo")
# Joueur = data.table("https://mon-espace-tennis.fft.fr/palmares/998110?millesime=2015", "DOS SANTOS Antonio")
JOUEURS = Joueur

while (nrow(JOUEURS) > 0)
{
  Joueur = JOUEURS[1]
  Joueur$V1[1] = strsplit(Joueur$V1[1], "?", fixed = TRUE)[[1]][1]
  try(PALMARES_ANNEE(Joueur$V1[1]), silent = TRUE)
  # try(PALMARES_ANNEE(paste(Joueur$V1[1], "?millesime=2015", sep = "")), silent = TRUE)
  # try(PALMARES_ANNEE(paste(Joueur$V1[1], "?millesime=2014", sep = "")), silent = TRUE)
  # try(PALMARES_ANNEE(paste(Joueur$V1[1], "?millesime=2013", sep = "")), silent = TRUE)

  DONE = unique(rbind(DONE, Joueur))
  JOUEURS$V1 = unlist(lapply(JOUEURS$V1, function(x) strsplit(x, "?", fixed = TRUE)))

  JOUEURS = unique(subset(JOUEURS,!(JOUEURS$V1 %in% DONE$V1)))
  print(c(nrow(FICHIER), Joueur$V2), quote = FALSE)
  if (nrow(FICHIER) > 1000) {break}
}

FICHIER = unique(FICHIER)
# FICHIER = readRDS("Dropbox/Carto & Stats/R/Tennis/FICHIER")
Dates = strptime(FICHIER$Date, format = "%d/%m/%Y")
FICHIER = cbind(FICHIER, Dates)
FICHIER = FICHIER[V1 != ""]
FICHIER = FICHIER[Adversaire != ""]
FICHIER = FICHIER[order(Dates)]
saveRDS(object = FICHIER, file = "Dropbox/Carto & Stats/R/Tennis/FICHIER")

results = subset(FICHIER, select = c("Dates", "V1", "Adversaire", "V/D"))

results$`V/D` = gsub(pattern = "V.", replacement = 1, x = results$`V/D`)
results$`V/D` = gsub(pattern = "D.", replacement = 0, x = results$`V/D`)
results$Dates = as.integer((FICHIER$Dates - min(FICHIER$Dates))/7/24/3600)
results$`V/D` = as.numeric(results$`V/D`)

# Ranking = fide(x = results, init = 1400, sort = TRUE)
Ranking = glicko(x = results, init = c(1500, 350), sort = TRUE)
Classement = data.table(Ranking$ratings)
Top = Classement[Games > 3]
