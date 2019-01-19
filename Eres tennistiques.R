library(igraph)
library(RSelenium)
library(rvest)
library(data.table)
library(stringr)
checkForServer(update = TRUE)
# example of commandline passing
startServer(args = c("-port 4455"),
            log = FALSE,
            invisible = FALSE)
remDr <- remoteDriver(browserName = "firefox", port = 4455)
remDr$open()


ERA = function(Players)
{
  Player = Players[[1]]
  appURL = paste(
    "http://www.tennisabstract.com/cgi-bin/player.cgi?p=",
    gsub(" ", "", Player),
    "&f=ACareerqqC0E0i1s00&view=h2h" ,
    sep = ""
  )
  remDr$navigate(appURL)

  doc <- htmlParse(remDr$getPageSource()[[1]])
  doc = readHTMLTable(doc)
  if (is.null(dim(doc$matches)))
  {
    DONE <<- unique(c(DONE, as.list(Player)))
    Players <<- setdiff(Players, DONE)
  }
  else
  {
    toto = as.data.frame(doc$matches, stringsAsFactors = FALSE)
    toto = data.table(subset(toto, select = c(H2Hs, Opponent, W, L)))

    toto$Opponent = substr(toto$Opponent,
                           1,
                           regexpr(pattern = " [", toto$Opponent, fixed = TRUE))
    toto$Opponent = str_trim(toto$Opponent)
    toto = cbind(toto, Player)

    RESULTS <<- c(RESULTS, list(data.table(toto)))
    Players = unique(c(Players, as.list(toto$Opponent)))
    DONE <<- unique(c(DONE, as.list(Player)))
    Players <<- setdiff(Players, DONE)
  }
}

Player = "Andy Murray"
Players = as.list(Player)
DONE = list()
RESULTS <- list()

while (length(Players > 0))
{
  show(c(length(Players), Players[[1]]))
  ERA(Players)
}

RESULTS = rbindlist(RESULTS, use.names = TRUE, fill = FALSE)
remDr$closeall()

RESULTS$W = as.numeric(as.character(RESULTS$W))
RESULTS$H2Hs = as.numeric(as.character(RESULTS$H2Hs))

library(sqldf)
DA = RESULTS

PALMARES = sqldf('
                 select Player, SUM(W) as WINS from DA
                 GROUP BY Player
                 ')

# create the graph
SELECTION = PALMARES[PALMARES$WINS >= 1, ]
DA = subset(
  RESULTS,
  RESULTS$Opponent %in% SELECTION$Player &
    RESULTS$Player %in% SELECTION$Player &
    RESULTS$H2Hs >= 3)



g = graph.data.frame(DA, directed = FALSE)
g = set.edge.attribute(g, "weight", value = edge.attributes(g)$H2Hs)

groupes = cluster_optimal(g, weights = NULL)
plot(groupes,
     g,
     col = membership(groupes),
     mark.groups = communities(groupes))
cliques = communities(groupes)
print(cliques)
max_cliques(g)
