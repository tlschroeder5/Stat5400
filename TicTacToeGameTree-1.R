library(data.tree)
root <- Node$new("Game Start")
n=3
Board <- matrix(rep("-",n^2),n,n)
root$Board <- Board
root$GameState
upDateTree(root)

#Function to build the tree

BuildTree <- function(node){
 if(is.null(node$GameState)){
   if((node$level%%2==0 && node$level < n^2 + 1)){
    for(i in which(node$Board=="-")){
     child <- node$AddChild(i)
     nextMove <- node$Board
     nextMove[i] <- "O"
     child$Board <- nextMove
     child$GameState <- CheckOWin(child$Board,child$level)
     upDateTree(child)
     BuildTree(child)
    }
   } else if(node$level < n^2+1){
    for(i in which(node$Board=="-")){
     child <- node$AddChild(i)
     nextMove <- node$Board
     nextMove[i] <- "X"
     child$Board <- nextMove
     child$GameState <- CheckXWin(child$Board,child$level)
     upDateTree(child)
     BuildTree(child)
    }
     }
 } 
}  
  
#Here are the definitions of the functions that check for win states. They take in vecotors of length 9. vector <- character(9)    

CheckXWin <- function(board,l){
  if(xMatch(board)){
      return("X")
  } else if(l==ncol(board)^2+1){
    return("T")
  }
  return(NULL)
}
  
CheckOWin <- function(board,l){
  if(oMatch(board)){
      return("O")
  } else if(l == ncol(board)^2+1){
    return("T")
  }
  return(NULL)
}

#Start at Node updater 

upDateTree <- function(node){
  node$XWinState <- 0
  node$OWinState <- 0
  if(!is.null(node$GameState)){
  if(node$GameState=="X"){
    node$XWinState <- node$XWinState + 1  
    upDateXWinState(node)
  } else if(node$GameState=="O"){
    node$OWinState <- node$OWinState + 1 
    upDateOWinState(node)
  }
  }
}

upDateXWinState <- function(node){
 if(node$level>1){
 node$parent$XWinState <- node$parent$XWinState + 1
 upDateXWinState(node$parent)
 }
}

upDateOWinState <- function(node){
  if(node$level>1){
  node$parent$OWinState <- node$parent$OWinState + 1 
  upDateOWinState(node$parent)
  }
}
#Checks whether or not we have a match for X's 
xMatch = function(M){
  n = ncol(M)
  if(identical(diag(M), rep("X",n)) |identical(M[row(M)==n+1-col(M)],rep("X",n))){
    return(TRUE)
  }
  for (i in 1:n){
    if(identical(M[i,], rep("X",n))){return(TRUE)}
    if(identical(M[,i],rep("X",n))){return(TRUE)}
  }
  return(FALSE)
}
#Checks whether or not we have a match for O's
oMatch = function(M){
  n = ncol(M)
  if(identical(diag(M), rep("O",n)) |identical(M[row(M)==n+1-col(M)],rep("O",n))){
    return(TRUE)
  }
  for (i in 1:n){
    if(identical(M[i,], rep("O",n))){return(TRUE)}
    if(identical(M[,i],rep("O",n))){return(TRUE)}
  }
  return(FALSE)
}
#Computes weights of nodes for X's turn
weightFinderX = function(node){
  return(node$XWinState - node$OWinState)
}

#Computes weights of nodes for O's turn
weightFinderO = function(node){
  return(node$OWinState - node$XWinState)
}

#return a game's GameState (if nonNull)
getGameState = function(node){
  return(node$GameState)
}


#Takes a node and starts a game from that node's "Board". Player "A" is assumed
#to go first, and makes choices by first ordering potential moves (child nodes)
#according to their weight (see weightFinder functions above), and then
#choosing randomly after removing all moves below the ea quantile. Similar
#rules apply for player B. n is an integer representing the side-length of
#of the game board. The expertise values ea and eb must be between 0 and 1. Will print
#moves if output is set to TRUE.
playGame = function(startNode, ea,eb, output = FALSE){
  current = startNode
  game.on = TRUE
  turn = 1
  if(output){cat("GAME START: ","\n")
  print(current$Board)}
  whichPlayer = "A"
  n = ncol(current$Board)
  if(current$level%%2==1){
    whosMove = "X"
  } else {whosMove = "O"}
  #let's play
  while((game.on) & (turn < n^2 +1)){
    
    look.ahead = current$children
    
    if(whosMove == "X"){
      
      if(whichPlayer == "A"){
        #check to see if "A" can win the game
        winning.moves = sapply(current$children,getGameState)
        if("X"%in% winning.moves){
          final.move = Climb(current, sample(names(which(winning.moves == "X")),1))
          game.on = FALSE
          winner = "A"
        } else if("T"%in% winning.moves){
          #see if we can force a tie
          final.move = Climb(current, sample(names(which(winning.moves == "T")),1))
          game.on = FALSE
          winner = "TIE"
        }else {
          weights = sapply(look.ahead,weightFinderX)
          possible.moves = weights[weights>=quantile(weights,c(ea))]
          if(length(possible.moves)>1){
            next.move = Climb(current, sample(names(which(weights == sample(possible.moves,1))),1))
          } else{next.move = Climb(current, names(possible.moves))}
          if(output){cat("MOVE ", turn, "\n")
          print(next.move$Board)}
        }
      } else if(whichPlayer == "B"){
        #check to see if "A" can win the game
        winning.moves = sapply(current$children,getGameState)
        if("X"%in% winning.moves){
          final.move = Climb(current, sample(names(which(winning.moves == "X")),1))
          game.on = FALSE
          winner = "B"
        } else if("T"%in% winning.moves){
          #see if we can force a tie
          final.move = Climb(current, sample(names(which(winning.moves == "T")),1))
          game.on = FALSE
          winner = "TIE"
        }else {
          weights = sapply(look.ahead,weightFinderX)
          possible.moves = weights[weights>=quantile(weights,c(eb))]
          if(length(possible.moves)>1){
            next.move = Climb(current, sample(names(which(weights == sample(possible.moves,1))),1))
          } else{next.move = Climb(current, names(possible.moves))}
          if(output){cat("MOVE ", turn,"\n")
          print(next.move$Board)}
        }
      } else {
        print("ERROR: Not a valid player!")
        return(NULL)
      }
    } else if(whosMove == "O"){
      
      if(whichPlayer == "A"){
        #check to see if "A" can win the game
        winning.moves = sapply(current$children,getGameState)
        if("O"%in% winning.moves){
          final.move = Climb(current, sample(names(which(winning.moves == "O")),1))
          game.on = FALSE
          winner = "A"
        } else if("T"%in% winning.moves){
          #see if we can force a tie
          final.move = Climb(current, sample(names(which(winning.moves == "T")),1))
          game.on = FALSE
          winner = "TIE"
        }else {
          weights = sapply(look.ahead,weightFinderO)
          possible.moves = weights[weights>=quantile(weights,c(ea))]
          if(length(possible.moves)>1){
            next.move = Climb(current, sample(names(which(weights == sample(possible.moves,1))),1))
          } else{next.move = Climb(current, names(possible.moves))}
          if(output){cat("MOVE ", turn, "\n")
          print(next.move$Board)}
        }
      } else if(whichPlayer == "B"){
        #check to see if "A" can win the game
        winning.moves = sapply(current$children,getGameState)
        if("O"%in% winning.moves){
          final.move = Climb(current, sample(names(which(winning.moves == "O")),1))
          game.on = FALSE
          winner = "B"
        } else if("T"%in% winning.moves){
          #see if we can force a tie
          final.move = Climb(current, sample(names(which(winning.moves == "T")),1))
          game.on = FALSE
          winner = "TIE"
        }else {
          weights = sapply(look.ahead,weightFinderO)
          possible.moves = weights[weights>=quantile(weights,c(eb))]
          if(length(possible.moves)>1){
            next.move = Climb(current, sample(names(which(weights == sample(possible.moves,1))),1))
          } else{next.move = Climb(current, names(possible.moves))}
          if(output){cat("MOVE ", turn, "\n")
          print(next.move$Board)}
        }
      } else {
        print("ERROR: Not a valid player!")
        return(NULL)
      }
    }
    current = next.move
    turn = turn +1
    if(whichPlayer == "A"){whichPlayer = "B"} else {whichPlayer = "A"}
    if(whosMove == "X"){whosMove = "O"} else{whosMove = "X"}
  }
  if(output){cat("FINAL MOVE: ","\n")
  print(final.move$Board)
  if(winner !="TIE"){
    cat("The winner is: ", winner, "!","\n")
  } else{print("The game is a tie!")}}
  
  return(list("winner" = winner, "Final.Board" = final.move$Board, "turns" = turn-1))
}

#Plays a match. A match is defined as a sequence of games, ending when one player has won raceTo
#games.

playMatch = function(start, ea, eb, raceTo, match.style = "alternate", output = FALSE){
  game = 1
  match.on = TRUE
  if(!(match.style%in%c("alternate","winFirst","loseFirst"))){
    print("match.style must be one of 'alternate', 'winFirst', or 'loseFirst'.")
    return(NULL)
    }
  #Choose first player randomly
  first = sample(c("A","B"),1)
  aWins = 0
  bWins = 0
  while(match.on){
    if(first == "A"){
      result = playGame(start, ea,eb)
      if(result$winner == "A"){
        aWins = aWins +1
        winner = "A"
      }else if(result$winner == "B"){
        bWins = bWins +1
        winner = "B"
      } else{winner = "TIE"}
      
      }else if(first == "B"){
      result = playGame(start, eb, ea)
      if(result$winner == "A"){
        bWins = bWins +1
        winner = "B"
      }else if(result$winner == "B"){
        aWins = aWins +1
        winner = "A"
      } else{winner = "TIE"}
      
      } else{
        print("Invalid player name.")
        return(NULL)
        }
    #update next player based on match.style
    if((match.style == "alternate") | (winner == "TIE")){
      first = c("A","B")[c("A","B")!=first]
    } else if(match.style == "winFirst"){
      first = winner
    } else if(match.style == "loseFirst"){
      first = c("A","B")[c("A","B")!=winner]
    }
    if(aWins == raceTo){
      matchWinner = "A"
      match.on = FALSE
    }
    if(bWins == raceTo){
      matchWinner = "B"
      match.on = FALSE
    }
    #print results if output = TRUE
    if(output){
      cat("Results of game ", game, ":\n")
      print(result$Final.Board)
      cat("Winner: ", winner, "\n")
    }
    game = game+1
  }
  return(game-1)
}

#USE REPLICATE TO RUN SIMULATIONS

#DATA GENERATORS:

#plays several games at fixed levels of expertise; returns list of winners

manyGames = function(startNode, numGames, ea, eb){
  return(replicate(numGames, playGame(startNode,ea, eb)$winner))
}

#plays several matches at fixed levels of expertise and raceto values

manyMatches = function(startNode, numMatches, ea, eb, raceto, match.style = "alternate"){
  return(replicate(numMatches, playMatch(startNode, ea, eb, raceto, match.style)))
}

#Computes win rate from a character vector of "A"s, "B"s or "TIE"s.

winRateA = function(results){
  return(sum(results == "A")/length(results))
}

winRateB = function(results){
  return(sum(results == "B")/length(results))
}

winRateTie = function(results){
  return(sum(results == "TIE")/length(results))
}

#for use with sapply; assumes startNode is named 'root'

playGamesGivenExpertise = function(v){
  return(manyGames(root, v[1], v[2], v[3]))
}
#for use with apply; can change raceto or matchtype by changing this function
playMatchesGivenExpertise = function(v){
  return(manyMatches(root, v[1],v[2],v[3],5, "loseFirst"))
}

#Generate a matrix of pairs of expertise values (ea, eb). User inputs
#granularity g (a positive integer) which determines how finely to subdivide
#the interval [0,1]. The matrix then generates all pairs obtained by walking through 
#[0,1] in g steps.

generateExpertise = function(g){
  if(g <= 0){
    print("Error: granularity must be an integer larger than 0.")
    return(NULL)
  }
  v = (1/g)*0:g
  expertises = matrix(rep(NA, 2*(g+1)^2), (g+1)^2,2)
  for (i in v){
    expertises[(i*g^2+i*g+1):((i+1/g)*g^2+(i+1/g)*g),1] = rep(i,g+1)
    expertises[(i*g^2+i*g+1):((i+1/g)*g^2+(i+1/g)*g),2] = v
  }
  return(expertises)
}

##SAMPLE CODE TO GENERATE DATA:

#generate matrix of expertise values
expertises = generateExpertise(10)

#generate input matrix (put #of simulations in first column)

games.input = matrix(c(rep(1000,length(M[,1])), M[,1],M[,2]),length(M[,1]),3)

#run 1000 simulations for each pair of expertise values (takes a long time; > 10 mins)
#each column of results represents a 1000-game simulation at a fixed pair of expertise values.

results = apply(games.input, 1, playGamesGivenExpertise)

#compute win/tie rates from the results and store everything in a data.frame

results.df = data.frame(wrA = apply(results,2,winRateA), wrB = apply(results,2,winRateB),tieRate = apply(results,2,winRateTie),ea = games.input[,2], eb = games.input[,3])

#plot data to see relationships between variables

plot(results.df)

#now you can generate linear models for various response variables.

aWins.lm = lm(wrA~ea + eb, data = results.df)
aWins.summary = summary(aWins.lm)

bWins.lm = lm(wrB ~ ea + eb, data = results.df)
bWins.summary = summary(bWins.lm)

ties.lm = lm(tieRate ~ea + eb, data = results.df)
ties.summary = summary(ties.lm)

#now for data on matches.

matches.alt = apply(games.input, 1, playMatchesGivenExpertise)
matches.winner=apply(games.input,1,playMatchesGivenExpertise)
matches.lose = apply(games.input,1,playMatchesGivenExpertise)

matches.alt.df = data.frame(wrA = apply(matches.alt,2,winRateA), wrB = apply(matches.alt,2,winRateB),ea = games.input[,2], eb = games.input[,3])
matches.winner.df = data.frame(wrA = apply(matches.winner,2,winRateA), wrB = apply(matches.winner,2,winRateB),ea = games.input[,2], eb = games.input[,3])
matches.lose.df = data.frame(wrA = apply(matches.lose,2,winRateA), wrB = apply(matches.lose,2,winRateB),ea = games.input[,2], eb = games.input[,3])

matches.alt.turns = apply(games.input,1,playMatchesGivenExpertise)
matches.winner.turns = apply(games.input,1,playMatchesGivenExpertise)
matches.lose.turns = apply(games.input,1,playMatchesGivenExpertise)


replicates.to.df = function(trials){
  n = ncol(trials)
  k = nrow(trials)
  
  df= data.frame(result = rep(NA, n*k), ea = rep(NA,n*k), eb = rep(NA,n*k))
  for(i in 1:n){
    for(j in 1:k){
      df$result[(i-1)*k + j] = trials[j,i]
      df$ea[(i-1)*k+j] = expertises[i,1]
      df$eb[(i-1)*k+j] = expertises[i,2]
    }
  }
  return(df)
}

games.logit.df = replicates.to.df(results)
matches.alt.logit.df = replicates.to.df(matches.alt)
matches.winner.logit.df = replicates.to.df(matches.winner)
matches.lose.logit.df = replicates.to.df(matches.lose)
matches.alt.turns.df = replicates.to.df(matches.alt.turns)
matches.winner.turns.df = replicates.to.df(matches.winner.turns)
matches.lose.turns.df = replicates.to.df(matches.lose.turns)

save(matches.alt.logit.df, file = "matches_alt_logit.RData")
save(matches.winner.logit.df, file = "matches_winner_logit.RData")
save(matches.lose.logit.df, file = "matches_lose_logit.RData")
save(matches.alt.turns.df, file = "matches_alt_turns.RData")
save(matches.winner.turns.df, file = "matches_winner_turns.RData")
save(matches.lose.turns.df, file = "matches_lose_turns.RData")

#use glm with family = binomial(link = "logit") to get logistic regression
#be sure to turn results into factor before modeling

#use multinom from nnet package for multinomial logistic regression.

games.logit.df$result = factor(games.logit.df$result, levels = c("A","TIE","B"), labels = c("A","TIE","B"))
matches.alt.logit.df$result = factor(matches.alt.logit.df$result, levels = c("A","B"), labels = c("A","B"))
matches.winner.logit.df$result = factor(matches.winner.logit.df$result, levels = c("A","B"), labels = c("A","B"))
matches.lose.logit.df$result = factor(matches.lose.logit.df$result, levels = c("A","B"), labels = c("A","B"))

#logistic models
matches.alt.logit.glm = glm(result~ea + eb, family = binomial(link = "logit"), data = matches.alt.logit.df)
matches.alt.logit.summary = summary(matches.alt.logit.glm)
matches.winner.logit.glm = glm(result~ea + eb, family = binomial(link = "logit"), data = matches.winner.logit.df)
matches.winner.logit.summary = summary(matches.winner.logit.glm)
matches.lose.logit.glm = glm(result~ea + eb, family = binomial(link = "logit"), data = matches.lose.logit.df)
matches.lose.logit.summary = summary(matches.lose.logit.glm)

#game winrate models
aWins.lm = lm(wrA~ea + eb, data = results.df)
aWins.summary = summary(aWins.lm)

bWins.lm = lm(wrB ~ ea + eb, data = results.df)
bWins.summary = summary(bWins.lm)

ties.lm = lm(tieRate ~ea + eb, data = results.df)
ties.summary = summary(ties.lm)

#turns models

matches.alt.turns.lm = lm(result ~ea + eb, data = matches.alt.turns.df)
matches.alt.turns.summary = summary(matches.alt.turns.lm)
matches.winner.turns.lm = lm(result ~ea + eb, data = matches.winner.turns.df)
matches.winner.turns.summary = summary(matches.winner.turns.lm)
matches.lose.turns.lm = lm(result ~ea + eb, data = matches.lose.turns.df)
matches.lose.turns.summary = summary(matches.lose.turns.lm)








