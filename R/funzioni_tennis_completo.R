################################################
#PACCHETTO AGGIORNATO DI FUNZIONI per il TENNIS# 
################################################
## Funzioni tennis per modificare/visualizzare le funzioni che chiama quando fa library(EloMC) ##

#merge

merged_tennis_data <- function(gender = "ATP") {
  # Check if the 'welo' package is installed, and install it if not
  if (!requireNamespace("welo", quietly = TRUE)) {
    install.packages("welo")
  }
  
  # Load the 'welo' package
  library(welo)
  
  # Load datasets for each year using the specified gender
  X2013 <- suppressMessages(suppressWarnings(tennis_data("2013", gender)))
  X2014 <- suppressMessages(suppressWarnings(tennis_data("2014", gender)))
  X2015 <- suppressMessages(suppressWarnings(tennis_data("2015", gender)))
  X2016 <- suppressMessages(suppressWarnings(tennis_data("2016", gender)))
  X2017 <- suppressMessages(suppressWarnings(tennis_data("2017", gender)))
  X2018 <- suppressMessages(suppressWarnings(tennis_data("2018", gender)))
  X2019 <- suppressMessages(suppressWarnings(tennis_data("2019", gender)))
  X2020 <- suppressMessages(suppressWarnings(tennis_data("2020", gender)))
  X2021 <- suppressMessages(suppressWarnings(tennis_data("2021", gender)))
  X2022 <- suppressMessages(suppressWarnings(tennis_data("2022", gender)))
  X2023 <- suppressMessages(suppressWarnings(tennis_data("2023", gender)))
  X2024 <- suppressMessages(suppressWarnings(tennis_data("2024", gender)))
  X2025 <- suppressMessages(suppressWarnings(tennis_data("2025", gender)))
  
  
  
  # Remove specified columns
  X2013 <- subset(X2013, select = -c(SJW, SJL, EXW, EXL, LBW, LBL))
  X2014 <- subset(X2014, select = -c(SJW, SJL, EXW, EXL, LBW, LBL))
  X2015 <- subset(X2015, select = -c(EXW, EXL, LBW, LBL))
  X2016 <- subset(X2016, select = -c(EXW, EXL, LBW, LBL))
  X2017 <- subset(X2017, select = -c(EXW, EXL, LBW, LBL))
  X2018 <- subset(X2018, select = -c(EXW, EXL, LBW, LBL))
  
  # Combine all datasets into one
  X <- rbind(X2013, X2014, X2015, X2016, X2017, X2018, X2019, X2020, X2021, X2022, X2023, X2024, X2025)
  
  print("Download completed.")
  
  return(X)
}


############### FUNZIONE MIO CLEAN ################################################

#Non fa molto, sistema solo il df per poterlo mettere dentro welofit, e cancella le righe dove ci sono dati mancanti

clean_data <- function (x, WELO = FALSE)
{
  start <- Sys.time()
  
  if (!inherits(x, "data.frame"))
    stop("x must be a data.frame. Please provide it in the correct form")
  
  cat("Number of matches", nrow(x), "\n")
  
  if (any(colnames(x) == "Tier")) {
    colnames(x)[which(colnames(x) == "Tier")] <- "Series"
  }
  
  #Rimuovo le righe che hanno valori mancanti nella colonna "ATP" o WTA
  
  if ("ATP" %in% colnames(x) && any(is.na(x[, "ATP"]))) {
    todrop <- which(is.na(x[, "ATP"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because ATP is missing\n")
  } else if ("WTA" %in% colnames(x) && any(is.na(x[, "WTA"]))) {
    todrop <- which(is.na(x[, "WTA"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because WTA is missing\n")
  }
  
  #Rimuovo le righe che hanno valori mancanti nella colonna "Location", "Tournament", "Date", "Series", "Surface" etcetc
  
  if (any(is.na(x[, "Location"]))) {
    todrop <- which(is.na(x[, "Location"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because Location is missing\n")
  }
  
  if (any(is.na(x[, "Tournament"]))) {
    todrop <- which(is.na(x[, "Tournament"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because Tournament is missing\n")
  }
  
  if (any(is.na(x[, "Date"]))) {
    todrop <- which(is.na(x[, "Date"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because Date is missing\n")
  }
  
  if (any(is.na(x[, "Series"]))) {
    todrop <- which(is.na(x[, "Series"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because Series is missing\n")
  }
  
  if (any(is.na(x[, "Surface"]))) {
    todrop <- which(is.na(x[, "Surface"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because Surface is missing\n")
  }
  
  if (any(is.na(x[, "Round"]))) {
    todrop <- which(is.na(x[, "Round"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because Round is missing\n")
  }
  
  if (any(is.na(x[, "Winner"]))) {
    todrop <- which(is.na(x[, "Winner"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because Winner is missing\n")
  }
  
  if (any(is.na(x[, "Loser"]))) {
    todrop <- which(is.na(x[, "Loser"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because Loser is missing\n")
  }
  
  if (any(is.na(x[, "Comment"]))) {
    todrop <- which(is.na(x[, "Comment"]))
    x <- x[-todrop, ]
    cat("Dropping", length(todrop), "matches because comment is missing\n")
  }
  
  res <- data.frame(Location = x$Location,
                    Tournament = x$Tournament,
                    Date = x$Date,
                    Series = x$Series,
                    Surface = x$Surface,
                    Round = x$Round,
                    Winner = x$Winner,
                    Loser = x$Loser,
                    Comment = x$Comment)
  
  if ("ATP" %in% colnames(x)) {
    res$ATP <- x$ATP
  } else if ("WTA" %in% colnames(x)) {
    res$WTA <- x$WTA
  }
  
  
  #Nel caso si vorra mai usare il WELO? Anche solo per il primo turno
  if(WELO){
    if (any(colnames(x) == "W4")) {
      NG_Winner <- rowSums(x[, c("W1", "W2", "W3", "W4", "W5")], na.rm = T)
      NG_Loser <- rowSums(x[, c("L1", "L2", "L3", "L4", "L5")], na.rm = T)
    }
    else {
      NG_Winner <- rowSums(x[, c("W1", "W2", "W3")], na.rm = T)
      NG_Loser <- rowSums(x[, c("L1", "L2", "L3")], na.rm = T)
    }
    NS_Winner <- as.numeric(x$Wsets)
    NS_Loser <- as.numeric(x$Lsets)
    res$f_g_Winner <- NG_Winner/(NG_Winner + NG_Loser)
    res$f_g_Loser <- NG_Loser/(NG_Loser + NG_Winner)
    res$f_s_Winner <- NS_Winner/(NS_Winner + NS_Loser)
    res$f_s_Loser <- NS_Loser/(NS_Winner + NS_Loser)
  }
  
  #Ordina per data, poi per round (1st, 2nd, 3rd, Quar, Semi, The Fin sono in ordine alfabetico)
  res <- res[order(res$Date, res$Round), ]
  
  #Mette la colonna id come prima colonna, per bellezza
  #res$id <- 1:nrow(res)
  #res <- res[ , c("id", names(res)[names(res) != "id"])]
  
  end <- Sys.time()
  cat("clean() took", round(end-start, 2), "seconds to run\n")
  return(res)
}



################################ FUNZIONE WELO FIT ##########################################################

#Fa elo e welo, dovrebbe andare, ho cambiato rispetto alla tesi la s, in modo da fare come il WELO, che ha il "tetto max" che è l'Elo, quindi s è minore di 1.
#Quindi ad esempio nell'Elo_Hard, il cambio rating sarà uguale a quello dell'Elo se la partita è su Hard, senno se il match non è su Hard, il cambio sara moltiplicato per s, quindi vale di meno,
#segue semplicemente la logica del Welo. Poi avevo sbagliato una formula della funzione di perdita. I CI non li ho mai guardati

#Le funzioni di perdita vengono calcolate solo per i match "Completed" e per quella specifica superficie.

compute_elo <- function (x, W = "GAMES", SP = 1500, K = "Kovalchik", s=0.5, CI = FALSE, alpha = 0.05, B = 1000, WELO = FALSE) {
  start <- Sys.time()
  if ((W != "GAMES") & (W != "SETS")) {
    stop(cat("#Warning:\n Valid choices for the parameter 'W' are currently 'GAMES' and 'SETS' \n"))
  }
  
  
  #Startiamo df vuoto
  Elo_df <- data.frame(Elo_Winner_before_match = NA,
                       Elo_Loser_before_match = NA,
                       Elo_Winner_after_match = NA,
                       Elo_Loser_after_match = NA,
                       Elo_pi_hat = NA)
  
  if(WELO) {
    WElo_Winner_before_match <- NA
    WElo_Loser_before_match <- NA
    WElo_Winner_after_match <- NA
    WElo_Loser_after_match <- NA
    WElo_pi_hat <- NA
  }
  
  if (CI == TRUE) {
    Elo_df$Elo_Winner_lb <- NA
    Elo_df$Elo_Winner_ub <- NA
    Elo_df$Elo_Loser_lb <- NA
    Elo_df$Elo_Loser_ub <- NA
    
    if(WELO) {
      Elo_df$WElo_Winner_lb <- NA
      Elo_df$WElo_Winner_ub <- NA
      Elo_df$WElo_Loser_lb <- NA
      Elo_df$WElo_Loser_ub <- NA
    }
  }
  
  #Startiamo un po di vettori
  players_current_elo = c()
  
  if(WELO)
    players_current_welo = c()
  players_played_matches = c()
  
  #Prendiamo i vincitori e perdenti
  for (Row in 1:nrow(x)) {
    Winner <- x$Winner[Row]
    Loser <- x$Loser[Row]
    
    #Se il vincitore non è già presente nell'elenco dei giocatori, inizializza il suo rating Elo con
    #il rating iniziale e imposta il numero di partite giocate a zero. Lo stesso viene fatto per il
    #perdente
    
    if (!(Winner %in% names(players_current_elo))){
      players_current_elo[Winner] <- SP
      if(WELO)
        players_current_welo[Winner] <- SP
      players_played_matches[Winner] <- 0
    }
    
    if (!(Loser %in% names(players_current_elo))){
      players_current_elo[Loser] <- SP
      
      if(WELO)
        players_current_welo[Loser] <- SP
      players_played_matches[Loser] <- 0
    }
    
    #Mette gli elo dell'ultima partita in before match, o quelli default se non ha fatto partite
    #aggiorno il numero di partite giocate dal vincitore.
    
    Elo_df[Row, "Elo_Winner_before_match"] <- players_current_elo[Winner]
    if(WELO)
      Elo_df[Row, "WElo_Winner_before_match"] <- players_current_welo[Winner]
    players_played_matches[Winner] <- players_played_matches[Winner] + 1
    
    Elo_df[Row, "Elo_Loser_before_match"] <- players_current_elo[Loser]
    
    if(WELO)
      Elo_df[Row, "WElo_Loser_before_match"] <- players_current_welo[Loser]
    players_played_matches[Loser] <- players_played_matches[Loser] + 1
    
    #L'Elo andrà calcolato se la partita è finita, altrimenti rimetti quelli di prima visto che la partita non si è giocata, o sbaglio?
    
    #Calcolo probabilità con ELO
    
    Elo_df[Row, "Elo_pi_hat"] <- tennis_prob(players_current_elo[Winner], players_current_elo[Loser])
    
    
    #Calcolo probabilità con WELO
    
    if(WELO)
      Elo_df[Row, "WElo_pi_hat"] <- tennis_prob(players_current_welo[Winner], players_current_welo[Loser])
    
    #Sinceramente ai CI non ci ho guardato perche non mi servono
    
    if (CI) {
      p <- Elo_df[Row, "Elo_pi_hat"]
      q <- 1 - p
      sim_Winner <- sample(0:1, B, replace = T, prob = c(q, p))
      Elo_df[Row, "Elo_Winner_lb"] <- stats::quantile(Elo_df[Row, "Elo_Winner_before_match"] + K_Winner * (sim_Winner - Elo_df[Row, "Elo_pi_hat"]), alpha/2)
      Elo_df[Row, "Elo_Winner_ub"] <- stats::quantile(Elo_df[Row, "Elo_Winner_before_match"] + K_Winner * (sim_Winner - Elo_df[Row, "Elo_pi_hat"]), 1 - alpha/2)
      sim_Loser <- sample(0:1, B, replace = T, prob = c(p, q))
      Elo_df[Row, "Elo_Loser_lb"] <- stats::quantile(Elo_df[Row, "Elo_Loser_before_match"] + K_Loser * (sim_Loser - (1 - Elo_df[Row, "Elo_pi_hat"])), alpha/2)
      Elo_df[Row, "Elo_Loser_ub"] <- stats::quantile(Elo_df[Row, "Elo_Loser_before_match"] + K_Loser * (sim_Loser - (1 - Elo_df[Row, "Elo_pi_hat"])), 1 - alpha/2)
      
      if(WELO) {
        p_w <- Elo_df[Row, "WElo_pi_hat"]
        q_w <- 1 - p_w
        sim_Winner <- sample(0:1, B, replace = T, prob = c(q_w, p_w))
        Elo_df[Row, "WElo_Winner_lb"] <- stats::quantile(Elo_df[Row, "WElo_Winner_before_match"] + K_Winner * (sim_Winner - Elo_df[Row, "WElo_pi_hat"]), alpha/2)
        Elo_df[Row, "WElo_Winner_ub"] <- stats::quantile(Elo_df[Row, "WElo_Winner_before_match"] + K_Winner * (sim_Winner - Elo_df[Row, "WElo_pi_hat"]), 1 - alpha/2)
        sim_Loser <- sample(0:1, B, replace = T, prob = c(p_w, q_w))
        Elo_df[Row, "WElo_Loser_lb"] <- stats::quantile(Elo_df[Row, "WElo_Loser_before_match"] + K_Loser * (sim_Loser - (1 - Elo_df[Row, "WElo_pi_hat"])), alpha/2)
        Elo_df[Row, "WElo_Loser_ub"] <- stats::quantile(Elo_df[Row, "WElo_Loser_before_match"] + K_Loser * (sim_Loser - (1 - Elo_df[Row, "WElo_pi_hat"])), 1 - alpha/2)
      }
    }
    
    
    
    #NEL calcolo dell ELO concorrono solo i Completed... mi sembra una buona idea
    
    if(x[Row, "Comment"] == "Completed"){
      
      surf_multiplier <- 1
      gs <- 1
      m1 <- 1
      atp5 <- 1 
      atp2 <- 1 
      
      if (K == "Surface_Hard" && x[Row, "Surface"] != "Hard") {
        surf_multiplier <- s
      }
      
      if (K == "Surface_Clay" && x[Row, "Surface"] != "Clay") {
        surf_multiplier <- s
      }
      
      if (K == "Surface_Grass" && x[Row, "Surface"] != "Grass") {
        surf_multiplier <- s
      }
      
      if (K == "Grand_Slam" && x[Row, "Series"] != "Grand Slam") {
        gs <- s
      }
      if (K %in% c("Masters 1000", "WTA1000") && !(x[Row, "Series"] %in% c("Masters 1000", "WTA1000"))) {   #Masters 1000 adjusted coefficient, tutte le volte che non è master 1000 pesiamo per s 
        m1 <- s
      }
      if (K %in% c("ATP500", "WTA500") && !(x[Row, "Series"] %in% c("ATP500", "WTA500"))) {  
        atp5 <- s
      }
      if (K %in% c("ATP250", "WTA250") && !(x[Row, "Series"] %in% c("ATP250", "WTA250"))) {  
        atp2 <- s
      }
      
    
      if (K == "Kovalchik") {
        K_Winner <- 250/(players_played_matches[Winner] + 5)^0.4
        K_Loser <- 250/(players_played_matches[Loser] + 5)^0.4
      }
      
      else if (K == "Grand_Slam") {
        K_Winner <- 250/(players_played_matches[Winner] + 5)^0.4 * gs
        K_Loser <- 250/(players_played_matches[Loser] + 5)^0.4 * gs
      }
      else if (K %in% c("Masters 1000", "WTA1000")) {
        K_Winner <- 250/(players_played_matches[Winner] + 5)^0.4 * m1
        K_Loser <- 250/(players_played_matches[Loser] + 5)^0.4 * m1
      }
      
      else if (K == "Surface_Grass" | K == "Surface_Clay" | K == "Surface_Hard") {
        K_Winner <- 250/(players_played_matches[Winner] + 5)^0.4 * surf_multiplier
        K_Loser <- 250/(players_played_matches[Loser] + 5)^0.4 * surf_multiplier
      }
      
      else if (K %in% c("ATP500", "WTA500")) {
      K_Winner <- 250/(players_played_matches[Winner] + 5)^0.4 * atp5
      K_Loser <- 250/(players_played_matches[Loser] + 5)^0.4 * atp5
      }
      else if (K %in% c("ATP250", "WTA250")) {
        K_Winner <- 250/(players_played_matches[Winner] + 5)^0.4 * atp2
        K_Loser <- 250/(players_played_matches[Loser] + 5)^0.4 * atp2
      }
      
      else {
        K_Winner <- K
        K_Loser <- K
      }
      
      #Calcolo ELO after match
      
      Elo_df[Row, "Elo_Winner_after_match"] <- players_current_elo[Winner] + K_Winner * (1 - Elo_df[Row, "Elo_pi_hat"])
      Elo_df[Row, "Elo_Loser_after_match"] <- players_current_elo[Loser] - K_Loser * (1 - Elo_df[Row, "Elo_pi_hat"])
      
      #Calcolo WELO after match
      if(WELO) {
        if (W == "GAMES") {
          Elo_df[Row, "WElo_Winner_after_match"] <- players_current_welo[Winner] + K_Winner * (1 - Elo_df[Row, "WElo_pi_hat"]) * x[Row, "f_g_Winner"]
          Elo_df[Row, "WElo_Loser_after_match"] <- players_current_welo[Loser] - K_Loser * (1 - Elo_df[Row, "WElo_pi_hat"]) * x[Row, "f_g_Winner"] #Qui forse avevo sbagliato la formula --> avevo messo f_g_Loser invee mi sa che ci va f_g_Winner
        } else {
          Elo_df[Row, "WElo_Winner_after_match"] <- players_current_welo[Winner] + K_Winner * (1 - Elo_df[Row, "WElo_pi_hat"]) * x[Row, "f_s_Winner"]
          Elo_df[Row, "WElo_Loser_after_match"] <- players_current_welo[Loser] - K_Loser * (1 - Elo_df[Row, "WElo_pi_hat"]) * x[Row, "f_s_Winner"]
        }
        
      }
      
      players_current_elo[Winner] <- Elo_df[Row, "Elo_Winner_after_match"]
      players_current_elo[Loser] <- Elo_df[Row, "Elo_Loser_after_match"]
      
      if(WELO) {
        players_current_welo[Winner] <- Elo_df[Row, "WElo_Winner_after_match"]
        players_current_welo[Loser] <- Elo_df[Row, "WElo_Loser_after_match"]
      }
    }
    #NB che qui chiudo il grande if la partita è stata completata, cioe questo è quello che succede se la partita non è stata completata
    else {
      #Controllare i match fatti se va bene così, cosa metto se la partita non viene giocata? NA? 0?
      players_played_matches[Winner] <- players_played_matches[Winner] - 1
      players_played_matches[Loser] <- players_played_matches[Loser] - 1
      
      Elo_df[Row, "Elo_Winner_after_match"] <- players_current_elo[Winner]
      Elo_df[Row, "Elo_Loser_after_match"] <- players_current_elo[Loser]
      
      if(WELO) {
        Elo_df[Row, "WElo_Winner_after_match"] <- players_current_welo[Winner]
        Elo_df[Row, "WElo_Loser_after_match"] <- players_current_welo[Loser]
      }
    }
  }
  
  cat("-----------------------------", "\n")
  
  x_sub <- data.frame(Date = x$Date, Tournament = x$Tournament, Series = x$Series, Round = x$Round,
                      Surface = x$Surface, P_i = x$Winner, P_j = x$Loser, Outcome_P_i = 1,
                      Outcome_P_j = 0, Elo_i_before_match = Elo_df$Elo_Winner_before_match,
                      Elo_j_before_match = Elo_df$Elo_Loser_before_match, Elo_pi_hat = Elo_df$Elo_pi_hat,
                      Elo_i_after_match = Elo_df$Elo_Winner_after_match, Elo_j_after_match = Elo_df$Elo_Loser_after_match,
                      Comment = x$Comment)
  
  
  
  if(WELO) {
    x_sub$WElo_i_before_match <- Elo_df$WElo_Winner_before_match
    x_sub$WElo_j_before_match <- Elo_df$WElo_Loser_before_match
    x_sub$WElo_pi_hat <- Elo_df$WElo_pi_hat
    x_sub$WElo_i_after_match <- Elo_df$WElo_Winner_after_match
    x_sub$WElo_j_after_match <- Elo_df$WElo_Loser_after_match
  }
  
  
  
  if(CI) {
    x_sub$Elo_i_before_match_lb <- Elo_df$Elo_Winner_lb
    x_sub$Elo_i_before_match_ub <- Elo_df$Elo_Winner_ub
    x_sub$Elo_j_before_match_lb <- Elo_df$Elo_Loser_lb
    x_sub$Elo_j_before_match_ub <- Elo_df$Elo_Loser_ub
    x_sub$WElo_i_before_match_lb <- Elo_df$WElo_Winner_lb
    x_sub$WElo_i_before_match_ub <- Elo_df$WElo_Winner_ub
    x_sub$WElo_j_before_match_lb <- Elo_df$WElo_Loser_lb
    x_sub$WElo_j_before_match_ub <- Elo_df$WElo_Loser_ub
  }
  
  
  
  #RICORDA: i vincitori sono sempre i giocatori i, i perdenti i giocatori j
  #Tuttavia, la funzione di perdita deve essere calcolata solamente per quelle righe che hanno la colonna Comment == completed
  #Quindi filtriamo il df per solo le partite complete
  
  filtered_data <- x_sub[x_sub$Comment == "Completed", ]
  
  #bisogna fare che in base al valore di K, cambia cosa verra evaluated
  #Superfici
  if (K == "Grand_Slam") {
    filtered_data <- filtered_data[filtered_data$Series == "Grand Slam", ]
  }
  else if (K %in% c("Masters 1000", "WTA1000")) {
    filtered_data <- filtered_data[filtered_data$Series %in% c("Masters 1000", "WTA1000"), ]
  }
  else if (K %in% c("ATP500", "WTA500")) {
    filtered_data <- filtered_data[filtered_data$Series %in% c("ATP500", "WTA500"), ]
  } 
  else if (K %in% c("ATP250", "WTA250")) {
    filtered_data <- filtered_data[filtered_data$Series %in% c("ATP250", "WTA250"), ]
  }
  else if (K == "Surface_Hard") {
    filtered_data <- filtered_data[filtered_data$Surface == "Hard", ]
  }
  
  else if (K == "Surface_Clay") {
    filtered_data <- filtered_data[filtered_data$Surface == "Clay", ]
  }
  
  else if (K == "Surface_Grass") {
    filtered_data <- filtered_data[filtered_data$Surface == "Grass", ]
  }
  
  else {
    filtered_data <- filtered_data
  }
  
  
  loss_e <- cbind(
    mean((filtered_data$Elo_pi_hat - filtered_data$Outcome_P_i)^2),
    mean(-log(filtered_data$Elo_pi_hat)))
  
  #prima avevo fatto il Brier cosi: ( mi sa è sbagliat)
  #ifelse(x_sub$Elo_i_before_match > x_sub$Elo_j_before_match, 1 - x_sub$Elo_pi_hat, x_sub$Elo_pi_hat))^2)
  
  if(WELO) {
    loss_w <- cbind(
      mean((filtered_data$WElo_pi_hat - filtered_data$Outcome_P_i)^2),
      mean(-log(filtered_data$WElo_pi_hat)))
    loss <- rbind(loss_w, loss_e)
  }
  
  else
    loss <- loss_e
  
  colnames(loss) <- c("Brier", "Log-Loss")
  
  if (WELO) {
    rownames(loss) <- c("WElo", "Elo")
  }
  else {
    rownames(loss) <- ("Elo")
  }
  
  #loss <- round(loss, 4)
  
  
  
  res <- list(results = x_sub, matches = paste("Number of matches:", nrow(x), sep = " "),
              period = paste("From", x_sub$Date[1], "to",	x_sub$Date[nrow(x_sub)], sep = " "),
              loss = loss,
              highest_elo = paste("The player with the highest Elo rate, reached on", x_sub$Date[which.max(x_sub$Elo_i_after_match)], "is:", x_sub$P_i[which.max(x_sub$Elo_i_after_match)], sep = " "))
  
  if(WELO)
    res$highest_welo = paste("The player with the highest WElo rate, reached on", x_sub$Date[which.max(x_sub$WElo_i_after_match)], "is:", x_sub$P_i[which.max(x_sub$WElo_i_after_match)], sep = " ")
  
  res$dataset = x
  
  cat(utils::capture.output(res$loss), sep = "\n")
  class(res) <- c("welo")
  end <- Sys.time()
  cat("welofit2() took", round(end-start, 2), "seconds to run\n")
  
  return(res)
}







create_table <- function(df, player_names) {
  
  # Inizializzo un dataframe per i match
  matches <- data.frame(player_i = character(),
                        player_j = character(),
                        Elo_i = numeric(),
                        Elo_j = numeric(),
                        n_partite_i = numeric(),
                        n_partite_j = numeric(),
                        stringsAsFactors = FALSE)
  
  
  for (i in 1:(length(player_names) / 2)) {
    
    # Seleziona i giocatori per il match corrente
    player_i <- player_names[(i - 1) * 2 + 1]
    player_j <- player_names[(i - 1) * 2 + 2]
    
    # Ottieni l'indice dell'ultima partita per il giocatore i
    last_row_i <- tail(which(df$P_i == player_i | df$P_j == player_i), 1)
    # Se il giocatore i non ha partite, imposta l'Elo a 1500
    if (length(last_row_i) == 0) {
      Elo_i <- 1500
    } else {
      # Seleziona l'Elo dopo l'ultima partita del giocatore i
      if (df$P_i[last_row_i] == player_i) {
        Elo_i <- df$Elo_i_after_match[last_row_i]
      } else {
        Elo_i <- df$Elo_j_after_match[last_row_i]
      }
    }
    
    # Ottieni l'indice dell'ultima partita per il giocatore j
    last_row_j <- tail(which(df$P_i == player_j | df$P_j == player_j), 1)
    # Se il giocatore j non ha partite, imposta l'Elo a 1500
    if (length(last_row_j) == 0) {
      Elo_j <- 1500
    } else {
      # Seleziona l'Elo dopo l'ultima partita del giocatore j
      if (df$P_i[last_row_j] == player_j) {
        Elo_j <- df$Elo_i_after_match[last_row_j]
      } else {
        Elo_j <- df$Elo_j_after_match[last_row_j]
      }
    }
    
    # Calcola il numero di partite per il giocatore i
    n_partite_i_sx<- sum(df$P_i == player_i)
    n_partite_i_dx<- sum(df$P_j == player_i)
    n_partite_i <- n_partite_i_sx + n_partite_i_dx
    
    # Calcola il numero di partite per il giocatore j
    n_partite_j_sx<- sum(df$P_i == player_j)
    n_partite_j_dx<- sum(df$P_j == player_j)
    n_partite_j <- n_partite_j_sx + n_partite_j_dx    
    # Aggiungi il match al dataframe dei match
    matches <- rbind(matches, data.frame(player_i = player_i,
                                         player_j = player_j,
                                         Elo_i = Elo_i,
                                         Elo_j = Elo_j,
                                         n_partite_i = n_partite_i,
                                         n_partite_j = n_partite_j))
  }
  
  return(matches)
}
players_adj <- function(x){
  x<- stri_trans_general(x, "Latin-ASCII")
  x[x == "Cerúndolo F."] <- "Cerundolo F." 
  x[x == "Díaz F."] <- "Diaz Acosta F." 
  x[x == "McCabe J."] <- "Mccabe J." 
  x[x == "Zandschulp B."] <- "Van De Zandschulp B." 
  x[x == "Comesaña F."] <- "Comesana F." 
  x[x == "Garín C."] <- "Garin C." 
  x[x == "Perricard G."] <- "Mpetshi G." 
  x[x == "Carreño P."] <- "Carreno Busta P." 
  x[x == "Wild T."] <- "Seyboth Wild T." 
  x[x == "Marozsán F."] <- "Marozsan F." 
  x[x == "Ugo C."] <- "Ugo Carabelli C." 
  x[x == "Machác T."] <- "Machac T." 
  x[x == "Menšík J."] <- "Mensik J."
  x[x == "Struff J."] <- "Struff J.L." 
  x[x == "Carballés R."] <- "Carballes Baena R." 
  x[x == "Báez S.B"] <- "Baez S." 
  x[x == "Martínez P."] <- "Martinez P." 
  x[x == "Davidovich A."] <- "Davidovich Fokina A." 
  x[x == "Minaur A."] <- "De Minaur A." 
  x[x == "Džumhur D."] <- "Dzumhur D." 
  x[x == "Báez S."] <- "Baez S."
  x[x == "Jones J."] <- "Pinnington Jones J." 
  x[x == "Carballes R."] <- "Carballes Baena R." 
  x[x == "Jong J."] <- "De Jong J." 
  
  
  #DONNE:
  x[x == "Bouzas J."] <- "Bouzas Maneiro J." 
  x[x == "Parrizas N."] <- "Parrizas Diaz N." 
  x[x == "Ruse G."] <- "Ruse E.G." 
  x[x == "Maia B."] <- "Haddad Maia B." 
  x[x == "Osorio C."] <- "Osorio Serrano M.C." 
  x[x == "Carle L."] <- "Carle M."   
  
  
  
  
  
  
  
  x
}
############################ FUNZIONE DEFINE TOURNAMENT ######################

"Prende in input il df pulito con la funzione mioclean e con gli elo calcolati. La date da inserire è
  la data in cui la prima partita del torneo viene disputata. Series = (Grand Slam/ Master 1000)"

#Ho aggiunto la possibilità di scegliere se usare l'Elo o il WElo
define_tournament <- function (X, start_date, Tournament = NULL, Serie = "Grand Slam", WELO = FALSE){
print("dentatp diversi")
  # Controllo sulla Serie
  if (!(Serie %in% c("Grand Slam", "Masters 1000", "WTA1000", "ATP500", "ATP250", "WTA250", "WTA500"))) {
    stop("Error: Serie not valid. Choose between: Grand Slam, Masters 1000, WTA1000, ATP500, WTA500, ATP250, WTA250")
  }
  if (is.null(Tournament)) {
    stop("You must provide a tournament name for Serie types other than Grand Slam, Masters 1000, or WTA1000.")
  }
  # Filtraggio del dataframe
  filtered_df <- X[X$Date == start_date & X$Series == Serie & X$Tournament == Tournament, ]
  
  # Controllo sul numero di righe trovate
  if (nrow(filtered_df) == 0) {
    stop("Error: No matches found in the specific Date and Serie for that Tournament")
  }
  
#COMINCIAMO CON I GRAND SLAM CHE INIZIANO AI 64ESIMI
  
  if (Serie == "Grand Slam"){
    
    # Filtraggio del dataframe
    indice_64esimi <- which(X$Date == start_date & X$Series == "Grand Slam")
    inizio_64 <- min(indice_64esimi) #Trovo la riga della prima partita del torneo
    
    #Vorrei trovare le successive 63 righe che hanno First Round scritto nella colonna X$Round
    successive_rows <- c()
    for (i in (inizio_64):nrow(X)) {
      if (X$Round[i] == "1st Round" & X$Series[i] == Serie) {
        successive_rows <- c(successive_rows, i)
        if (length(successive_rows) == 64) {
          break
        }
      }
    }
    primo_turno <- X[successive_rows, ]
    
    if (nrow(primo_turno) != 64) {
      stop("Error: this is not a Grand Slam, or there aren't enough games: maybe you cleaned them")
    }
    
    # Creazione dei vettori
    if (WELO){
      WElo_i.64 <- primo_turno$WElo_i_before_match
      WElo_j.64 <- primo_turno$WElo_j_before_match
    } else {
      Elo_i.64 <- primo_turno$Elo_i_before_match
      Elo_j.64 <- primo_turno$Elo_j_before_match
    }
    
    player_i.64  <- primo_turno$P_i
    player_j.64 <- primo_turno$P_j
    outcome_P_i.64 <- primo_turno$Outcome_P_i
    outcome_P_j.64 <- primo_turno$Outcome_P_j
    
    # Creiamo il set informativo, ovverò tutto ciò che è successo prima dei 64esimi
    set_info.64 <- X[X$Date < start_date, ]
    
    # Vettore risultati per i giocatori del Primo_turno
    n_partite_i.64 <- numeric(nrow(primo_turno))
    n_partite_j.64 <- numeric(nrow(primo_turno))
    
    # Calcolo del numero di partite precedenti per ogni giocatore
    for (t in 1:length(player_i.64)) {
      count_P_i <- sum(set_info.64$P_i == player_i.64[t]) #in questo caso deicidamo di contare tutte le partite, non solo le completed etc, meglio cosi
      count_P_j <- sum(set_info.64$P_j == player_i.64[t])
      n_partite_i.64[t] <- count_P_i + count_P_j
    }
    
    for (t in 1:length(player_j.64)) {
      count_P_i <- sum(set_info.64$P_i == player_j.64[t])
      count_P_j <- sum(set_info.64$P_j == player_j.64[t])
      n_partite_j.64[t] <- count_P_i + count_P_j
    }
    
    
    # Creazione del dataframe per i 64esimi
    if (WELO){
      sessantaquattresimi <- data.frame(player_i.64, player_j.64, WElo_i.64, WElo_j.64, n_partite_i.64, n_partite_j.64, outcome_P_i.64, outcome_P_j.64)
    } else {
      sessantaquattresimi <- data.frame(player_i.64, player_j.64, Elo_i.64, Elo_j.64, n_partite_i.64, n_partite_j.64, outcome_P_i.64, outcome_P_j.64)
    }
    
    ###################################
    
    
    # 32esimi
    
    #Vorrei trovare le successive 32 righe che hanno Second Round scritto nella colonna X$Round
    successive_rows.32 <- c()
    for (i in (inizio_64):nrow(X)) {
      if (X$Round[i] == "2nd Round" & X$Series[i] == Serie) {
        successive_rows.32 <- c(successive_rows.32, i)
        if (length(successive_rows.32) == 32) {
          break
        }
      }
    }
    inizio_32  <- min (successive_rows.32)
    secondo_turno <- X[successive_rows.32, ]
    
    # Creazione dei vettori
    if (WELO){
      WElo_i.32 <- secondo_turno$WElo_i_before_match
      WElo_j.32 <- secondo_turno$WElo_j_before_match
    } else {
      Elo_i.32 <- secondo_turno$Elo_i_before_match
      Elo_j.32 <- secondo_turno$Elo_j_before_match
    }
    player_i.32 <- secondo_turno$P_i
    player_j.32 <- secondo_turno$P_j
    outcome_P_i.32 <- secondo_turno$Outcome_P_i
    outcome_P_j.32 <- secondo_turno$Outcome_P_j
    
    # Creiamo il set informativo aggiornato
    set_info.32 <- rbind(set_info.64, primo_turno)
    
    n_partite_i.32 <- numeric(length(player_i.32))
    n_partite_j.32 <- numeric(length(player_j.32))
    
    # Calcolo del numero di partite precedenti per ogni giocatore
    for (t in 1:length(player_i.32)) {
      count_P_i.32 <- sum(set_info.32$P_i == player_i.32[t])
      count_P_j.32 <- sum(set_info.32$P_j == player_i.32[t])
      n_partite_i.32[t] <- count_P_i.32 + count_P_j.32
    }
    for (t in 1:length(player_j.32)) {
      count_P_i.32 <- sum(set_info.32$P_i == player_j.32[t])
      count_P_j.32 <- sum(set_info.32$P_j == player_j.32[t])
      n_partite_j.32[t] <- count_P_i.32 + count_P_j.32
    }
    
    # Creazione del dataframe per i trentaduesimi
    if (WELO){
      trentaduesimi <- data.frame(player_i.32, player_j.32, WElo_i.32, WElo_j.32, n_partite_i.32, n_partite_j.32, outcome_P_i.32, outcome_P_j.32)
    } else {
      trentaduesimi <- data.frame(player_i.32, player_j.32, Elo_i.32, Elo_j.32, n_partite_i.32, n_partite_j.32, outcome_P_i.32, outcome_P_j.32)
    }
    
  # 16esimi
  
  # Vorrei trovare le successive 16 righe che hanno "3rd Round" scritto nella colonna X$Round
  successive_rows.16 <- c()
  for (i in (inizio_64):nrow(X)) {
    if (X$Round[i] == "3rd Round" & X$Series[i] == Serie) {
      successive_rows.16 <- c(successive_rows.16, i)
      if (length(successive_rows.16) == 16) {
        break
      }
    }
  }
  inizio_16 <- min(successive_rows.16)
  turno_sedicesimi <- X[successive_rows.16, ]
  
  # Creazione dei vettori
  if (WELO){
    WElo_i.16 <- turno_sedicesimi$WElo_i_before_match
    WElo_j.16 <- turno_sedicesimi$WElo_j_before_match
  } else {
    Elo_i.16 <- turno_sedicesimi$Elo_i_before_match
    Elo_j.16 <- turno_sedicesimi$Elo_j_before_match
  }
  player_i.16 <- turno_sedicesimi$P_i
  player_j.16 <- turno_sedicesimi$P_j
  outcome_P_i.16 <- turno_sedicesimi$Outcome_P_i
  outcome_P_j.16 <- turno_sedicesimi$Outcome_P_j
  
  # Creiamo il set informativo aggiornato (fino al secondo turno)
  set_info.16 <- rbind(set_info.32, secondo_turno)
  
  n_partite_i.16 <- numeric(length(player_i.16))
  n_partite_j.16 <- numeric(length(player_j.16))
  
  # Calcolo del numero di partite precedenti per ogni giocatore
  for (t in 1:length(player_i.16)) {
    count_P_i.16 <- sum(set_info.16$P_i == player_i.16[t])
    count_P_j.16 <- sum(set_info.16$P_j == player_i.16[t])
    n_partite_i.16[t] <- count_P_i.16 + count_P_j.16
  }
  for (t in 1:length(player_j.16)) {
    count_P_i.16 <- sum(set_info.16$P_i == player_j.16[t])
    count_P_j.16 <- sum(set_info.16$P_j == player_j.16[t])
    n_partite_j.16[t] <- count_P_i.16 + count_P_j.16
  }
  
  # Creazione del dataframe per i sedicesimi
  if (WELO){
    sedicesimi <- data.frame(player_i.16, player_j.16, WElo_i.16, WElo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
  } else {
    sedicesimi <- data.frame(player_i.16, player_j.16, Elo_i.16, Elo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
  }
  
  # Ottavi
  successive_rows.8 <- c()
  for (i in (inizio_16):nrow(X)) {
    if (X$Round[i] == "4th Round" & X$Series[i] == Serie) {
      successive_rows.8 <- c(successive_rows.8, i)
      if (length(successive_rows.8) == 8) {
        break
      }
    }
  }
  righe_8 <- X[successive_rows.8, ]
  fine_8 <- max(successive_rows.8)
  
  
  # Creazione dei vettori
  if (WELO == TRUE){
    WElo_i.8 <- righe_8$WElo_i_before_match
    WElo_j.8 <- righe_8$WElo_j_before_match
  } else {
    Elo_i.8 <- righe_8$Elo_i_before_match
    Elo_j.8 <- righe_8$Elo_j_before_match
  }
  
  player_i.8 <- righe_8$P_i
  player_j.8 <- righe_8$P_j
  outcome_P_i.8 <- righe_8$Outcome_P_i
  outcome_P_j.8 <- righe_8$Outcome_P_j
  
  # Creiamo il set informativo aggiornato
  set_info.8 <- rbind(set_info.16, turno_sedicesimi)
  
  n_partite_i.8 <- numeric(length(player_i.8))
  n_partite_j.8 <- numeric(length(player_j.8))
  
  # Calcolo del numero di partite precedenti per ogni giocatore
  for (t in 1:length(player_i.8)) {
    count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
    count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
    n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
  }
  
  for (t in 1:length(player_j.8)) {
    count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
    count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
    n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
  }
  
  # Creazione del dataframe per gli Ottavi
  if (WELO == TRUE){
    ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
  } else {
    ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
  }
 }
  
  else if (Serie %in% c("Masters 1000", "WTA1000")) {
  #Prima di tutto, dovremmo cercare di identificare il numero di righe associate al 2nd round, se sono 32 
  #allora si può eseguire il codice già presente, altrimenti bisognerebbe adattarlo al caso con 16 partite
    
    # Punto di partenza: prima riga del torneo
    righe_partita <- which(X$Date == start_date & X$Series == Serie)
    inizio_torneo <- min(righe_partita) 
    
    # Vettore per raccogliere gli indici del 2nd Round
    successive_rows <- c()
    
    # Scorri dalla prima riga del torneo finché non arrivi a "3rd Round"
    for (i in inizio_torneo:nrow(X)) {
      # Fermati se siamo arrivati al 3rd Round, dovrebbe andare bene anche nel caso in cui si giochi una finale/semifinale atp tra 1st e 2nd round 
      if (X$Round[i] == "3rd Round") {
        break
      }
    
      # Se siamo nel 2nd Round, aggiungiamo l'indice
      if (X$Round[i] == "2nd Round") {
        successive_rows <- c(successive_rows, i)
        
        # Fermati se hai raggiunto 32 partite
        if (length(successive_rows) == 32) {
          break
        }
      }
    }
    
    # Controllo se le righe sono 16 o 32
    if (!(length(successive_rows) %in% c(16, 32))) {
      stop("Error: invalid number of matches for the 2nd Round.")
    }
    
    # Ora puoi estrarre i dati
    secondo_turno <- X[successive_rows, ]
    
    if (nrow(secondo_turno) == 32) {
      # Codice per 32 partite
      if (WELO == TRUE){
        WElo_i.32 <- secondo_turno$WElo_i_before_match
        WElo_j.32 <- secondo_turno$WElo_j_before_match
      } else {
        Elo_i.32 <- secondo_turno$Elo_i_before_match
        Elo_j.32 <- secondo_turno$Elo_j_before_match
      }
      player_i.32 <- secondo_turno$P_i
      player_j.32 <- secondo_turno$P_j
      outcome_P_i.32 <- secondo_turno$Outcome_P_i
      outcome_P_j.32 <- secondo_turno$Outcome_P_j
      
      set_info.32 <- X[1:(min(successive_rows) - 1), ] #In questo modo dovremmo prendere tutte le righe precedenti all'inizio del 2nd round 
      
      n_partite_i.32 <- numeric(length(player_i.32))
      n_partite_j.32 <- numeric(length(player_j.32))
      
      for (t in 1:length(player_i.32)) {
        n_partite_i.32[t] <- sum(set_info.32$P_i == player_i.32[t]) + sum(set_info.32$P_j == player_i.32[t])
      }
      for (t in 1:length(player_j.32)) {
        n_partite_j.32[t] <- sum(set_info.32$P_i == player_j.32[t]) + sum(set_info.32$P_j == player_j.32[t])
      }
      
      if (WELO == TRUE) {
        trentaduesimi <- data.frame(player_i.32, player_j.32, WElo_i.32, WElo_j.32, n_partite_i.32, n_partite_j.32, outcome_P_i.32, outcome_P_j.32)
      } else {
        trentaduesimi <- data.frame(player_i.32, player_j.32, Elo_i.32, Elo_j.32, n_partite_i.32, n_partite_j.32, outcome_P_i.32, outcome_P_j.32)
      }
      
      # 16esimi
      
      # Vorrei trovare le successive 16 righe che hanno "3rd Round" scritto nella colonna X$Round
      successive_rows.16 <- c()
      for (i in (inizio_torneo):nrow(X)) {
        if (X$Round[i] == "3rd Round" & X$Series[i] == Serie) {
          successive_rows.16 <- c(successive_rows.16, i)
          if (length(successive_rows.16) == 16) {
            break
          }
        }
      }
      inizio_16 <- min(successive_rows.16)
      turno_sedicesimi <- X[successive_rows.16, ]
      
      # Creazione dei vettori
      if (WELO){
        WElo_i.16 <- turno_sedicesimi$WElo_i_before_match
        WElo_j.16 <- turno_sedicesimi$WElo_j_before_match
      } else {
        Elo_i.16 <- turno_sedicesimi$Elo_i_before_match
        Elo_j.16 <- turno_sedicesimi$Elo_j_before_match
      }
      player_i.16 <- turno_sedicesimi$P_i
      player_j.16 <- turno_sedicesimi$P_j
      outcome_P_i.16 <- turno_sedicesimi$Outcome_P_i
      outcome_P_j.16 <- turno_sedicesimi$Outcome_P_j
      
      # Creiamo il set informativo aggiornato (fino al secondo turno)
      set_info.16 <- rbind(set_info.32, secondo_turno)
      
      n_partite_i.16 <- numeric(length(player_i.16))
      n_partite_j.16 <- numeric(length(player_j.16))
      
      # Calcolo del numero di partite precedenti per ogni giocatore
      for (t in 1:length(player_i.16)) {
        count_P_i.16 <- sum(set_info.16$P_i == player_i.16[t])
        count_P_j.16 <- sum(set_info.16$P_j == player_i.16[t])
        n_partite_i.16[t] <- count_P_i.16 + count_P_j.16
      }
      for (t in 1:length(player_j.16)) {
        count_P_i.16 <- sum(set_info.16$P_i == player_j.16[t])
        count_P_j.16 <- sum(set_info.16$P_j == player_j.16[t])
        n_partite_j.16[t] <- count_P_i.16 + count_P_j.16
      }
      
      # Creazione del dataframe per i sedicesimi
      if (WELO){
        sedicesimi <- data.frame(player_i.16, player_j.16, WElo_i.16, WElo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
      } else {
        sedicesimi <- data.frame(player_i.16, player_j.16, Elo_i.16, Elo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
      }
      
      # Ottavi, le successive otto righe con 4th round nella colonna round 
      successive_rows.8 <- c()
      for (i in (inizio_16):nrow(X)) {
        if (X$Round[i] == "4th Round" & X$Series[i] == Serie) {
          successive_rows.8 <- c(successive_rows.8, i)
          if (length(successive_rows.8) == 8) {
            break
          }
        }
      }
      righe_8 <- X[successive_rows.8, ]
      fine_8 <- max(successive_rows.8)
      
      
      # Creazione dei vettori
      if (WELO == TRUE){
        WElo_i.8 <- righe_8$WElo_i_before_match
        WElo_j.8 <- righe_8$WElo_j_before_match
      } else {
        Elo_i.8 <- righe_8$Elo_i_before_match
        Elo_j.8 <- righe_8$Elo_j_before_match
      }
      
      player_i.8 <- righe_8$P_i
      player_j.8 <- righe_8$P_j
      outcome_P_i.8 <- righe_8$Outcome_P_i
      outcome_P_j.8 <- righe_8$Outcome_P_j
      
      # Creiamo il set informativo aggiornato
      set_info.8 <- rbind(set_info.16, turno_sedicesimi)
      
      n_partite_i.8 <- numeric(length(player_i.8))
      n_partite_j.8 <- numeric(length(player_j.8))
      
      # Calcolo del numero di partite precedenti per ogni giocatore
      for (t in 1:length(player_i.8)) {
        count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
        count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
        n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
      }
      
      for (t in 1:length(player_j.8)) {
        count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
        count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
        n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
      }
      
      # Creazione del dataframe per gli Ottavi
      if (WELO == TRUE){
        ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
      } else {
        ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
      }
    
    }  
    
    else if (nrow(secondo_turno) == 16) {
      
      # Codice per 16 partite
      inizio_16 <- min(successive_rows) #primo indice di riga corrispondente al secondo turno 
      if (WELO == TRUE){
        WElo_i.16 <- secondo_turno$WElo_i_before_match
        WElo_j.16 <- secondo_turno$WElo_j_before_match
      } else {
        Elo_i.16 <- secondo_turno$Elo_i_before_match
        Elo_j.16 <- secondo_turno$Elo_j_before_match
      }
      player_i.16 <- secondo_turno$P_i
      player_j.16 <- secondo_turno$P_j
      outcome_P_i.16 <- secondo_turno$Outcome_P_i
      outcome_P_j.16 <- secondo_turno$Outcome_P_j
      
      set_info.16 <- X[1:(min(successive_rows) - 1), ]
      
      n_partite_i.16 <- numeric(length(player_i.16))
      n_partite_j.16 <- numeric(length(player_j.16))
      
      for (t in 1:length(player_i.16)) {
        n_partite_i.16[t] <- sum(set_info.16$P_i == player_i.16[t]) + sum(set_info.16$P_j == player_i.16[t])
      }
      for (t in 1:length(player_j.16)) {
        n_partite_j.16[t] <- sum(set_info.16$P_i == player_j.16[t]) + sum(set_info.16$P_j == player_j.16[t])
      }
      
      if (WELO == TRUE) {
        sedicesimi <- data.frame(player_i.16, player_j.16, WElo_i.16, WElo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
      } else {
        sedicesimi <- data.frame(player_i.16, player_j.16, Elo_i.16, Elo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
      }
      
      turno_sedicesimi <- secondo_turno #Per uniformare con i casi precedenti, si poteva lasciare secondo_turno e metterlo dopo nell'information set
      
      #Adesso nel caso di 2nd round = sedicesimi sostituisco 4th round con 3rd round nel calcolo degli ottavi
      
      successive_rows.8 <- c()
      for (i in (inizio_16):nrow(X)) {
        if (X$Round[i] == "3rd Round" & X$Series[i] == Serie) {
          successive_rows.8 <- c(successive_rows.8, i)
          if (length(successive_rows.8) == 8) {
            break
          }
        }
      }
      righe_8 <- X[successive_rows.8, ]
      fine_8 <- max(successive_rows.8)
      
      
      # Creazione dei vettori
      if (WELO == TRUE){
        WElo_i.8 <- righe_8$WElo_i_before_match
        WElo_j.8 <- righe_8$WElo_j_before_match
      } else {
        Elo_i.8 <- righe_8$Elo_i_before_match
        Elo_j.8 <- righe_8$Elo_j_before_match
      }
      
      player_i.8 <- righe_8$P_i
      player_j.8 <- righe_8$P_j
      outcome_P_i.8 <- righe_8$Outcome_P_i
      outcome_P_j.8 <- righe_8$Outcome_P_j
      
      # Creiamo il set informativo aggiornato
      set_info.8 <- rbind(set_info.16, turno_sedicesimi)
      
      n_partite_i.8 <- numeric(length(player_i.8))
      n_partite_j.8 <- numeric(length(player_j.8))
      
      # Calcolo del numero di partite precedenti per ogni giocatore
      for (t in 1:length(player_i.8)) {
        count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
        count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
        n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
      }
      
      for (t in 1:length(player_j.8)) {
        count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
        count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
        n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
      }
      
      # Creazione del dataframe per gli Ottavi
      if (WELO == TRUE){
        ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
      } else {
        ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
      }
      
    } else {
      stop("Error: invalid number of matches for the 2nd Round.")
    }
  }
  
  else if (Serie %in% c("ATP500", "WTA500")) {
    #A questo punto bisogna considerare che nella maggior parte dei casi i sedicesimi rappresentano il primo turno, in due casi il secondo turno 
    #Abbiamo bisogno di identificare il numero di partite giocate nel secondo turno, che possono essere o 16 o 8
    righe_torneo <- which(X$Series == Serie & X$Tournament == Tournament & X$Date == start_date)
    inizio_torneo <- min(righe_torneo)
    successive_rows <- c()
    for (i in inizio_torneo:nrow(X)) {
      if (X$Tournament[i] == Tournament & X$Round[i] == "Quarterfinals") {
        break
      }
      if (X$Tournament[i] == Tournament & X$Round[i] == "2nd Round") {
        successive_rows <- c(successive_rows, i)
      }
      if (length(successive_rows) == 16) {
        break
      }
    }
    # Controllo se le righe sono 16 o 8
    if (!(length(successive_rows) %in% c(16, 8))) {
      stop("Error: invalid number of matches for the 2nd Round.")
    }
  
    secondo_turno <- X[successive_rows, ]
    
#Se sono 16 partite nel secondo turno    
    if (nrow(secondo_turno) == 16) {
    
    inizio_16 <- min(successive_rows)
    turno_sedicesimi <- secondo_turno
    
    # Creazione dei vettori
    if (WELO){
      WElo_i.16 <- turno_sedicesimi$WElo_i_before_match
      WElo_j.16 <- turno_sedicesimi$WElo_j_before_match
    } else {
      Elo_i.16 <- turno_sedicesimi$Elo_i_before_match
      Elo_j.16 <- turno_sedicesimi$Elo_j_before_match
    }
    player_i.16 <- turno_sedicesimi$P_i
    player_j.16 <- turno_sedicesimi$P_j
    outcome_P_i.16 <- turno_sedicesimi$Outcome_P_i
    outcome_P_j.16 <- turno_sedicesimi$Outcome_P_j
    
    # Creiamo il set informativo aggiornato (fino al secondo turno)
    set_info.16 <- X[1:(inizio_16 - 1), ]
    
    n_partite_i.16 <- numeric(length(player_i.16))
    n_partite_j.16 <- numeric(length(player_j.16))
    
    # Calcolo del numero di partite precedenti per ogni giocatore
    for (t in 1:length(player_i.16)) {
      count_P_i.16 <- sum(set_info.16$P_i == player_i.16[t])
      count_P_j.16 <- sum(set_info.16$P_j == player_i.16[t])
      n_partite_i.16[t] <- count_P_i.16 + count_P_j.16
    }
    for (t in 1:length(player_j.16)) {
      count_P_i.16 <- sum(set_info.16$P_i == player_j.16[t])
      count_P_j.16 <- sum(set_info.16$P_j == player_j.16[t])
      n_partite_j.16[t] <- count_P_i.16 + count_P_j.16
    }
    
    # Creazione del dataframe per i sedicesimi
    if (WELO){
      sedicesimi <- data.frame(player_i.16, player_j.16, WElo_i.16, WElo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
    } else {
      sedicesimi <- data.frame(player_i.16, player_j.16, Elo_i.16, Elo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
    }
    
    # Ottavi, le successive otto righe 
    successive_rows.8 <- c()
    for (i in (inizio_16):nrow(X)) {
      if (X$Tournament[i] == Tournament & X$Round[i] == "3rd Round" & X$Series[i] == Serie) {
        successive_rows.8 <- c(successive_rows.8, i)
        if (length(successive_rows.8) == 8) {
          break
        }
      }
    }
    righe_8 <- X[successive_rows.8, ]
    fine_8 <- max(successive_rows.8)
    
    
    # Creazione dei vettori
    if (WELO == TRUE){
      WElo_i.8 <- righe_8$WElo_i_before_match
      WElo_j.8 <- righe_8$WElo_j_before_match
    } else {
      Elo_i.8 <- righe_8$Elo_i_before_match
      Elo_j.8 <- righe_8$Elo_j_before_match
    }
    
    player_i.8 <- righe_8$P_i
    player_j.8 <- righe_8$P_j
    outcome_P_i.8 <- righe_8$Outcome_P_i
    outcome_P_j.8 <- righe_8$Outcome_P_j
    
    # Creiamo il set informativo aggiornato
    set_info.8 <- rbind(set_info.16, turno_sedicesimi)
    
    n_partite_i.8 <- numeric(length(player_i.8))
    n_partite_j.8 <- numeric(length(player_j.8))
    
    # Calcolo del numero di partite precedenti per ogni giocatore
    for (t in 1:length(player_i.8)) {
      count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
      count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
      n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
    }
    
    for (t in 1:length(player_j.8)) {
      count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
      count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
      n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
    }
    
    # Creazione del dataframe per gli Ottavi
    if (WELO == TRUE){
      ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
    } else {
      ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
    }
  }
    
    else if (nrow(secondo_turno) == 8) {
      successive_rows.16 <- c()
      for (i in inizio_torneo:nrow(X)) {
        if (X$Tournament[i] == Tournament & X$Series[i] == Serie & X$Round[i] == "1st Round"){
          successive_rows.16 <- c(successive_rows.16, i)
        }
        if (length(successive_rows.16) == 16) {
          break
        } 
      }
      if (!(length(successive_rows.16) == 16)) {
        stop("Invalid number of matches for the first round")
      }
      if (length(successive_rows.16) == 16) {
        print("Indici delle righe selezionate per il primo turno:")
        print(successive_rows.16)
      }
      
      inizio_16 <- min(successive_rows.16)
      turno_sedicesimi <- X[successive_rows.16, ]
      
      # Creazione dei vettori
      if (WELO){
        WElo_i.16 <- turno_sedicesimi$WElo_i_before_match
        WElo_j.16 <- turno_sedicesimi$WElo_j_before_match
      } else {
        Elo_i.16 <- turno_sedicesimi$Elo_i_before_match
        Elo_j.16 <- turno_sedicesimi$Elo_j_before_match
      }
      player_i.16 <- turno_sedicesimi$P_i
      player_j.16 <- turno_sedicesimi$P_j
      outcome_P_i.16 <- turno_sedicesimi$Outcome_P_i
      outcome_P_j.16 <- turno_sedicesimi$Outcome_P_j
      
      # Creiamo il set informativo aggiornato (fino al secondo turno)
      set_info.16 <- X[1:(inizio_16 - 1), ] #Qua potremmo anche mettere X[X$Date < start_date, ]
      
      n_partite_i.16 <- numeric(length(player_i.16))
      n_partite_j.16 <- numeric(length(player_j.16))
      
      # Calcolo del numero di partite precedenti per ogni giocatore
      for (t in 1:length(player_i.16)) {
        count_P_i.16 <- sum(set_info.16$P_i == player_i.16[t])
        count_P_j.16 <- sum(set_info.16$P_j == player_i.16[t])
        n_partite_i.16[t] <- count_P_i.16 + count_P_j.16
      }
      for (t in 1:length(player_j.16)) {
        count_P_i.16 <- sum(set_info.16$P_i == player_j.16[t])
        count_P_j.16 <- sum(set_info.16$P_j == player_j.16[t])
        n_partite_j.16[t] <- count_P_i.16 + count_P_j.16
      }
      
      # Creazione del dataframe per i sedicesimi
      if (WELO){
        sedicesimi <- data.frame(player_i.16, player_j.16, WElo_i.16, WElo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
      } else {
        sedicesimi <- data.frame(player_i.16, player_j.16, Elo_i.16, Elo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
        assign("sedicesimi", sedicesimi, envir = .GlobalEnv)
      }
      
      
      # Ottavi, le successive otto righe con 2nd round nella colonna round 
      successive_rows.8 <- c()
      for (i in (inizio_16):nrow(X)) {
        if (X$Tournament[i] == Tournament & X$Round[i] == "2nd Round" & X$Series[i] == Serie) {
          successive_rows.8 <- c(successive_rows.8, i)
          if (length(successive_rows.8) == 8) {
            print(successive_rows.8)
            break
          }
        }
      }
      righe_8 <- X[successive_rows.8, ]
      fine_8 <- max(successive_rows.8)
      
      
      # Creazione dei vettori
      if (WELO == TRUE){
        WElo_i.8 <- righe_8$WElo_i_before_match
        WElo_j.8 <- righe_8$WElo_j_before_match
      } else {
        Elo_i.8 <- righe_8$Elo_i_before_match
        Elo_j.8 <- righe_8$Elo_j_before_match
      }
      
      player_i.8 <- righe_8$P_i
      player_j.8 <- righe_8$P_j
      outcome_P_i.8 <- righe_8$Outcome_P_i
      outcome_P_j.8 <- righe_8$Outcome_P_j
      
      # Creiamo il set informativo aggiornato
      set_info.8 <- rbind(set_info.16, turno_sedicesimi)
      
      n_partite_i.8 <- numeric(length(player_i.8))
      n_partite_j.8 <- numeric(length(player_j.8))
      
      # Calcolo del numero di partite precedenti per ogni giocatore
      for (t in 1:length(player_i.8)) {
        count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
        count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
        n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
      }
      
      for (t in 1:length(player_j.8)) {
        count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
        count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
        n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
      }
      
      # Creazione del dataframe per gli Ottavi
      if (WELO == TRUE){
        ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
      } else {
        ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
        assign("ottavi", ottavi, envir = .GlobalEnv)
      }
    }
  }
  
  else if(Serie %in% c("ATP250", "WTA250")) {
    #Innanzitutto identifichiamo il numero di righe del primo turno
    righe_torneo <- which(X$Tournament == Tournament & X$Series == Serie & X$Date == start_date)
    inizio_torneo <- min(righe_torneo)
    successive_rows.1 <- c()
    
    for (i in inizio_torneo:nrow(X)) {
      if (X$Tournament[i] == Tournament & X$Series[i] == Serie & X$Round[i] == "Quarterfinals") {
        break
      }
      if (X$Tournament[i] == Tournament & X$Series[i] == Serie & X$Round[i] == "1st Round") {
        successive_rows.1 <- c(successive_rows.1, i)
      }
    }
    if (!(length(successive_rows.1) %in% c(16, 12))) {
      stop("Invalid number of matches for the first round")
    }
    primo_turno <- X[successive_rows.1, ]
    #Ora possiamo estrarre i dati
    
    if (nrow(primo_turno) == 12) {
      #Partiamo dal secondo turno, ci saranno solo otto partite
      successive_rows.2 <- c()
      for (i in inizio_torneo:nrow(X)){
        if (X$Tournament[i] == Tournament & X$Series[i] == Serie & X$Round[i] == "2nd Round") {
          successive_rows.2 <- c(successive_rows.2, i)
        }
        if (length(successive_rows.2) == 8) {
          break
        }
      }
      if (!(length(successive_rows.2) == 8)) {
        stop("Invalid number of matches for the second round")
      }

      righe_8 <- X[successive_rows.2, ]
      inizio_ottavi <- min(successive_rows.2)
      fine_8 <- max(successive_rows.2)
      
      
      # Creazione dei vettori
      if (WELO == TRUE){
        WElo_i.8 <- righe_8$WElo_i_before_match
        WElo_j.8 <- righe_8$WElo_j_before_match
      } else {
        Elo_i.8 <- righe_8$Elo_i_before_match
        Elo_j.8 <- righe_8$Elo_j_before_match
      }
      
      player_i.8 <- righe_8$P_i
      player_j.8 <- righe_8$P_j
      outcome_P_i.8 <- righe_8$Outcome_P_i
      outcome_P_j.8 <- righe_8$Outcome_P_j
      
      # Creiamo il set informativo aggiornato
      set_info.8 <- X[1:(inizio_ottavi - 1), ]
      
      n_partite_i.8 <- numeric(length(player_i.8))
      n_partite_j.8 <- numeric(length(player_j.8))
      
      # Calcolo del numero di partite precedenti per ogni giocatore
      for (t in 1:length(player_i.8)) {
        count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
        count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
        n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
      }
      
      for (t in 1:length(player_j.8)) {
        count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
        count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
        n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
      }
      
      # Creazione del dataframe per gli Ottavi
      if (WELO == TRUE){
        ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
      } else {
        ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
      }
    }
    
    else if (nrow(primo_turno) == 16) {
      #Dobbiamo capire di che torneo si tratta, potrebbe essere che i draws sono 48 quindi nel secondo turno abbiamo
      #sempre 16 partite oppure sono 32 e in questo caso non ci sono byes e nel secondo turno ci sono 8 partite
      successive_rows.2 <- c()
      for (i in inizio_torneo:nrow(X)) {
        if (X$Tournament[i] == Tournament & X$Series[i] == Serie & X$Round[i] == "Quarterfinals") {
          break
        }
        if (X$Tournament[i] == Tournament & X$Series[i] == Serie & X$Round[i] == "2nd Round"){
          successive_rows.2 <- c(successive_rows.2, i)
        }
        if (length(successive_rows.2) == 16)
          break
      }
      if (!(length(successive_rows.2) %in% c(16, 8))) {
        stop("Invalid number of matches for the second round")
      }
      secondo_turno <- X[successive_rows.2, ]
      #A questo punto
      if (nrow(secondo_turno) == 8) {
        turno_sedicesimi <- primo_turno
        inizio_16 = min(successive_rows.1)
        # Creazione dei vettori
        if (WELO){
          WElo_i.16 <- turno_sedicesimi$WElo_i_before_match
          WElo_j.16 <- turno_sedicesimi$WElo_j_before_match
        } else {
          Elo_i.16 <- turno_sedicesimi$Elo_i_before_match
          Elo_j.16 <- turno_sedicesimi$Elo_j_before_match
        }
        player_i.16 <- turno_sedicesimi$P_i
        player_j.16 <- turno_sedicesimi$P_j
        outcome_P_i.16 <- turno_sedicesimi$Outcome_P_i
        outcome_P_j.16 <- turno_sedicesimi$Outcome_P_j
        
        # Creiamo il set informativo aggiornato (fino al secondo turno)
        set_info.16 <- X[1:(inizio_16 - 1), ]
        
        n_partite_i.16 <- numeric(length(player_i.16))
        n_partite_j.16 <- numeric(length(player_j.16))
        
        # Calcolo del numero di partite precedenti per ogni giocatore
        for (t in 1:length(player_i.16)) {
          count_P_i.16 <- sum(set_info.16$P_i == player_i.16[t])
          count_P_j.16 <- sum(set_info.16$P_j == player_i.16[t])
          n_partite_i.16[t] <- count_P_i.16 + count_P_j.16
        }
        for (t in 1:length(player_j.16)) {
          count_P_i.16 <- sum(set_info.16$P_i == player_j.16[t])
          count_P_j.16 <- sum(set_info.16$P_j == player_j.16[t])
          n_partite_j.16[t] <- count_P_i.16 + count_P_j.16
        }
        
        # Creazione del dataframe per i sedicesimi
        if (WELO){
          sedicesimi <- data.frame(player_i.16, player_j.16, WElo_i.16, WElo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
        } else {
          sedicesimi <- data.frame(player_i.16, player_j.16, Elo_i.16, Elo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
        }
        
        # Ottavi, le successive otto righe 
        
        righe_8 <- secondo_turno
        fine_8 <- max(successive_rows.2)
        
        
        # Creazione dei vettori
        if (WELO == TRUE){
          WElo_i.8 <- righe_8$WElo_i_before_match
          WElo_j.8 <- righe_8$WElo_j_before_match
        } else {
          Elo_i.8 <- righe_8$Elo_i_before_match
          Elo_j.8 <- righe_8$Elo_j_before_match
        }
        
        player_i.8 <- righe_8$P_i
        player_j.8 <- righe_8$P_j
        outcome_P_i.8 <- righe_8$Outcome_P_i
        outcome_P_j.8 <- righe_8$Outcome_P_j
        
        # Creiamo il set informativo aggiornato
        set_info.8 <- rbind(set_info.16, turno_sedicesimi)
        
        n_partite_i.8 <- numeric(length(player_i.8))
        n_partite_j.8 <- numeric(length(player_j.8))
        
        # Calcolo del numero di partite precedenti per ogni giocatore
        for (t in 1:length(player_i.8)) {
          count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
          count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
          n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
        }
        
        for (t in 1:length(player_j.8)) {
          count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
          count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
          n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
        }
        
        # Creazione del dataframe per gli Ottavi
        if (WELO == TRUE){
          ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
        } else {
          ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
        }
      } 
      
      else if (nrow(secondo_turno) == 16) {
        #partiamo dal secondo turno
        turno_sedicesimi <-secondo_turno
        inizio_16 = min(successive_rows.2)
        # Creazione dei vettori
        if (WELO){
          WElo_i.16 <- turno_sedicesimi$WElo_i_before_match
          WElo_j.16 <- turno_sedicesimi$WElo_j_before_match
        } else {
          Elo_i.16 <- turno_sedicesimi$Elo_i_before_match
          Elo_j.16 <- turno_sedicesimi$Elo_j_before_match
        }
        player_i.16 <- turno_sedicesimi$P_i
        player_j.16 <- turno_sedicesimi$P_j
        outcome_P_i.16 <- turno_sedicesimi$Outcome_P_i
        outcome_P_j.16 <- turno_sedicesimi$Outcome_P_j
        
        # Creiamo il set informativo aggiornato (fino al secondo turno)
        set_info.16 <- X[1:(inizio_16 - 1), ]
        
        n_partite_i.16 <- numeric(length(player_i.16))
        n_partite_j.16 <- numeric(length(player_j.16))
        
        # Calcolo del numero di partite precedenti per ogni giocatore
        for (t in 1:length(player_i.16)) {
          count_P_i.16 <- sum(set_info.16$P_i == player_i.16[t])
          count_P_j.16 <- sum(set_info.16$P_j == player_i.16[t])
          n_partite_i.16[t] <- count_P_i.16 + count_P_j.16
        }
        for (t in 1:length(player_j.16)) {
          count_P_i.16 <- sum(set_info.16$P_i == player_j.16[t])
          count_P_j.16 <- sum(set_info.16$P_j == player_j.16[t])
          n_partite_j.16[t] <- count_P_i.16 + count_P_j.16
        }
        
        # Creazione del dataframe per i sedicesimi
        if (WELO){
          sedicesimi <- data.frame(player_i.16, player_j.16, WElo_i.16, WElo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
        } else {
          sedicesimi <- data.frame(player_i.16, player_j.16, Elo_i.16, Elo_j.16, n_partite_i.16, n_partite_j.16, outcome_P_i.16, outcome_P_j.16)
        }
        
        # Ottavi, le successive otto righe 
        successive_rows.8 <- c()
        for (i in (inizio_16):nrow(X)) {
          if (X$Tournament[i] == Tournament & X$Round[i] == "3rd Round" & X$Series[i] == Serie) {
            successive_rows.8 <- c(successive_rows.8, i)
            if (length(successive_rows.8) == 8) {
              break
            }
          }
        }
        righe_8 <- X[successive_rows.8, ]
        fine_8 <- max(successive_rows.8)
        
        
        # Creazione dei vettori
        if (WELO == TRUE){
          WElo_i.8 <- righe_8$WElo_i_before_match
          WElo_j.8 <- righe_8$WElo_j_before_match
        } else {
          Elo_i.8 <- righe_8$Elo_i_before_match
          Elo_j.8 <- righe_8$Elo_j_before_match
        }
        
        player_i.8 <- righe_8$P_i
        player_j.8 <- righe_8$P_j
        outcome_P_i.8 <- righe_8$Outcome_P_i
        outcome_P_j.8 <- righe_8$Outcome_P_j
        
        # Creiamo il set informativo aggiornato
        set_info.8 <- rbind(set_info.16, turno_sedicesimi)
        
        n_partite_i.8 <- numeric(length(player_i.8))
        n_partite_j.8 <- numeric(length(player_j.8))
        
        # Calcolo del numero di partite precedenti per ogni giocatore
        for (t in 1:length(player_i.8)) {
          count_P_i.8 <- sum(set_info.8$P_i == player_i.8[t])
          count_P_j.8 <- sum(set_info.8$P_j == player_i.8[t])
          n_partite_i.8[t] <- count_P_i.8 + count_P_j.8
        }
        
        for (t in 1:length(player_j.8)) {
          count_P_i.8 <- sum(set_info.8$P_i == player_j.8[t])
          count_P_j.8 <- sum(set_info.8$P_j == player_j.8[t])
          n_partite_j.8[t] <- count_P_i.8 + count_P_j.8
        }
        
        # Creazione del dataframe per gli Ottavi
        if (WELO == TRUE){
          ottavi <- data.frame(player_i.8, player_j.8, WElo_i.8, WElo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
        } else {
          ottavi <- data.frame(player_i.8, player_j.8, Elo_i.8, Elo_j.8, n_partite_i.8, n_partite_j.8, outcome_P_i.8, outcome_P_j.8)
        }
      }
      }
        
      }
  
    
    
    
  ########################################################
  # Quarti
  successive_rows.4 <- c()
  for (i in (fine_8):nrow(X)) {
    if (X$Round[i] == "Quarterfinals" & X$Tournament[i] == Tournament & X$Series[i] == Serie) {
      successive_rows.4 <- c(successive_rows.4, i)
      if (length(successive_rows.4) == 4) {
        break
      }
    }
  }
  righe_4 <- X[successive_rows.4, ]
  fine_4 <- max(successive_rows.4)
  
  
  # Creazione dei vettori
  if (WELO == TRUE){
    WElo_i.4 <- righe_4$WElo_i_before_match
    WElo_j.4 <- righe_4$WElo_j_before_match
  } else {
    Elo_i.4 <- righe_4$Elo_i_before_match
    Elo_j.4 <- righe_4$Elo_j_before_match
  }
  
  player_i.4 <- righe_4$P_i
  player_j.4 <- righe_4$P_j
  outcome_P_i.4 <- righe_4$Outcome_P_i
  outcome_P_j.4 <- righe_4$Outcome_P_j
  
  # Creiamo il set informativo aggiornato
  set_info.4 <- rbind(set_info.8, righe_8)
  
  n_partite_i.4 <- numeric(length(player_i.4))
  n_partite_j.4 <- numeric(length(player_j.4))
  
  # Calcolo del numero di partite precedenti per ogni giocatore
  for (t in 1:length(player_i.4)) {
    count_P_i.4 <- sum(set_info.4$P_i == player_i.4[t])
    count_P_j.4 <- sum(set_info.4$P_j == player_i.4[t])
    n_partite_i.4[t] <- count_P_i.4 + count_P_j.4
  }
  
  for (t in 1:length(player_j.4)) {
    count_P_i.4 <- sum(set_info.4$P_i == player_j.4[t])
    count_P_j.4 <- sum(set_info.4$P_j == player_j.4[t])
    n_partite_j.4[t] <- count_P_i.4 + count_P_j.4
  }
  
  
  # Creazione del dataframe per i Quarti
  if (WELO == TRUE){
    quarti <- data.frame(player_i.4, player_j.4, WElo_i.4, WElo_j.4, n_partite_i.4, n_partite_j.4, outcome_P_i.4, outcome_P_j.4)
  } else {
    quarti <- data.frame(player_i.4, player_j.4, Elo_i.4, Elo_j.4, n_partite_i.4, n_partite_j.4, outcome_P_i.4, outcome_P_j.4)
  }
  
  ########################################################
  # Semi-finals
  successive_rows.2 <- c()
  for (i in (fine_4):nrow(X)) {
    if (X$Round[i] == "Semifinals" & X$Tournament[i] == Tournament & X$Series[i] == Serie) {
      successive_rows.2 <- c(successive_rows.2, i)
      if (length(successive_rows.2) == 2) {
        break
      }
    }
  }
  righe_2 <- X[successive_rows.2, ]
  fine_2 <- max(successive_rows.2)
  
  
  
  # Creazione dei vettori
  if (WELO == TRUE){
    WElo_i.2 <- righe_2$WElo_i_before_match
    WElo_j.2 <- righe_2$WElo_j_before_match
  } else {
    Elo_i.2 <- righe_2$Elo_i_before_match
    Elo_j.2 <- righe_2$Elo_j_before_match
  }
  player_i.2 <- righe_2$P_i
  player_j.2 <- righe_2$P_j
  outcome_P_i.2 <- righe_2$Outcome_P_i
  outcome_P_j.2 <- righe_2$Outcome_P_j
  
  # Creiamo il set informativo aggiornato
  set_info.2 <- rbind(set_info.4, righe_4)
  
  n_partite_i.2 <- numeric(length(player_i.2))
  n_partite_j.2 <- numeric(length(player_j.2))
  
  # Calcolo del numero di partite precedenti per ogni giocatore
  for (t in 1:length(player_i.2)) {
    count_P_i.2 <- sum(set_info.2$P_i == player_i.2[t])
    count_P_j.2 <- sum(set_info.2$P_j == player_i.2[t])
    n_partite_i.2[t] <- count_P_i.2 + count_P_j.2
  }
  
  for (t in 1:length(player_j.2)) {
    count_P_i.2 <- sum(set_info.2$P_i == player_j.2[t])
    count_P_j.2 <- sum(set_info.2$P_j == player_j.2[t])
    n_partite_j.2[t] <- count_P_i.2 + count_P_j.2
  }
  
  
  # Creazione del dataframe per le Semi-finals
  if (WELO == TRUE){
    semifinali <- data.frame(player_i.2, player_j.2, WElo_i.2, WElo_j.2, n_partite_i.2, n_partite_j.2, outcome_P_i.2, outcome_P_j.2)
  } else {
    semifinali <- data.frame(player_i.2, player_j.2, Elo_i.2, Elo_j.2, n_partite_i.2, n_partite_j.2, outcome_P_i.2, outcome_P_j.2)
  }
  
  
  
  
  #SECONDA PARTE:
  ######################################################################################################àà
  
  # ORDINAMENTO TABELLONE
  results.4 <- c()
  for (i in 1:2) {
    # Andiamo a trovare quale dei due player di riga dei quarti(player i o j), ha una corrispondenza nelle semifinali
    #Cosi troviamo il vincitore.  Di fatto lo facciamo in ordine e segnamo il nome delle righe,
    #Cosi avremo un vettore di indici da usare per ordinare i quarti
    
    results.4 <- c(results.4,
                   which(quarti$player_i.4 == semifinali$player_i.2[i] | quarti$player_j.4 == semifinali$player_i.2[i]),
                   which(quarti$player_i.4 == semifinali$player_j.2[i] | quarti$player_j.4 == semifinali$player_j.2[i])
    )
  }
  
  quarti_ord <- quarti[results.4, ]
  
  #Reset indici di riga disordinati
  rownames(quarti_ord) <- NULL
  
  #################################################
  # OTTAVI
  results.8 <- c()
  
  # STessa roba:
  "Nel ciclo for,
  Data una riga dei quarti ordinati (un match tra A e B), trovi di fatto le righe in cui A ha giocato il
  suo ottavo e B il suo, poi le metti vicino.
  "
  for (i in 1:4) {
    results.8 <- c(results.8,
                   which(ottavi$player_i.8 == quarti_ord$player_i.4[i] | ottavi$player_j.8 == quarti_ord$player_i.4[i]),
                   which(ottavi$player_i.8 == quarti_ord$player_j.4[i] | ottavi$player_j.8 == quarti_ord$player_j.4[i])
    )
  }
  
  ottavi_ord <- ottavi[results.8, ]
  
  rownames(ottavi_ord) <- NULL

  if (Serie %in% c("ATP250", "WTA250") && nrow(primo_turno) == 12) {
    return_list <- list()
    return_list <- c(return_list,
                     list(ottavi_ord = ottavi_ord,
                          quarti_ord = quarti_ord,
                          semifinali = semifinali))
    
    return(return_list)
  }
  # Sempre stesso:
  " In che righe, coloro che giocheranno gli ottavi, hanno giocato i 16esimi?"
  # Chi ha giocato contro viene messo vicino
  
  results.16 <- c()
  
  for (i in 1:8) {
    results.16 <- c(results.16,
                    which(sedicesimi$player_i.16 == ottavi_ord$player_i.8[i] | sedicesimi$player_j.16 == ottavi_ord$player_i.8[i]),
                    which(sedicesimi$player_i.16 == ottavi_ord$player_j.8[i] | sedicesimi$player_j.16 == ottavi_ord$player_j.8[i])
    )
  }
  
  sedicesimi_ord <- sedicesimi[results.16, ]
  rownames(sedicesimi_ord) <- NULL

  if ((Serie %in% c("Masters 1000", "WTA1000") && nrow(secondo_turno) == 16) ||
      (Serie %in% c("ATP500", "WTA500", "ATP250", "WTA250"))) {
    
    return_list <- list()
    return_list <- c(return_list,
                     list(sedicesimi_ord = sedicesimi_ord,
                          ottavi_ord = ottavi_ord,
                          quarti_ord = quarti_ord,
                          semifinali = semifinali))
    
    return(return_list)
  }
  
  #Da questo momento in poi non modifico più niente. 
  
  results.32 <- c()
  
  for (i in 1:16) {
    results.32 <- c(results.32,
                    which(trentaduesimi$player_i.32 == sedicesimi_ord$player_i.16[i] | trentaduesimi$player_j.32 == sedicesimi_ord$player_i.16[i]),
                    which(trentaduesimi$player_i.32 == sedicesimi_ord$player_j.16[i] | trentaduesimi$player_j.32 == sedicesimi_ord$player_j.16[i])
    )
  }
  
  trentaduesimi_ord <- trentaduesimi[results.32, ]
  
  rownames(trentaduesimi_ord) <- NULL
  
  
  
  if (Serie == "Grand Slam"){
    results.64 <- c()
    
    for (i in 1:32) {
      results.64 <- c(results.64,
                      which(sessantaquattresimi$player_i.64 == trentaduesimi_ord$player_i.32[i] | sessantaquattresimi$player_j.64 == trentaduesimi_ord$player_i.32[i]),
                      which(sessantaquattresimi$player_i.64 == trentaduesimi_ord$player_j.32[i] | sessantaquattresimi$player_j.64 == trentaduesimi_ord$player_j.32[i])
      )
    }
    
    sessantaquattresimi_ord <- sessantaquattresimi[results.64, ]
    
    rownames(sessantaquattresimi_ord) <- NULL
  }
  
  
  
  return_list <- list()
  
  if (Serie == "Grand Slam") {
    return_list$sessantaquattresimi_ord <- sessantaquattresimi_ord
  }
  
  return_list <- c(return_list,
                   list(trentaduesimi_ord = trentaduesimi_ord,
                        sedicesimi_ord = sedicesimi_ord,
                        ottavi_ord = ottavi_ord,
                        quarti_ord = quarti_ord,
                        semifinali = semifinali))
  
  return(return_list)
}
################# FUNZIONE simulate Tournament (efficiente) ###########################
"Prende in input il valore del tabellone ordinato con la funzione apposita. Successivamente imponi
  il numero delle simulazioni, e simula il torneo quel numero di volte.
  L'output sarà un oggetto formato dalle varie tabelle di probabilità di arrivare a un certo punto
  del torneo"

simulate_tournament <- function(X, sim =  10000, WELO = FALSE) {
  
  n <- nrow(X)
  
  table.64 <- matrix(0, nrow = sim, ncol = 64)
  table.32 <- matrix(0, nrow = sim, ncol = 32)
  table.16 <- matrix(0, nrow = sim, ncol = 16)
  table.8 <- matrix(0, nrow = sim, ncol = 8)
  table.4 <- matrix(0, nrow = sim, ncol = 4)
  table.2 <- matrix(0, nrow = sim, ncol = 2)
  table.1 <- matrix(0, nrow = sim, ncol = 1)
  
  #  startiamo i vettori per i 64esimi
  p.64 <- numeric(64)
  
  if (WELO){
    welo_winner.64 <- numeric(64)
  } else {
    elo_winner.64 <- numeric(64)
  }
  
  n_winner.64 <- numeric(64)
  winner.64 <- numeric(64)
  
  #  #startiamo i vettori per i 32esimi
  p.32<-numeric(32)
  if (WELO){
    welo_winner.32 <- numeric(32)
  } else {
    elo_winner.32 <- numeric(32)
  }
  
  n_winner.32 <- numeric(32)
  winner.32 <- numeric(32)
  
  #startiamo i valori per i 16esimi
  p.16<-numeric(16)
  
  if (WELO){
    welo_winner.16 <- numeric(16)
  } else {
    elo_winner.16<- numeric(16)
  }
  
  n_winner.16 <- numeric(16)
  winner.16 <- numeric(16)
  
  #startiamo i valori per i ottavi
  p.8<-numeric(8)
  
  if (WELO){
    welo_winner.8 <- numeric(8)
  } else {
    elo_winner.8<- numeric(8)
  }
  n_winner.8 <- numeric(8)
  winner.8 <- numeric(8)
  
  #startiamo i valori per i quarti
  p.4<-numeric(4)
  
  if (WELO){
    welo_winner.4 <- numeric(4)
  } else {
    elo_winner.4<- numeric(4)
  }
  n_winner.4 <- numeric(4)
  winner.4 <- numeric(4)
  
  #startiamo i valori per semi
  p.2<-numeric(2)
  if (WELO){
    welo_winner.2 <- numeric(2)
  } else {
    elo_winner.2<- numeric(2)
  }
  n_winner.2 <- numeric(2)
  winner.2 <- numeric(2)
  
  #startiamo i valori per finale
  p.1<-numeric(1)
  if (WELO){
    welo_winner.1 <- numeric(1)
  } else {
    elo_winner.1<- numeric(1)
  }
  n_winner.1 <- numeric(1)
  winner.1 <- numeric(1)
  
  pb <- progress_bar$new(total = sim, format = "[:bar] :percent Time remaining: :eta")
  
  # Ora incomincio una serie di if, quello che fanno è tutti lo stesso, cambia solo in base
  # a che fase del torneo gli metto in input
  
  if ( n==64) {
    for (t in 1:sim) {
      
      pb$tick() # Aggiorna la barra di avanzamento
      
      
      p.64 <- sapply(1:64, function (x) tennis_prob(X[x,3], X[x,4]))
      match_result.64 <- rbern (64, p.64)
      
      if (WELO){
        welo_winner.64 <- ifelse(match_result.64 ==1, X[,3], X[,4])
      } else {
        elo_winner.64<-ifelse(match_result.64 ==1, X[,3], X[,4])}
      
      n_winner.64 <- ifelse(match_result.64 == 1, X[,5] + 1, X[,6] + 1)
      
      if (WELO){
        welo_winner.64 <- welo_winner.64 + ((250/((n_winner.64 + 5)^0.4)) * (ifelse(match_result.64 == 1, 1 - p.64, p.64)))
      } else {
        elo_winner.64 <- elo_winner.64 + ((250/((n_winner.64 + 5)^0.4)) * (ifelse(match_result.64 == 1, 1 - p.64, p.64)))
      }
      
      winner.64 <- ifelse(match_result.64 == 1, X[, 1], X[, 2])
      table.64[t, ] <- winner.64
      
      
      
      
      
      if (WELO){
        p.32 <- sapply(1:32, function (f) tennis_prob(welo_winner.64[f*2-1], welo_winner.64[f*2]))
      } else {
        p.32 <- sapply(1:32, function (f) tennis_prob(elo_winner.64[f*2-1], elo_winner.64[f*2]))}
      
      match_result.32 <- rbern (32, p.32)
      
      if (WELO){
        welo_winner.32 <- ifelse(match_result.32 == 1, welo_winner.64[seq(1, 64, by = 2)], welo_winner.64[seq(2, 64, by = 2)])
      } else {
        elo_winner.32 <- ifelse(match_result.32 == 1, elo_winner.64[seq(1, 64, by = 2)], elo_winner.64[seq(2, 64, by = 2)])
      }
      n_winner.32 <- ifelse(match_result.32 == 1, n_winner.64[seq(1, 64, by = 2)] + 1, n_winner.64[seq(2, 64, by = 2)] + 1)
      
      if (WELO){
        welo_winner.32 <- welo_winner.32 + ((250/((n_winner.32 + 5)^0.4)) * (ifelse(match_result.32 == 1, 1 - p.32, p.32)))
      } else {
        elo_winner.32 <- elo_winner.32 + ((250/((n_winner.32 + 5)^0.4)) * (ifelse(match_result.32 == 1, 1 - p.32, p.32)))
      }
      winner.32 <- ifelse(match_result.32 == 1, winner.64[seq(1, 64, by = 2)], winner.64[seq(2, 64, by = 2)])
      table.32[t, ] <- winner.32
      
      
      
      
      
      
      if (WELO){
        p.16 <- sapply(1:16, function (g) tennis_prob(welo_winner.32[g*2-1], welo_winner.32[g*2]))
      } else {
        p.16 <- sapply(1:16, function (g) tennis_prob(elo_winner.32[g*2-1], elo_winner.32[g*2]))
      }
      
      match_result.16 <- rbern (16, p.16)
      
      if (WELO){
        welo_winner.16 <- ifelse(match_result.16 == 1, welo_winner.32[seq(1, 32, by = 2)], welo_winner.32[seq(2, 32, by = 2)])
      } else {
        elo_winner.16 <- ifelse(match_result.16 == 1, elo_winner.32[seq(1, 32, by = 2)], elo_winner.32[seq(2, 32, by = 2)])
      }
      
      n_winner.16 <- ifelse(match_result.16 == 1, n_winner.32[seq(1, 32, by = 2)] + 1, n_winner.32[seq(2, 32, by = 2)] + 1) #tipo qua se vuoi fare qualcosa di utile dovresti mettere +2 perche si ipotizza che il 32 sia gia stato giocato?
      
      if (WELO){
        welo_winner.16 <- welo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      } else {
        elo_winner.16 <- elo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      }
      
      winner.16 <- ifelse(match_result.16 == 1, winner.32[seq(1, 32, by = 2)], winner.32[seq(2, 32, by = 2)])
      table.16[t, ] <- winner.16
      
      
      
      
      if (WELO){
        p.8 <- sapply(1:8, function (m) tennis_prob(welo_winner.16[m*2-1], welo_winner.16[m*2]))
      } else {
        p.8 <- sapply(1:8, function (m) tennis_prob(elo_winner.16[m*2-1], elo_winner.16[m*2]))
      }
      match_result.8 <- rbern (8, p.8)
      if (WELO){
        welo_winner.8 <- ifelse(match_result.8 == 1, welo_winner.16[seq(1, 16, by = 2)], welo_winner.16[seq(2, 16, by = 2)])
      } else {
        elo_winner.8 <- ifelse(match_result.8 == 1, elo_winner.16[seq(1, 16, by = 2)], elo_winner.16[seq(2, 16, by = 2)])
      }
      n_winner.8 <- ifelse(match_result.8 == 1, n_winner.16[seq(1, 16, by = 2)] + 1, n_winner.16[seq(2, 16, by = 2)] + 1)
      if (WELO){
        welo_winner.8 <- welo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))
      } else {
        elo_winner.8 <- elo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))}
      winner.8 <- ifelse(match_result.8 == 1, winner.16[seq(1, 16, by = 2)], winner.16[seq(2, 16, by = 2)])
      table.8[t, ] <- winner.8
      
      if (WELO){
        p.4 <- sapply(1:4, function (c) tennis_prob(welo_winner.8[c*2-1], welo_winner.8[c*2]))
      } else {
        p.4 <- sapply(1:4, function (c) tennis_prob(elo_winner.8[c*2-1], elo_winner.8[c*2]))}
      match_result.4 <- rbern (4, p.4)
      if (WELO){
        welo_winner.4 <- ifelse(match_result.4 == 1, welo_winner.8[seq(1, 8, by = 2)], welo_winner.8[seq(2, 8, by = 2)])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, elo_winner.8[seq(1, 8, by = 2)], elo_winner.8[seq(2, 8, by = 2)])
      }
      n_winner.4 <- ifelse(match_result.4 == 1, n_winner.8[seq(1, 8, by = 2)] + 1, n_winner.8[seq(2, 8, by = 2)] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1, winner.8[seq(1, 8, by = 2)], winner.8[seq(2, 8, by = 2)])
      table.4[t, ] <- winner.4
      
      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2
      
      
      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
      
      
    }
    
    prob.64 <- table(table.64) / sim
    prob.32 <- table(table.32) / sim
    prob.16 <- table(table.16) / sim
    prob.8 <- table(table.8) / sim
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim
    
    prob.64 <- sort(prob.64, decreasing = TRUE)
    prob.32 <- sort(prob.32, decreasing = TRUE)
    prob.16 <- sort(prob.16, decreasing = TRUE)
    prob.8 <- sort(prob.8, decreasing = TRUE)
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)
    
    odd.1 <- 1 / prob.1
    odd.1 <- sort(odd.1, increasing = TRUE
    
    return(list(prob.64 = prob.64, prob.32 = prob.32, prob.16 = prob.16, prob.8 = prob.8, prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1, odd.1 = odd.1))
  }
  else if (n == 32){
    for (t in 1:sim) {
      
      pb$tick() # Aggiorna la barra di avanzamento
      p.32 <- sapply(1:32, function (f) tennis_prob(X[f,3], X[f,4]))
      match_result.32 <- rbern (32, p.32)
      if (WELO){
        welo_winner.32 <- ifelse(match_result.32 ==1, X[,3], X[,4])
      } else {
        elo_winner.32 <- ifelse(match_result.32 == 1, X[,3], X[,4])    }
      n_winner.32 <- ifelse(match_result.32 == 1,  X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.32 <- welo_winner.32 + ((250/((n_winner.32 + 5)^0.4)) * (ifelse(match_result.32 == 1, 1 - p.32, p.32)))
      } else {
        elo_winner.32 <- elo_winner.32 + ((250/((n_winner.32 + 5)^0.4)) * (ifelse(match_result.32 == 1, 1 - p.32, p.32)))}
      winner.32 <- ifelse(match_result.32 == 1, X[, 1], X[, 2])
      table.32[t, ] <- winner.32
      
      
      
      if (WELO){
        p.16 <- sapply(1:16, function (g) tennis_prob(welo_winner.32[g*2-1], welo_winner.32[g*2]))
      } else {
        p.16 <- sapply(1:16, function (g) tennis_prob(elo_winner.32[g*2-1], elo_winner.32[g*2]))
      }
      
      match_result.16 <- rbern (16, p.16)
      
      if (WELO){
        welo_winner.16 <- ifelse(match_result.16 == 1, welo_winner.32[seq(1, 32, by = 2)], welo_winner.32[seq(2, 32, by = 2)])
      } else {
        elo_winner.16 <- ifelse(match_result.16 == 1, elo_winner.32[seq(1, 32, by = 2)], elo_winner.32[seq(2, 32, by = 2)])
      }
      
      n_winner.16 <- ifelse(match_result.16 == 1, n_winner.32[seq(1, 32, by = 2)] + 1, n_winner.32[seq(2, 32, by = 2)] + 1)
      
      if (WELO){
        welo_winner.16 <- welo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      } else {
        elo_winner.16 <- elo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      }
      
      winner.16 <- ifelse(match_result.16 == 1, winner.32[seq(1, 32, by = 2)], winner.32[seq(2, 32, by = 2)])
      table.16[t, ] <- winner.16
      
      
      
      
      if (WELO){
        p.8 <- sapply(1:8, function (m) tennis_prob(welo_winner.16[m*2-1], welo_winner.16[m*2]))
      } else {
        p.8 <- sapply(1:8, function (m) tennis_prob(elo_winner.16[m*2-1], elo_winner.16[m*2]))
      }
      match_result.8 <- rbern (8, p.8)
      if (WELO){
        welo_winner.8 <- ifelse(match_result.8 == 1, welo_winner.16[seq(1, 16, by = 2)], welo_winner.16[seq(2, 16, by = 2)])
      } else {
        elo_winner.8 <- ifelse(match_result.8 == 1, elo_winner.16[seq(1, 16, by = 2)], elo_winner.16[seq(2, 16, by = 2)])
      }
      n_winner.8 <- ifelse(match_result.8 == 1, n_winner.16[seq(1, 16, by = 2)] + 1, n_winner.16[seq(2, 16, by = 2)] + 1)
      if (WELO){
        welo_winner.8 <- welo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))
      } else {
        elo_winner.8 <- elo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))}
      winner.8 <- ifelse(match_result.8 == 1, winner.16[seq(1, 16, by = 2)], winner.16[seq(2, 16, by = 2)])
      table.8[t, ] <- winner.8
      
      if (WELO){
        p.4 <- sapply(1:4, function (c) tennis_prob(welo_winner.8[c*2-1], welo_winner.8[c*2]))
      } else {
        p.4 <- sapply(1:4, function (c) tennis_prob(elo_winner.8[c*2-1], elo_winner.8[c*2]))}
      match_result.4 <- rbern (4, p.4)
      if (WELO){
        welo_winner.4 <- ifelse(match_result.4 == 1, welo_winner.8[seq(1, 8, by = 2)], welo_winner.8[seq(2, 8, by = 2)])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, elo_winner.8[seq(1, 8, by = 2)], elo_winner.8[seq(2, 8, by = 2)])
      }
      n_winner.4 <- ifelse(match_result.4 == 1, n_winner.8[seq(1, 8, by = 2)] + 1, n_winner.8[seq(2, 8, by = 2)] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1, winner.8[seq(1, 8, by = 2)], winner.8[seq(2, 8, by = 2)])
      table.4[t, ] <- winner.4
      
      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2
      
      
      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
    }
    
    prob.16 <- table(table.16) / sim
    prob.8 <- table(table.8) / sim
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim
    
    prob.16 <- sort(prob.16, decreasing = TRUE)
    prob.8 <- sort(prob.8, decreasing = TRUE)
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)
    
    odd.1 <- 1 / prob.1
    odd.1 <- sort(odd.1, increasing = TRUE
    
    return(list(prob.16 = prob.16, prob.8 = prob.8, prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1, odd.1 = odd.1))
  }
  
  else if (n == 16){
    for (t in 1:sim) {
      
      pb$tick() # Aggiorna la barra di avanzamento
      p.16 <- sapply(1:16, function (j) tennis_prob(X[j,3], X[j,4]))
      match_result.16 <- rbern (16, p.16)
      if (WELO){
        welo_winner.16 <- ifelse(match_result.16 ==1, X[,3], X[,4])
      } else {
        elo_winner.16 <- ifelse(match_result.16 == 1, X[,3], X[,4])    }
      n_winner.16 <- ifelse(match_result.16 == 1,  X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.16 <- welo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))
      } else {
        elo_winner.16 <- elo_winner.16 + ((250/((n_winner.16 + 5)^0.4)) * (ifelse(match_result.16 == 1, 1 - p.16, p.16)))}
      winner.16 <- ifelse(match_result.16 == 1, X[, 1], X[, 2])
      table.16[t, ] <- winner.16
      
      if (WELO){
        p.8 <- sapply(1:8, function (m) tennis_prob(welo_winner.16[m*2-1], welo_winner.16[m*2]))
      } else {
        p.8 <- sapply(1:8, function (m) tennis_prob(elo_winner.16[m*2-1], elo_winner.16[m*2]))
      }
      match_result.8 <- rbern (8, p.8)
      if (WELO){
        welo_winner.8 <- ifelse(match_result.8 == 1, welo_winner.16[seq(1, 16, by = 2)], welo_winner.16[seq(2, 16, by = 2)])
      } else {
        elo_winner.8 <- ifelse(match_result.8 == 1, elo_winner.16[seq(1, 16, by = 2)], elo_winner.16[seq(2, 16, by = 2)])
      }
      n_winner.8 <- ifelse(match_result.8 == 1, n_winner.16[seq(1, 16, by = 2)] + 1, n_winner.16[seq(2, 16, by = 2)] + 1)
      if (WELO){
        welo_winner.8 <- welo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))
      } else {
        elo_winner.8 <- elo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))}
      winner.8 <- ifelse(match_result.8 == 1, winner.16[seq(1, 16, by = 2)], winner.16[seq(2, 16, by = 2)])
      table.8[t, ] <- winner.8
      
      if (WELO){
        p.4 <- sapply(1:4, function (c) tennis_prob(welo_winner.8[c*2-1], welo_winner.8[c*2]))
      } else {
        p.4 <- sapply(1:4, function (c) tennis_prob(elo_winner.8[c*2-1], elo_winner.8[c*2]))}
      match_result.4 <- rbern (4, p.4)
      if (WELO){
        welo_winner.4 <- ifelse(match_result.4 == 1, welo_winner.8[seq(1, 8, by = 2)], welo_winner.8[seq(2, 8, by = 2)])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, elo_winner.8[seq(1, 8, by = 2)], elo_winner.8[seq(2, 8, by = 2)])
      }
      n_winner.4 <- ifelse(match_result.4 == 1, n_winner.8[seq(1, 8, by = 2)] + 1, n_winner.8[seq(2, 8, by = 2)] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1, winner.8[seq(1, 8, by = 2)], winner.8[seq(2, 8, by = 2)])
      table.4[t, ] <- winner.4
      
      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2
      
      
      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
    }
    
    prob.8 <- table(table.8) / sim
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim
    
    prob.8 <- sort(prob.8, decreasing = TRUE)
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)
    
    odd.1 <- 1 / prob.1
    odd.1 <- sort(odd.1, increasing = TRUE
    
    return(list(prob.8 = prob.8, prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1, odd.1 = odd.1))
  }
  
  else if (n == 8){
    for (t in 1:sim) {
      
      pb$tick() # Aggiorna la barra di avanzamento
      
      p.8 <- sapply(1:8, function (m)  tennis_prob(X[m,3], X[m,4]))
      match_result.8 <- rbern (8, p.8)
      if (WELO){
        welo_winner.8 <- ifelse(match_result.8 == 1, X[,3], X[,4])
      } else {
        elo_winner.8 <- ifelse(match_result.8 == 1, X[,3], X[,4]) }
      n_winner.8 <- ifelse(match_result.8 == 1, X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.8 <- welo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))
      } else {
        elo_winner.8 <- elo_winner.8 + ((250/((n_winner.8 + 5)^0.4)) * (ifelse(match_result.8 == 1, 1 - p.8, p.8)))}
      winner.8 <- ifelse(match_result.8 == 1, X[, 1], X[, 2])
      table.8[t, ] <- winner.8
      
      if (WELO){
        p.4 <- sapply(1:4, function (c) tennis_prob(welo_winner.8[c*2-1], welo_winner.8[c*2]))
      } else {
        p.4 <- sapply(1:4, function (c) tennis_prob(elo_winner.8[c*2-1], elo_winner.8[c*2]))}
      match_result.4 <- rbern (4, p.4)
      if (WELO){
        welo_winner.4 <- ifelse(match_result.4 == 1, welo_winner.8[seq(1, 8, by = 2)], welo_winner.8[seq(2, 8, by = 2)])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, elo_winner.8[seq(1, 8, by = 2)], elo_winner.8[seq(2, 8, by = 2)])
      }
      n_winner.4 <- ifelse(match_result.4 == 1, n_winner.8[seq(1, 8, by = 2)] + 1, n_winner.8[seq(2, 8, by = 2)] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1, winner.8[seq(1, 8, by = 2)], winner.8[seq(2, 8, by = 2)])
      table.4[t, ] <- winner.4
      
      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2
      
      
      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
    }
    
    prob.8 <- table(table.8) / sim
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim
    
    prob.8 <- sort(prob.8, decreasing = TRUE)
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)
    
    odd.1 <- 1 / prob.1
    odd.1 <- sort(odd.1, increasing = TRUE)
    
    return(list(prob.8 = prob.8, prob.4 = prob.4, prob.2 = prob.2, prob.1 = prob.1, odd.1 = odd.1))
  }
  
  
  else if (n == 4){
    for (t in 1:sim) {
      
      pb$tick() # Aggiorna la barra di avanzamento
      
      p.4 <- sapply(1:4, function (c) tennis_prob(X[c,3], X[c,4]))
      match_result.4 <- rbern (4, p.4)
      if (WELO) {
        welo_winner.4 <- ifelse(match_result.4 == 1, X[,3], X[,4])
      } else {
        elo_winner.4 <- ifelse(match_result.4 == 1, X[,3], X[,4])}
      n_winner.4 <- ifelse(match_result.4 == 1, X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.4 <- welo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))
      } else {
        elo_winner.4 <- elo_winner.4 + ((250/((n_winner.4 + 5)^0.4)) * (ifelse(match_result.4 == 1, 1 - p.4, p.4)))}
      winner.4 <- ifelse(match_result.4 == 1,  X[, 1], X[, 2])
      table.4[t, ] <- winner.4
      
      if (WELO){
        p.2 <- sapply(1:2, function (b) tennis_prob(welo_winner.4[b*2-1], welo_winner.4[b*2]))
      } else {
        p.2 <- sapply(1:2, function (b) tennis_prob(elo_winner.4[b*2-1], elo_winner.4[b*2]))}
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, welo_winner.4[seq(1, 4, by = 2)], welo_winner.4[seq(2, 4, by = 2)])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, elo_winner.4[seq(1, 4, by = 2)], elo_winner.4[seq(2, 4, by = 2)])}
      n_winner.2 <- ifelse(match_result.2 == 1, n_winner.4[seq(1, 4, by = 2)] + 1, n_winner.4[seq(2, 4, by = 2)] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, winner.4[seq(1, 4, by = 2)], winner.4[seq(2, 4, by = 2)])
      table.2[t, ] <- winner.2
      
      
      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
      
    }
    
    prob.4 <- table(table.4) / sim
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim
    
    prob.4 <- sort(prob.4, decreasing = TRUE)
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)
    
  
    odd.1 <- 1 / prob.1
    
    return(list(prob.4 = prob.4,
                prob.2 = prob.2, 
                prob.1 = prob.1, odd.1 = odd.1))
    
  }
  
  else if (n == 2){
    for (t in 1:sim) {
      
      pb$tick() # Aggiorna la barra di avanzamento
      
      p.2 <- sapply(1:2, function (b) tennis_prob(X[b,3], X[b,4]))
      match_result.2 <- rbern (2, p.2)
      if (WELO){
        welo_winner.2 <- ifelse(match_result.2 == 1, X[,3], X[,4])
      } else {
        elo_winner.2 <- ifelse(match_result.2 == 1, X[,3], X[,4])    }
      n_winner.2 <- ifelse(match_result.2 == 1,  X[,5] + 1, X[,6] + 1)
      if (WELO){
        welo_winner.2 <- welo_winner.2 + ((250/((n_winner.2 + 5)^0.4)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))
      } else {
        elo_winner.2 <- elo_winner.2 + ((250/((n_winner.2 + 5)^0.2)) * (ifelse(match_result.2 == 1, 1 - p.2, p.2)))}
      winner.2 <- ifelse(match_result.2 == 1, X[, 1], X[, 2])
      table.2[t, ] <- winner.2
      
      
      
      
      if (WELO){
        p.1 <- tennis_prob(welo_winner.2[1], welo_winner.2[2])
      } else {
        p.1 <- tennis_prob(elo_winner.2[1], elo_winner.2[2])}
      match_result.1 <- rbern(1, p.1)
      if (WELO){
        welo_winner.1 <- if (match_result.1 == 1) welo_winner.2[1] else  welo_winner.2[2]
      } else {
        elo_winner.1 <- if (match_result.1 == 1) elo_winner.2[1] else  elo_winner.2[2] }
      n_winner.1 <- if (match_result.1 == 1)  n_winner.2[1] + 1 else n_winner.2[2] + 1
      if (WELO){
        welo_winner.1 <- welo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1)))
      } else {
        elo_winner.1 <- elo_winner.1 + ((250/((n_winner.1+5)^0.4)) * (ifelse(match_result.1 == 1, 1 - p.1, p.1))) }
      winner.1 <- if (match_result.1 == 1) winner.2[1] else winner.2[2]
      table.1[t, 1] <- winner.1
      
      
    }
    
    prob.2 <- table(table.2) / sim
    prob.1 <- table(table.1) / sim
    odd.1 <- 1/prob.1
    
    prob.2 <- sort(prob.2, decreasing = TRUE)
    prob.1 <- sort(prob.1, decreasing = TRUE)
    odd.1 <- sort(odd.1, increasing = TRUE)
    
    return(list(prob.2 = prob.2, prob.1 = prob.1, odd.1 = odd.1))
  }
}
