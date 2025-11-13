# =============================================================================
# 3. Simulations
# =============================================================================
source("Decks.R")
library(png)
library(grid)
library(gridExtra)
library(ggplot2)
library(reshape2)

# Function to compare two decks using card count battles
compare_deck_battles <- function(deck1, deck2, num_battles = 1000, show_plots = TRUE) {
  # Generate shared random rolls for both decks
  shared_rolls <- sample(1:1000, num_battles, replace = TRUE)
  
  # Initialize results matrices
  cards1 <- unique(deck1$Card)
  cards2 <- unique(deck2$Card)
  
  # Create results matrices for wins, losses, ties
  results_deck1 <- matrix(0, nrow = length(cards1), ncol = 3,
                          dimnames = list(cards1, c("Wins", "Losses", "Ties")))
  results_deck2 <- matrix(0, nrow = length(cards2), ncol = 3,
                          dimnames = list(cards2, c("Wins", "Losses", "Ties")))
  
  # Track battle history
  battle_history <- data.frame(
    Battle = 1:num_battles,
    Roll = shared_rolls,
    Card1 = character(num_battles),
    Card2 = character(num_battles),
    Count1 = numeric(num_battles),
    Count2 = numeric(num_battles),
    Result = character(num_battles),
    stringsAsFactors = FALSE
  )
  
  # Get card counts for both decks
  count1 <- table(deck1$Card)
  count2 <- table(deck2$Card)
  
  # Process each battle
  for (i in 1:num_battles) {
    roll <- shared_rolls[i]
    
    # Draw cards from both decks using the same roll
    idx1 <- ((roll - 1) %% nrow(deck1)) + 1
    idx2 <- ((roll - 1) %% nrow(deck2)) + 1
    
    card1 <- as.character(deck1$Card[idx1])
    card2 <- as.character(deck2$Card[idx2])
    
    # Get counts of these cards in their respective decks
    cnt1 <- count1[card1]
    cnt2 <- count2[card2]
    
    # Determine battle outcome
    if (cnt1 > cnt2) {
      # Deck1 wins
      results_deck1[card1, "Wins"] <- results_deck1[card1, "Wins"] + 1
      results_deck2[card2, "Losses"] <- results_deck2[card2, "Losses"] + 1
      result <- "Deck1 Wins"
    } else if (cnt1 < cnt2) {
      # Deck2 wins
      results_deck1[card1, "Losses"] <- results_deck1[card1, "Losses"] + 1
      results_deck2[card2, "Wins"] <- results_deck2[card2, "Wins"] + 1
      result <- "Deck2 Wins"
    } else {
      # Tie
      results_deck1[card1, "Ties"] <- results_deck1[card1, "Ties"] + 1
      results_deck2[card2, "Ties"] <- results_deck2[card2, "Ties"] + 1
      result <- "Tie"
    }
    
    # Record battle history
    battle_history$Card1[i] <- card1
    battle_history$Card2[i] <- card2
    battle_history$Count1[i] <- cnt1
    battle_history$Count2[i] <- cnt2
    battle_history$Result[i] <- result
  }
  
  # Create plots if requested
  if (show_plots) {
    plot_deck_battle_results_faceted(results_deck1, results_deck2, num_battles)
  }
  # Return comprehensive results
  return(list(
    deck1_results = results_deck1,
    deck2_results = results_deck2,
    battle_history = battle_history,
    summary = list(
      total_battles = num_battles,
      deck1_wins = sum(results_deck1[, "Wins"]),
      deck2_wins = sum(results_deck2[, "Wins"]),
      ties = sum(results_deck1[, "Ties"])
    )
  ))
}
# Plot the battle results with uniform Y-axis and proper ordering
plot_deck_battle_results <- function(results_deck1, results_deck2, num_battles) {
  # Load required libraries
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(reshape2)) install.packages("reshape2")
  if (!require(gridExtra)) install.packages("gridExtra")
  library(ggplot2)
  library(reshape2)
  library(gridExtra)
  
  # Define the standard card order (AH, KH, QH, JH, 10H, 9H, ... 2H, AS, KS, ... 2D)
  suits <- c("H", "S", "C", "D")
  ranks <- c("A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  standard_order <- paste0(rep(ranks, each = 4), rep(suits, times = length(ranks)))
  
  # Prepare data for deck1 - maintain standard order
  deck1_df <- as.data.frame(results_deck1)
  deck1_df$Card <- rownames(results_deck1)
  
  # Reorder to match standard order (only keep cards that exist in results)
  deck1_df$Card <- factor(deck1_df$Card, levels = standard_order)
  deck1_df <- deck1_df[order(deck1_df$Card), ]
  deck1_df <- deck1_df[complete.cases(deck1_df), ]  # Remove NA rows for cards not in results
  
  deck1_melt <- melt(deck1_df, id.vars = "Card", 
                     variable.name = "Outcome", value.name = "Count")
  deck1_melt$Card <- factor(deck1_melt$Card, levels = deck1_df$Card)
  
  # Prepare data for deck2 - maintain standard order
  deck2_df <- as.data.frame(results_deck2)
  deck2_df$Card <- rownames(results_deck2)
  
  # Reorder to match standard order
  deck2_df$Card <- factor(deck2_df$Card, levels = standard_order)
  deck2_df <- deck2_df[order(deck2_df$Card), ]
  deck2_df <- deck2_df[complete.cases(deck2_df), ]  # Remove NA rows for cards not in results
  
  deck2_melt <- melt(deck2_df, id.vars = "Card", 
                     variable.name = "Outcome", value.name = "Count")
  deck2_melt$Card <- factor(deck2_melt$Card, levels = deck2_df$Card)
  
  # Calculate uniform Y-axis limits for both plots
  max_count1 <- max(deck1_melt$Count)
  max_count2 <- max(deck2_melt$Count)
  uniform_y_max <- max(max_count1, max_count2) * 1.1  # Add 10% padding
  
  # Create horizontal plots with consistent ordering and uniform Y-axis
  p1 <- ggplot(deck1_melt, aes(x = Card, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(values = c("Wins" = "#2E8B57", "Losses" = "#DC143C", "Ties" = "#FFD700")) +
    labs(title = "Deck 1: Card Battle Performance",
         subtitle = paste("Total Battles:", num_battles),
         x = "Card", y = "Number of Battles") +
    ylim(0, uniform_y_max) +  # Uniform Y-axis
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    scale_x_discrete(drop = FALSE)  # Keep all card labels even if some are missing
  
  p2 <- ggplot(deck2_melt, aes(x = Card, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(values = c("Wins" = "#2E8B57", "Losses" = "#DC143C", "Ties" = "#FFD700")) +
    labs(title = "Deck 2: Card Battle Performance", 
         subtitle = paste("Total Battles:", num_battles),
         x = "Card", y = "Number of Battles") +
    ylim(0, uniform_y_max) +  # Uniform Y-axis
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    scale_x_discrete(drop = FALSE)  # Keep all card labels even if some are missing
  
  # Arrange plots vertically
  grid.arrange(p1, p2, ncol = 1, heights = c(1, 1))
}

# Alternative version with faceting and uniform Y-axis
plot_deck_battle_results_faceted <- function(results_deck1, results_deck2, num_battles) {
  # Define the standard card order
  suits <- c("H", "S", "C", "D")
  ranks <- c("A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  standard_order <- paste0(rep(ranks, each = 4), rep(suits, times = length(ranks)))
  
  # Prepare combined data
  deck1_df <- as.data.frame(results_deck1)
  deck1_df$Card <- rownames(results_deck1)
  deck1_df$Deck <- "Deck 1"
  
  deck2_df <- as.data.frame(results_deck2)
  deck2_df$Card <- rownames(results_deck2)
  deck2_df$Deck <- "Deck 2"
  
  combined_df <- rbind(deck1_df, deck2_df)
  
  # Apply standard ordering
  combined_df$Card <- factor(combined_df$Card, levels = standard_order)
  combined_df <- combined_df[order(combined_df$Card), ]
  combined_df <- combined_df[complete.cases(combined_df), ]
  
  combined_melt <- melt(combined_df, id.vars = c("Card", "Deck"), 
                        variable.name = "Outcome", value.name = "Count")
  combined_melt$Card <- factor(combined_melt$Card, levels = unique(combined_df$Card))
  
  # Calculate uniform Y-axis limit
  max_count <- max(combined_melt$Count)
  uniform_y_max <- max_count * 1.1  # Add 10% padding
  
  # Create faceted plot with uniform Y-axis
  p <- ggplot(combined_melt, aes(x = Card, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    scale_fill_manual(values = c("Wins" = "#2E8B57", "Losses" = "#DC143C", "Ties" = "#FFD700")) +
    labs(title = "Card Battle Performance Comparison",
         subtitle = paste("Total Battles:", num_battles),
         x = "Card", y = "Number of Battles") +
    ylim(0, uniform_y_max) +  # Uniform Y-axis
    facet_grid(Deck ~ ., scales = "fixed") +  # Fixed scales for uniform Y-axis
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(face = "bold", size = 10)
    ) +
    scale_x_discrete(drop = FALSE)
  
  print(p)
}