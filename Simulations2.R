# =============================================================================
# 3. Simulations
# =============================================================================
source("Decks.R")
library(png)
library(grid)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(ggplot2)
library(reshape2)
# Function to plot the battle results with proper deck names and standard card order
plot_deck_battle_results_faceted <- function(results_deck1, results_deck2, num_battles, deck1_name = "Deck 1", deck2_name = "Deck 2") {
  # Load required library
  if (!require(reshape2)) install.packages("reshape2"); library(reshape2)
  if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
  
  # Define the standard card order
  suits <- c("S", "H", "D", "C")  # Spades, Hearts, Diamonds, Clubs
  ranks <- c("A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  standard_order <- paste0(rep(ranks, each = 4), rep(suits, times = length(ranks)))
  
  # Prepare combined data
  deck1_df <- as.data.frame(results_deck1)
  deck1_df$Card <- rownames(results_deck1)
  deck1_df$Deck <- deck1_name
  
  deck2_df <- as.data.frame(results_deck2)
  deck2_df$Card <- rownames(results_deck2)
  deck2_df$Deck <- deck2_name
  
  combined_df <- rbind(deck1_df, deck2_df)
  
  # Get only cards that have at least one battle (wins, losses, or ties > 0)
  active_cards <- unique(combined_df$Card[
    combined_df$Wins > 0 | combined_df$Losses > 0 | combined_df$Ties > 0
  ])
  
  # Filter to only include active cards
  combined_df <- combined_df[combined_df$Card %in% active_cards, ]
  
  # Apply standard ordering but ONLY for active cards
  active_standard_order <- standard_order[standard_order %in% active_cards]
  combined_df$Card <- factor(combined_df$Card, levels = active_standard_order)
  
  # Order the data frame by the card factor
  combined_df <- combined_df[order(combined_df$Card), ]
  
  # Melt the data for plotting
  combined_melt <- melt(combined_df, id.vars = c("Card", "Deck"), 
                        variable.name = "Outcome", value.name = "Count")
  
  # Ensure the melted data uses the same factor levels
  combined_melt$Card <- factor(combined_melt$Card, levels = active_standard_order)
  
  # Calculate unified Y-axis limit
  max_count <- max(combined_melt$Count, na.rm = TRUE)
  unified_y_max <- max_count * 1.1  # Add 10% padding
  
  # Create faceted plot with unified Y-axis and value labels
  p <- ggplot(combined_melt, aes(x = Card, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    # Win labels (white on green)
    geom_text(data = combined_df, aes(x = Card, y = Wins/2, 
                                      label = ifelse(Wins > 0, Wins, "")),
              color = "white", size = 2.5, fontface = "bold", inherit.aes = FALSE) +
    # Loss labels (white on red)
    geom_text(data = combined_df, aes(x = Card, y = Wins + Losses/2, 
                                      label = ifelse(Losses > 0, Losses, "")),
              color = "white", size = 2.5, fontface = "bold", inherit.aes = FALSE) +
    # Tie labels (black on yellow)
    geom_text(data = combined_df, aes(x = Card, y = Wins + Losses + Ties/2, 
                                      label = ifelse(Ties > 0, Ties, "")),
              color = "black", size = 2.5, fontface = "bold", inherit.aes = FALSE) +
    scale_fill_manual(values = c("Wins" = "#2E8B57", "Losses" = "#DC143C", "Ties" = "#FFD700")) +
    labs(title = "Card Battle Performance Comparison",
         subtitle = paste("Total Battles:", num_battles),
         x = "Card", y = "Number of Battles") +
    ylim(0, unified_y_max) +  # Unified Y-axis
    facet_grid(Deck ~ ., scales = "fixed") +  # Fixed scales for unified Y-axis
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "lightgray", color = "darkgray")
    ) +
    scale_x_discrete(drop = TRUE)  # This should now properly drop unused levels
  
  print(p)
}

# New function with candlestick format to show differences more clearly
plot_deck_battle_candlestick <- function(results_deck1, results_deck2, num_battles, deck1_name = "Deck 1", deck2_name = "Deck 2") {
  # Load required library
  if (!require(reshape2)) install.packages("reshape2"); library(reshape2)
  if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
  if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
  
  # Define the standard card order
  suits <- c("S", "H", "D", "C")  # Spades, Hearts, Diamonds, Clubs
  ranks <- c("A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  standard_order <- paste0(rep(ranks, each = 4), rep(suits, times = length(ranks)))
  
  # Prepare data for both decks
  deck1_df <- as.data.frame(results_deck1)
  deck1_df$Card <- rownames(results_deck1)
  deck1_df$Deck <- deck1_name
  
  deck2_df <- as.data.frame(results_deck2)
  deck2_df$Card <- rownames(results_deck2)
  deck2_df$Deck <- deck2_name
  
  # Get only cards that have at least one battle in either deck
  active_cards <- unique(c(
    deck1_df$Card[deck1_df$Wins > 0 | deck1_df$Losses > 0 | deck1_df$Ties > 0],
    deck2_df$Card[deck2_df$Wins > 0 | deck2_df$Losses > 0 | deck2_df$Ties > 0]
  ))
  
  # Filter to only include active cards
  deck1_df <- deck1_df[deck1_df$Card %in% active_cards, ]
  deck2_df <- deck2_df[deck2_df$Card %in% active_cards, ]
  
  # Merge the two decks to compare performance
  comparison_df <- merge(deck1_df, deck2_df, by = "Card", suffixes = c("_deck1", "_deck2"))
  
  # Calculate performance metrics
  comparison_df$Win_Rate_Deck1 <- comparison_df$Wins_deck1 / 
    (comparison_df$Wins_deck1 + comparison_df$Losses_deck1 + comparison_df$Ties_deck1)
  comparison_df$Win_Rate_Deck2 <- comparison_df$Wins_deck2 / 
    (comparison_df$Wins_deck2 + comparison_df$Losses_deck2 + comparison_df$Ties_deck2)
  
  comparison_df$Performance_Diff <- comparison_df$Win_Rate_Deck1 - comparison_df$Win_Rate_Deck2
  
  # Apply standard ordering but ONLY for active cards
  active_standard_order <- standard_order[standard_order %in% active_cards]
  comparison_df$Card <- factor(comparison_df$Card, levels = active_standard_order)
  comparison_df <- comparison_df[order(comparison_df$Card), ]
  
  # Create candlestick-style plot
  p <- ggplot(comparison_df, aes(x = Card)) +
    # Candlestick body (performance difference)
    geom_rect(aes(xmin = as.numeric(Card) - 0.4, 
                  xmax = as.numeric(Card) + 0.4,
                  ymin = pmin(0, Performance_Diff), 
                  ymax = pmax(0, Performance_Diff),
                  fill = ifelse(Performance_Diff >= 0, "#2E8B57", "#DC143C")),
              alpha = 0.7) +
    # Zero line reference
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    # Points for win rates
    geom_point(aes(y = Win_Rate_Deck1, color = deck1_name), size = 2, shape = 19) +
    geom_point(aes(y = Win_Rate_Deck2, color = deck2_name), size = 2, shape = 17) +
    # Lines connecting win rates (showing the range)
    geom_segment(aes(x = as.numeric(Card) - 0.2, xend = as.numeric(Card) + 0.2,
                     y = Win_Rate_Deck1, yend = Win_Rate_Deck1, color = deck1_name)) +
    geom_segment(aes(x = as.numeric(Card) - 0.2, xend = as.numeric(Card) + 0.2,
                     y = Win_Rate_Deck2, yend = Win_Rate_Deck2, color = deck2_name)) +
    scale_fill_identity() +
    scale_color_manual(name = "Deck Win Rate", 
                       values = c("#1f77b4", "#ff7f0e"),
                       labels = c(deck1_name, deck2_name)) +
    labs(title = "Card Battle Performance Difference (Candlestick)",
         subtitle = paste("Total Battles:", num_battles),
         x = "Card", 
         y = "Win Rate / Performance Difference",
         caption = paste("Green: Better in", deck1_name, "| Red: Better in", deck2_name)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    scale_x_discrete(drop = TRUE) +
    scale_y_continuous(labels = scales::percent)
  
  print(p)
}

# Updated compare_deck_battles function to use both plot types
compare_deck_battles <- function(deck1, deck2, num_battles = 1000, show_plots = TRUE, plot_type = "faceted") {
  # Generate shared random rolls for both decks
  shared_rolls <- sample(1:1000, num_battles, replace = TRUE)
  
  # Get variable names for automatic deck naming
  deck1_name <- deparse(substitute(deck1))
  deck2_name <- deparse(substitute(deck2))
  
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
      result <- paste(deck1_name, "Wins")
    } else if (cnt1 < cnt2) {
      # Deck2 wins
      results_deck1[card1, "Losses"] <- results_deck1[card1, "Losses"] + 1
      results_deck2[card2, "Wins"] <- results_deck2[card2, "Wins"] + 1
      result <- paste(deck2_name, "Wins")
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
    if (plot_type == "faceted") {
      plot_deck_battle_results_faceted(results_deck1, results_deck2, num_battles, deck1_name, deck2_name)
    } else if (plot_type == "candlestick") {
      plot_deck_battle_candlestick(results_deck1, results_deck2, num_battles, deck1_name, deck2_name)
    }
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
      ties = sum(results_deck1[, "Ties"]),
      deck1_name = deck1_name,
      deck2_name = deck2_name
    )
  ))
}
