
# Function to generate random card draws (returns clean data for analysis)
generate_draws <- function(deck, num_draws = 1) {
  # Generate random rolls
  rolls <- sample(1:1000, num_draws, replace = TRUE)
  
  # Calculate card indices using modulo
  deck_size <- nrow(deck)
  card_indices <- ((rolls - 1) %% deck_size) + 1
  
  # Get the selected cards
  selected_cards <- deck[card_indices, ]
  
  # Create clean results data frame
  results <- data.frame(
    Draw = 1:num_draws,
    Roll = rolls,
    Card_Index = card_indices,
    Card_Code = as.character(selected_cards$Card),
    Rank = as.character(selected_cards$Rank),
    Suit = as.character(selected_cards$Suit),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Function to display draw results with detailed history
display_draw_history <- function(draw_results, show_summary = TRUE) {
  num_draws <- nrow(draw_results)
  
  if (num_draws <= 20) {
    # Show detailed results for small numbers of draws
    cat("=== RANDOM CARD DRAW HISTORY ===\n")
    for (i in 1:num_draws) {
      cat(sprintf("Draw %d: Rolled %d -> %s (%s of %s)\n", 
                  draw_results$Draw[i], draw_results$Roll[i], draw_results$Card_Code[i],
                  draw_results$Rank[i], draw_results$Suit[i]))
    }
    cat("Total draws:", num_draws, "\n")
    cat("================================\n")
  } else {
    # Show summary for large numbers of draws
    cat("=== BULK DRAW SUMMARY ===\n")
    cat("Total draws:", num_draws, "\n")
    cat("Unique cards drawn:", length(unique(draw_results$Card_Code)), "\n")
    cat("Most common card:", names(which.max(table(draw_results$Card_Code))), "\n")
    cat("First 10 draws:\n")
    print(head(draw_results, 10))
    cat("...\n")
    cat("Last 5 draws:\n")
    print(tail(draw_results, 5))
    cat("=========================\n")
  }
  
  # Additional summary statistics if requested
  if (show_summary) {
    cat("\n=== DISTRIBUTION ANALYSIS ===\n")
    card_dist <- table(draw_results$Card_Code)
    if (length(card_dist) > 0) {
      cat("Card distribution:\n")
      print(card_dist)
    }
    
    rank_dist <- table(draw_results$Rank)
    cat("\nRank distribution:\n")
    print(rank_dist)
    
    suit_dist <- table(draw_results$Suit)
    cat("\nSuit distribution:\n")
    print(suit_dist)
    cat("================================\n")
  }
}