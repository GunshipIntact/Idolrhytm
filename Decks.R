# =============================================================================
# Idolrhythm
# =============================================================================
#install.packages(c("png", "grid", "gridExtra", "ggplot2"))
library(png)
library(grid)
library(gridExtra)
library(ggplot2)

# =============================================================================
# 0. Deck Creation + Sorting + Adding/ Removing Cards
# =============================================================================

# Create a deck of cards
create_deck <- function() {
  # Define suits and their abbreviations
  suits <- c("S", "H", "C", "D")  # Spades, Hearts, Clubs, Diamonds
  suit_names <- c("S" = "Spades", "H" = "Hearts", "C" = "Clubs", "D" = "Diamonds")
  
  # Define ranks in the specified order (Ace high to 2 low)
  ranks <- c("A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  rank_names <- c("A" = "Ace", "K" = "King", "Q" = "Queen", "J" = "Jack",
                  "10" = "10", "9" = "9", "8" = "8", "7" = "7", "6" = "6",
                  "5" = "5", "4" = "4", "3" = "3", "2" = "2")
  
  # Create all combinations of ranks and suits
  deck <- expand.grid(Rank = ranks, Suit = suits)
  
  # Create card codes (e.g., "AH", "2S")
  deck$Card <- paste0(deck$Rank, deck$Suit)
  
  # Set the order for sorting
  deck$Rank <- factor(deck$Rank, levels = ranks)
  deck$Suit <- factor(deck$Suit, levels = suits)
  
  return(deck)
}


# Initialize the deck
deck <- create_deck()

# Function to sort the deck
sort_deck <- function(deck) {
  # Sort by suit first (columns), then by rank (rows)
  sorted_deck <- deck[order(deck$Suit, deck$Rank), ]
  return(sorted_deck)
}

# =============================================================================
# 1. Adding/ Removing Cards + Erratic Deck + Adding/Removing Multiple cards + Remove Ranks or Suits
# =============================================================================


# Function to add a card to the deck
add_card <- function(deck, card_code) {
  # Validate card code format (e.g., "AH", "2S")
  if (!grepl("^[AKQJ2-9][0-9]?[SHCD]$", card_code)) {
    stop("Invalid card code format. Use format like 'AH', '2S', '10H'")
  }
  
  # Extract rank and suit from card code
  rank <- gsub("([AKQJ2-9][0-9]?)[SHCD]", "\\1", card_code)
  suit <- gsub("[AKQJ2-9][0-9]?", "", card_code)
  
  # Create new card
  new_card <- data.frame(
    Rank = factor(rank, levels = levels(deck$Rank)),
    Suit = factor(suit, levels = levels(deck$Suit)),
    Card = card_code
  )
  
  # Add to deck and sort
  new_deck <- rbind(deck, new_card)
  return(sort_deck(new_deck))
}

# Function to remove a card from the deck
remove_card <- function(deck, card_code) {
  # Find and remove the card
  card_index <- which(deck$Card == card_code)
  
  if (length(card_index) == 0) {
    warning("Card ", card_code, " not found in the deck")
    return(deck)
  }
  
  return(deck[-card_index[1], ])
}

# Customizable erratic deck creator
create_erratic_deck <- function(deck_size = 52) {
  # Define suits and ranks
  suits <- c("S", "H", "C", "D")
  ranks <- c("A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  
  # Generate random cards with specified deck size
  random_ranks <- sample(ranks, deck_size, replace = TRUE)
  random_suits <- sample(suits, deck_size, replace = TRUE)
  card_codes <- paste0(random_ranks, random_suits)
  
  # Create the deck using your existing structure
  random_deck <- data.frame(
    Rank = factor(random_ranks, levels = ranks),
    Suit = factor(random_suits, levels = suits),
    Card = card_codes
  )
  
  # Sort using your existing sort function
  return(sort_deck(random_deck))
}

# =============================================================================
# 1.1 Fantoms Sorting Algorithm + BlockShip Sorting Algorithm
# =============================================================================

# Custom sorting function for your requirements
sort_Fantoms <- function(deck) {
  # Count duplicates for each card
  card_counts <- table(deck$Card)
  
  # Add count information to the deck
  deck$Count <- card_counts[deck$Card]
  
  # Define custom suit order (S > H > C > D)
  suit_order <- c("S" = 1, "H" = 2, "C" = 3, "D" = 4)
  deck$Suit_Order <- suit_order[as.character(deck$Suit)]
  
  # Define rank order (A > K > Q > J > 10 > 9 > ... > 2)
  rank_order <- c("A" = 1, "K" = 2, "Q" = 3, "J" = 4, "10" = 5, "9" = 6, 
                  "8" = 7, "7" = 8, "6" = 9, "5" = 10, "4" = 11, "3" = 12, "2" = 13)
  deck$Rank_Order <- rank_order[as.character(deck$Rank)]
  
  # Sort by: Count (descending), Suit_Order (ascending), Rank_Order (ascending)
  sorted_deck <- deck[order(-deck$Count, deck$Suit_Order, deck$Rank_Order), ]
  
  # Remove temporary columns
  sorted_deck$Count <- NULL
  sorted_deck$Suit_Order <- NULL
  sorted_deck$Rank_Order <- NULL
  
  return(sorted_deck)
}

#Sorting algorithm: Group by rank, then sort by suit order (S,H,C,D)
sort_BlockShip <- function(deck) {
  # Count duplicates for each card
  card_counts <- table(deck$Card)
  deck$Count <- card_counts[deck$Card]
  
  # Define custom suit order (S > H > C > D)
  suit_order <- c("S" = 1, "H" = 2, "C" = 3, "D" = 4)
  deck$Suit_Order <- suit_order[as.character(deck$Suit)]
  
  # Define rank order (A > K > Q > J > 10 > 9 > ... > 2)
  rank_order <- c("A" = 1, "K" = 2, "Q" = 3, "J" = 4, "10" = 5, "9" = 6, 
                  "8" = 7, "7" = 8, "6" = 9, "5" = 10, "4" = 11, "3" = 12, "2" = 13)
  deck$Rank_Order <- rank_order[as.character(deck$Rank)]
  
  # Sort by: Count (descending), Rank_Order (ascending), Suit_Order (ascending)
  sorted_deck <- deck[order(-deck$Count, deck$Rank_Order, deck$Suit_Order), ]
  
  # Remove temporary columns
  sorted_deck$Count <- NULL
  sorted_deck$Suit_Order <- NULL
  sorted_deck$Rank_Order <- NULL
  
  return(sorted_deck)
}

# Sorting algorithm: Group by rank first, then sort by suit order (S,H,C,D)
sort_BlockShip2 <- function(deck) {
  # Define custom suit order (S > H > C > D)
  suit_order <- c("S" = 1, "H" = 2, "C" = 3, "D" = 4)
  deck$Suit_Order <- suit_order[as.character(deck$Suit)]
  
  # Define rank order (A > K > Q > J > 10 > 9 > ... > 2)
  rank_order <- c("A" = 1, "K" = 2, "Q" = 3, "J" = 4, "10" = 5, "9" = 6, 
                  "8" = 7, "7" = 8, "6" = 9, "5" = 10, "4" = 11, "3" = 12, "2" = 13)
  deck$Rank_Order <- rank_order[as.character(deck$Rank)]
  
  # Sort by: Rank_Order (ascending), Suit_Order (ascending)
  sorted_deck <- deck[order(deck$Rank_Order, deck$Suit_Order), ]
  
  # Remove temporary columns
  sorted_deck$Suit_Order <- NULL
  sorted_deck$Rank_Order <- NULL
  
  return(sorted_deck)
}

# Function to add multiple cards to the deck
add_bulk <- function(deck, card_codes) {
  # Handle single card as vector for consistency
  if (length(card_codes) == 1) {
    card_codes <- rep(card_codes, 1)
  }
  
  # Improved regex pattern to handle "10" properly
  valid_codes <- grepl("^(A|K|Q|J|[2-9]|10)[SHCD]$", card_codes)
  if (!all(valid_codes)) {
    invalid_codes <- card_codes[!valid_codes]
    stop("Invalid card code format(s): ", paste(invalid_codes, collapse = ", "), 
         ". Use format like 'AH', '2S', '10H'")
  }
  
  # Extract ranks and suits from card codes
  ranks <- gsub("([AKQJ2-9]|10)[SHCD]", "\\1", card_codes)
  suits <- gsub("(A|K|Q|J|[2-9]|10)", "", card_codes)
  
  # Create new cards data frame
  new_cards <- data.frame(
    Rank = factor(ranks, levels = levels(deck$Rank)),
    Suit = factor(suits, levels = levels(deck$Suit)),
    Card = card_codes,
    stringsAsFactors = FALSE
  )
  
  # Add to deck and sort
  new_deck <- rbind(deck, new_cards)
  return(sort_deck(new_deck))
}

# Function to remove multiple cards from the deck
remove_bulk <- function(deck, card_codes, max_remove = NULL) {
  # Handle single card as vector for consistency
  if (length(card_codes) == 1) {
    card_codes <- rep(card_codes, 1)
  }
  
  # Count current occurrences of each card
  card_table <- table(deck$Card)
  
  # Create removal plan
  removal_plan <- list()
  
  for (card_code in unique(card_codes)) {
    requested_removals <- sum(card_codes == card_code)
    current_count <- ifelse(card_code %in% names(card_table), 
                            card_table[card_code], 0)
    
    # Apply max_remove limit if specified
    if (!is.null(max_remove)) {
      actual_removals <- min(requested_removals, max_remove, current_count)
    } else {
      actual_removals <- min(requested_removals, current_count)
    }
    
    removal_plan[[card_code]] <- actual_removals
    
    if (actual_removals < requested_removals) {
      warning("Only removing ", actual_removals, " of ", requested_removals, 
              " requested for card ", card_code, " (only ", current_count, " available)")
    }
  }
  
  # Perform removals
  remaining_deck <- deck
  
  for (card_code in names(removal_plan)) {
    removals_needed <- removal_plan[[card_code]]
    
    if (removals_needed > 0) {
      card_indices <- which(remaining_deck$Card == card_code)
      
      if (length(card_indices) >= removals_needed) {
        # Remove the specified number of this card
        indices_to_remove <- card_indices[1:removals_needed]
        remaining_deck <- remaining_deck[-indices_to_remove, ]
      }
    }
  }
  
  return(remaining_deck)
}

# Function to remove entire suits or ranks
remove_sor <- function(deck, suits = NULL, ranks = NULL) {
  if (is.null(suits) && is.null(ranks)) {
    warning("No suits or ranks specified for removal")
    return(deck)
  }
  
  removal_indices <- logical(nrow(deck))  # Start with all FALSE
  
  # Remove specified suits
  if (!is.null(suits)) {
    suit_indices <- as.character(deck$Suit) %in% suits
    removal_indices <- removal_indices | suit_indices
  }
  
  # Remove specified ranks
  if (!is.null(ranks)) {
    rank_indices <- as.character(deck$Rank) %in% ranks
    removal_indices <- removal_indices | rank_indices
  }
  
  # Keep only cards that don't match removal criteria
  result_deck <- deck[!removal_indices, ]
  
  # Report what was removed
  removed_count <- sum(removal_indices)
  cat("Removed", removed_count, "cards from deck\n")
  
  if (!is.null(suits)) {
    cat("Removed suits:", paste(suits, collapse = ", "), "\n")
  }
  if (!is.null(ranks)) {
    cat("Removed ranks:", paste(ranks, collapse = ", "), "\n")
  }
  
  return(result_deck)
}
# =============================================================================
# 2. Idolrhythm
# =============================================================================

# Function to pick random cards using 1-1000 rolls (optimized for bulk operations)
Idolrhythm <- function(deck, num_draws = 1, show_results = TRUE) {
  # Generate random rolls
  rolls <- sample(1:1000, num_draws, replace = TRUE)
  
  # Calculate card indices using modulo
  deck_size <- nrow(deck)
  card_indices <- ((rolls - 1) %% deck_size) + 1
  
  # Get the selected cards
  selected_cards <- deck[card_indices, ]
  
  # Create results
  results <- data.frame(
    Draw = 1:num_draws,
    Roll = rolls,
    Card_Index = card_indices,
    Card_Code = as.character(selected_cards$Card),
    Rank = as.character(selected_cards$Rank),
    Suit = as.character(selected_cards$Suit),
    stringsAsFactors = FALSE
  )
  
  # Display results if requested
  if (show_results && num_draws <= 20) {
    # Show detailed results for small numbers of draws
    cat("=== RANDOM CARD DRAW RESULTS ===\n")
    for (i in 1:nrow(results)) {
      cat(sprintf("Draw %d: Rolled %d -> %s (%s of %s)\n", 
                  results$Draw[i], results$Roll[i], results$Card_Code[i],
                  results$Rank[i], results$Suit[i]))
    }
    cat("Total draws:", num_draws, "\n")
    cat("===============================\n")
  } else if (show_results) {
    # Show summary for large numbers of draws
    cat("=== BULK DRAW SUMMARY ===\n")
    cat("Total draws:", num_draws, "\n")
    cat("Unique cards drawn:", length(unique(results$Card_Code)), "\n")
    cat("Most common card:", names(which.max(table(results$Card_Code))), "\n")
    cat("First 5 draws:\n")
    print(head(results, 5))
    cat("...\n")
    cat("=========================\n")
  }
  
  return(results)
}

# =============================================================================
# Extra: Deck Matrix + Image Cache + Image Display
# =============================================================================
display_dmatrix<- function(deck) {
  # Get current suit and rank levels
  suits <- levels(deck$Suit)
  ranks <- levels(deck$Rank)
  
  # Create empty matrix with proper dimensions + extra row/column for totals
  deck_matrix <- matrix("", nrow = length(suits) + 1, ncol = length(ranks) + 1,
                        dimnames = list(c(suits, "∑ Suit"), c(ranks, "∑ Rank")))
  
  # Count cards by suit and rank
  card_counts <- table(deck$Suit, deck$Rank)
  
  # Fill matrix with counts or card codes
  for(suit in suits) {
    for(rank in ranks) {
      count <- card_counts[suit, rank]
      if(count > 0) {
        if(count == 1) {
          card_code <- deck$Card[deck$Suit == suit & deck$Rank == rank][1]
          deck_matrix[suit, rank] <- card_code
        } else {
          deck_matrix[suit, rank] <- paste0(count, "x")  # Shows "3x" instead of "(3)"
        }
      }
    }
  }
  
  # Calculate and add totals
  for(suit in suits) {
    suit_total <- sum(deck$Suit == suit)
    deck_matrix[suit, "∑ Rank"] <- paste0("[", suit_total, "]")
  }
  
  for(rank in ranks) {
    rank_total <- sum(deck$Rank == rank)
    deck_matrix["∑ Suit", rank] <- paste0("[", rank_total, "]")
  }
  
  # Grand total
  grand_total <- nrow(deck)
  deck_matrix["∑ Suit", "∑ Rank"] <- paste0("TOTAL: ", grand_total)
  
  cat("Deck Matrix -", grand_total, "cards total\n")
  return(deck_matrix)
}

# Load all images ONCE at startup
load_card_images <- function(image_folder = "card_images/") {
  suits <- c("S", "H", "C", "D")
  ranks <- c("A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  
  image_cache <- list()
  
  for(suit in suits) {
    for(rank in ranks) {
      card_code <- paste0(rank, suit)
      image_file <- file.path(image_folder, paste0(card_code, ".png"))
      
      if(file.exists(image_file)) {
        image_cache[[card_code]] <- readPNG(image_file)
        cat("Loaded:", card_code, "\n")
      } else {
        image_cache[[card_code]] <- NULL
        cat("Missing:", card_code, "\n")
      }
    }
  }
  
  return(image_cache)
}

display_cards <- function(deck, image_cache) {
  suits <- levels(deck$Suit)
  ranks <- levels(deck$Rank)
  
  # Create a list of all card instances (including duplicates)
  all_card_instances <- list()
  
  for(i in 1:nrow(deck)) {
    card_code <- as.character(deck$Card[i])
    suit <- as.character(deck$Suit[i])
    rank <- as.character(deck$Rank[i])
    
    all_card_instances[[length(all_card_instances) + 1]] <- list(
      suit = suit,
      rank = rank,
      code = card_code
    )
  }
  
  total_cards <- length(all_card_instances)
  
  # Calculate layout - use maximum cards per row to fill space
  max_cards_per_row <- min(13, total_cards)
  num_rows <- ceiling(total_cards / max_cards_per_row)
  
  # Set up the plot with zero margins
  par(mar = c(0, 0, 2, 0))
  
  # Calculate plot dimensions to exactly fit the cards with no whitespace
  card_width <- 0.5   # Tall aspect ratio (portrait)
  card_height <- 0.8  # Taller than wide
  
  plot_width <- max_cards_per_row * card_width
  plot_height <- num_rows * card_height
  
  plot(0, 0, type = "n", 
       xlim = c(0, plot_width), 
       ylim = c(0, plot_height),
       main = paste("Card Deck -", total_cards, "cards"),
       xlab = "", ylab = "", axes = FALSE, asp = 1)
  
  # Plot each card instance separately (including duplicates)
  for(card_idx in 1:length(all_card_instances)) {
    card <- all_card_instances[[card_idx]]
    
    # Calculate position (left to right, top to bottom, starting from top-left)
    row <- num_rows - ceiling(card_idx / max_cards_per_row)  # Start from top
    col <- ((card_idx - 1) %% max_cards_per_row)
    
    x_center <- col * card_width + card_width/2
    y_center <- row * card_height + card_height/2
    
    # Direct access to cached image - cards will touch each other
    rasterImage(image_cache[[card$code]], 
                x_center - card_width/2, y_center - card_height/2,
                x_center + card_width/2, y_center + card_height/2)
  }
}