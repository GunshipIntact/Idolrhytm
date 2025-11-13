# =============================================================================
# Testing FUNCTIONS
# =============================================================================
source("Decks.R")
source("Idolrhythm2.R")
source("Simulations.R")
source("Simulations2.R")
library(png)
library(grid)
library(gridExtra)
library(ggplot2)

# Load once at the beginning
image_cache <- load_card_images()

# Verify the sizes
print(paste("Deck 1 size:", nrow(deckf2)))  # 52

# Create a test deck with duplicates
test_deck <- create_deck()

# Add some duplicate cards for testing
test_deck <- rbind(test_deck, 
                   test_deck[1:3, ],  # Add 3 copies of first 3 cards
                   test_deck[10:12, ]) # Add 3 copies of cards 10-12

# Sort using custom algorithm
sorted_test_deck <- sort_Fantoms(test_deck)


# Sort the initial deck
deck <- sort_deck(deck)
deck <- add_card(deck, "2S")

#testing deck sorting
deck2 <- create_erratic_deck(100)
deck2 <- sort_BlockShip(deck2)
deck2 <- sort_BlockShip2(deck2)
deck2 <- sort_Fantoms(deck2)
display_cards(deck2,image_cache)

# Create a deck
deck <- create_deck()

# BULK ADDITIONS
# Add multiple cards at once
deck <- add_bulk(deck, c("AH", "AH", "5S", "5S", "5S", "5S", "5S", "5S", "10H"))

# REMOVE ENTIRE SUITS/RANKS

# Remove all Aces and Kings
deck <- remove_sor(deck, ranks = c("A", "K"))

# Remove all Diamonds and all 2s
deck <- remove_sor(deck, suits = "D", ranks = "2")

# Complex operations
deck <- create_deck()
deck <- add_bulk(deck, rep(c("AH", "5S"), times = c(3, 6)))  # Add 3 AH and 6 5S
deck <- remove_sor(deck, suits = "C")              # Remove all Clubs
deck <- remove_bulk(deck, c("AH", "AH"), max_remove = 2)     # Remove up to 2 AH

# Display the matrix
matrix_result <- display_dmatrix(deck)
print(matrix_result)


# =============================================================================
# Simulations Idolrhythm
# =============================================================================

# Create a deck
deck <- create_deck()

# Single draw
result <- Idolrhythm(deck)

# Multiple draws with detailed output
results <- Idolrhythm(deck, num_draws = 10, show_results = TRUE)

# Analyze the results
cat("Card distribution in 10 draws:\n")
print(table(results$Card_Code))

# =============================================================================
# Simulations Comparison
# =============================================================================


# Create test decks
deckf <- create_erratic_deck(72)
deckf2 <-  sort_Fantoms(add_bulk(deckf, rep(c("KH"), times = c(10))))
deckbs <- deckf
deckbs2 <- sort_BlockShip(add_bulk(deckf, rep(c("KH"), times = c(10))))


# Run with new horizontal layout
battle_results <- compare_deck_battles(deckf, deckf2, num_battles = 10000, show_plots = TRUE)

battle_results <- compare_deck_battles(deckbs, deckbs2, num_battles = 10000, show_plots = TRUE)

# For the original faceted plot
results <- compare_deck_battles(deckf, deckf2, num_battles = 1000, plot_type = "faceted")

# For the new candlestick plot
results <- compare_deck_battles(deckf, deckf2, num_battles = 1000, plot_type = "candlestick")
# Or try the faceted version by replacing the plot call in the main function
# Create decks with meaningful names
standard_deck <- create_deck()
custom_deck <- create_deck()
custom_deck <- add_cards(custom_deck, rep(c("KH", "8H", "AS"), times = c(3, 5, 2)))

# Run enhanced simulation with custom names
results <- compare_deck_battles_enhanced(
  deck1 = standard_deck,
  deck2 = custom_deck,
  num_battles = 10000,
  deck1_name = "Standard Deck",
  deck2_name = "Enhanced Deck"
)

# Display results with proper names
cat("Battle Summary:\n")
cat(results$summary$deck1_name, "wins:", results$summary$deck1_wins, "\n")
cat(results$summary$deck2_name, "wins:", results$summary$deck2_wins, "\n")
cat("Ties:", results$summary$ties, "\n")
# =============================================================================
# Idolrhythm 2
# =============================================================================


deck <- create_deck()

# Get clean data to work with
draw_data <- generate_draws(deck, num_draws = 1000)

# Now you can analyze it however you want
most_common_card <- names(which.max(table(deckf$Card_Code)))
rank_frequency <- table(draw_data$Rank)
suit_frequency <- table(draw_data$Suit)