DoubleDigestProblem <- function(XA, XB, XAB) {
  # Step 1: Generate all possible permutations of fragments for A and B
  permuted_A <- permn(XA)
  permuted_B <- permn(XB)
  
  # Initialize the list for solutions
  all_solutions <- list()
  
  # Step 2: Loop over all possible fragment combinations
  for (frag_A in permuted_A) {
    for (frag_B in permuted_B) {
      
      # Step 3: Calculate cumulative sum of fragments for A and B
      positions_A <- cumsum(c(0, frag_A))
      positions_B <- cumsum(c(0, frag_B))
      
      # Step 4: Combine and sort unique positions
      combined_positions <- unique(sort(c(positions_A, positions_B)))
      
      # Step 5: Calculate the successive differences between combined positions
      successive_diffs <- diff(combined_positions)
      
      # Step 6: Sort the differences and compare with XAB (should be XAB, not ??XAB)
      if (identical(sort(successive_diffs), sort(XAB))) {
        # Add the valid solution to the list of all solutions
        all_solutions[[length(all_solutions) + 1]] <- list(positions_A = positions_A, positions_B = positions_B)
      }
    }
  }
  
  # Return the solutions if any are found, otherwise return a message
  if (length(all_solutions) > 0) {
    return(all_solutions)
  } else {
    return("No valid solution found")   # when no valid solution is found
  }
}
# Example input
XA <- c(2, 3, 5, 10)
XB <- c(3, 7, 10)
XAB <- c(1, 2, 2, 5, 5, 5)

# Run the Double Digest Problem algorithm
solution <- DoubleDigestProblem(XA, XB, XAB)
print(solution)



install.packages("combinat")
library(combinat)
