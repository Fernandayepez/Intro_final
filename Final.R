library(gtools)
library(MASS)

df1 <- read.csv("Body_masses.csv")

df1$Body.mass <- log(df1$Body.mass)

results_df <- data.frame()

num_permutations <- 2000

for (i in 1:num_permutations) {
  
  perms <- sample(df1$Body.mass, length(df1$Body.mass), replace = FALSE)
  
  results_df <- rbind(results_df, data.frame(Permutation = i, Body.mass = perms))
}


calculate_sum_squared_differences <- function(vector) {
  squared_differences <- (vector[-1] - vector[-length(vector)])^2
  sum_squared_differences <- sum(squared_differences)
  mean_value <- mean(vector)
  differences <- vector - mean_value
  sum_of_differences <- sum((differences)^{2})
  c_value <- sum_squared_differences / sum_of_differences
  return(c_value)
}

c_value_df <- data.frame(Result = numeric(0))

for (i in 1:num_permutations) {
  perm_values <- results_df$Body.mass[results_df$Permutation == i]
  result <- calculate_sum_squared_differences(perm_values)
  c_value_df <- rbind(c_value_df, data.frame(Result = result))
}


# fitting a normal distribution to my results
fit <- fitdistr(c_value_df$Result, densfun = "normal")
mean_estimate <- fit$estimate["mean"]
sd_estimate <- fit$estimate["sd"]

#Normal after log transformation
hist(c_value_df$Result, main = "Histogram of C Values", xlab = "C Values", ylab = "Frequency", col = "lightblue", border = "black")


# function that gives us the adjacency matrix from our original edge 
create_adjacency_matrix <- function(edges) {
  # Find the number of nodes, to get the size of our matrix
  nodes <- unique(as.vector(edges))
  num_nodes <- length(nodes)
  
  # Create an empty adjacency matrix
  adj_matrix <- matrix(0, nrow = num_nodes, ncol = num_nodes)
  
  # Recursively populate 
  for (i in 1:nrow(edges)) {
    #we look at the edge matrix and the right column are the from_nodes
    from_node <- edges[i, 1]
    #left columns
    to_node <- edges[i, 2]
    
    if (from_node %in% nodes) {
      #storing the first to_node as the right child, defined as 1
      adj_matrix[from_node, to_node] <- 1 
      #we remove from_node from the list
      nodes <- nodes[nodes != from_node]
    } else {
      #if it appears again then we assign the remaining child to the left, defined as 2
      adj_matrix[from_node, to_node] <- 2  
    }
  }
  
  return(adj_matrix)
}

get_edge_matrix <- function(adj_matrix) {
  num_nodes <- nrow(adj_matrix)
  edge_matrix <- matrix(NA, ncol = 2, nrow = 0)

  # We define what a child would look like in the adj matrix
  find_children <- function(parent, root) {
    children <- which(adj_matrix[parent, ] != 0)
    right_children <- children[adj_matrix[parent, children] == 1]
    left_children <- children[adj_matrix[parent, children] == 2]

    # We add the right children
    for (child in right_children) {
      edge <- c(parent, child)
      edge_matrix <<- rbind(edge_matrix, edge)

      # Recursion
      find_children(child, root)
    }

    # Then, add left children
    for (child in left_children) {
      edge <- c(parent, child)
      edge_matrix <<- rbind(edge_matrix, edge)

      # Recursion
      find_children(child, root)
    }
  }

   #Iterate through nodes to find roots and start find
  for (node in 1:num_nodes) {
    if (sum(adj_matrix[, node] != 0) == 0) {
      # Node is a root
      find_children(node, node)
    }
  }

  return(edge_matrix)
}

permute_rows <- function(vector, adj_matrix) {
  for (n in vector) {
    left_child <- which(adj_matrix[n, ] == 1)
    right_child <- which(adj_matrix[n, ] == 2)
    
    adj_matrix[n, left_child] <- 2
    adj_matrix[n, right_child] <- 1
  }
  
  return(adj_matrix)
}
permute_matrix_combinations <- function(matrix_combinations, adj_matrix) {
  permuted_matrices_list <- list()
  
  for (i in seq_along(matrix_combinations)) {
    matrix_comb <- matrix_combinations[[i]]
    
    
    # Apply the function for each column vector in the matrix combination
    permuted_matrix <- adj_matrix
    for (j in seq_along(matrix_comb)) {
      vector <- matrix_comb[j]
      permuted_matrix <- permute_rows(vector, permuted_matrix)
    }
    
    # Save the permuted matrix
    permuted_matrices_list[[i]] <- permuted_matrix
    
    
    #print(permuted_matrix)
  }
  
  return(permuted_matrices_list)
}


