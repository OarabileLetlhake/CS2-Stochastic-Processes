library(markovchain)

transition_matrix <- matrix(c(0.1, 0.9, .8, 0.2), nrow = 2, byrow=TRUE)

get_nth_order_matrix <- function(matrix, n){
	p <- diag(nrow(matrix))
		for(i in 1:n){
			p <- matrix %*% p
		}

	dimnames(p) <- dimnames(matrix)

	p

}

get_nth_order_matrix(transition_matrix, 2)

transition_states <- c('State_1', 'State_2')

mc <- new("markovchain", transitionMatrix = transition_matrix, states = transition_states, name = 'dummyChain')

analyse_matrix <- function(markovchain_obj){
	properties <- list(markovchain_obj@transitionMatrix, mc@states, period(markovchain_obj))
	properties
}


# --------------------------------------------------------------------------------------------------------------------------------

# Question 2.1
# i.

