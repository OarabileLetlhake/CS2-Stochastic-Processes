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
P <- matrix(c(0.2, 0.8, 0, 0.4, 0.5, 0.1, 0.85, 0, 0.15), nrow = 3, byrow=TRUE)

#ii.
mc <- new("markovchain", transitionMatrix = P, name = 'Q1MC')

#iii.
#a.
is.irreducible(mc)

#b.
period(mc)

#iv.
rowSums(mc@transitionMatrix)

#v.
#a.
P_4 <- c(0,1,0)*mc^4

P_4[1]

#b.
d <- c(.2, .3, .5)

P_8 <- d%*%mc^8

P_8[2]

#c.
find_stationary_dist <- function(trans_matrix){
	step_dist <- c(0.2, 0.3, 0.5)
	steps <- 0

	while(TRUE){

		step_dist <- step_dist %*% trans_matrix

		if(all(step_dist%*%trans_matrix == step_dist)){
			stationary_dist <- step_dist
			break
		}

		else{
			steps <- steps + 1
		}

	}

	print(steps)

	stationary_dist
}

(stationary_dist <- find_stationary_dist(mc@transitionMatrix))

steadyStates(mc)

# ------------------------------------------------------------------------------------------------
#2.2
#i.
past18 <- c(2,1,2,1,3,1,1,2,2,1,1,3,3,1,2,1,2,1)

fit1 <- markovchainFit(past18)

fit1$estimate
fit1$logLikelihood

#2.3
#i.
#a.
