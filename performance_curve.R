# Rohan McLure
# Matrix algebra 

# Global time variable.
Time <- Sys.time()


# tests is the size of the data being input.
# samplesize is the number of trials allocated to each load size.
benchmark <- function(testsizes, nsamples)
{
	samplemeans <- c()
	
	for (i in 1:length(testsizes))
	{
		dataset = c()

		for (trial in 1:nsamples)
		{
			# Populates the entry for an input size with its trial time.
			dataset[trial] <- matrix_bench(testsizes[i])
		}
	
		samplemeans[i] <- mean(dataset)
	}

	pdf(file="performance_curve.pdf")
	plot(testsizes, samplemeans,xlabel="Input Size",ylabel="Computation Time (s)")

}

# Generates matrix A and vector b, solves for X = Ab, barplot contents of X.
matrix_bench <- function(size)
{
	A = rand_matrix(TRUE, size)
	b = rand_matrix(FALSE, size)

	# Excludes generation time from 
	module_time()

	# Solves matrix, plots contents of variable vector X.
	X <- solve(A,b)
	barplot(1:length(X),X)

	# Removing overhead, computation time for solve and plot.
	return(module_time())
}

rand_matrix <- function(is_matrix, slength)
{	
	n = slength
	
	if (is_matrix)
	{
		n <- n ** 2
	}
	
	# Generates a line of members for the matrix.
	members <- sample(-10:10, size=n, replace=TRUE)

	return( matrix((members), nrow=slength ))
}

# Returns dt for the last time this function was called, assigns time to global variable.
module_time <- function()
{
	dt <- Sys.time() - Time
	Time <- Time + dt
	return(dt)
}

benchmark(50*1:20, 10)
