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
			dataset[trial] <- divisibility_bench(testsizes[i])
		}
	
		samplemeans[i] <- mean(dataset)
	}

	pdf(file="performance_curve.pdf")
	plot(testsizes,samplemeans)

}

# Generates matrix A and vector b, solves for X = Ab, barplot contents of X.
matrix_bench <- function(size)
{
	A = rand_matrix(TRUE, size)
	b = rand_matrix(FALSE, size)

	# Excludes generation time from 
	module_time()

	# Solves matrix, plots contents of variable vector X.
	return system.time(X <- solve(A,b); barplot(1:length(X),X))
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

divisibility_bench <- function(size)
{
	data <- sample(-100:100, size, replace=TRUE)
	moduli <- sample(1:10, size, replace=TRUE)

	return (system.time(data %% moduli))
}


benchmark(50000*1:20, 10)
