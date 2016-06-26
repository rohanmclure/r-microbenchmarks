# Rohan McLure
# Matrix algebra 

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

		print(dataset)
	
		samplemeans[i] <- mean(dataset)
	}

	pdf(file="performance_curve.pdf")
	plot(testsizes,samplemeans)

}

# Generates matrix A and vector b, solves for X = Ab, barplot contents of X.
matrix_arithmetic_bench <- function(size)
{
	A <- rand_matrix(TRUE, size)
	b <- rand_matrix(FALSE, size)

	# Solves matrix, plots contents of variable vector X.
	dt <- system.time(X <- solve(A,b))[3]
	dt <- dt + system.time(barplot(1:length(X),X))[3]
	return (dt)
}

# Multiplies matrices A and B
matrix_multiplication_bench <- function(size)
{
	A <- rand_matrix(TRUE, size)
	B <- rand_matrix(TRUE, size)

	return (system.time(A %*% B)[3])
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

	return (system.time(data %% moduli)[3])
}

incrementation_bench <- function(size)
{
	# Vector of 0's of amount size.
	data <- rep(0,size)

	return (system.time(data <- data + 1))
}

vector_column_synthesis_bench <- function(size)
{
	vectors <- c()	

	for (i in 1:size)
	{
		vectors[i] <- rand_matrix(FALSE, size)
	}

	return (system.time(cbind(vectors)))
}


benchmark(500000*10:30, 2)
