# Rohan McLure


# tests is the size of the data being input.
# samplesize is the number of trials allocated to each load size.
benchmark <- function(test, testsizes, nsamples)
{
	samplemeans <- c()

	for (i in 1:length(testsizes))
	{
		dataset = c()

		for (trial in 1:nsamples)
		{
			# Populates the entry for an input size with its trial time.
			dataset[trial] <- do.call(test,list(testsizes[i]))
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
	dt <- system.time(X <- solve(A,b))[1]
	dt <- dt + system.time(barplot(1:length(X),X))[1]
	return (dt)
}

# Multiplies matrices A and B
matrix_multiplication_bench <- function(size)
{
	A <- rand_matrix(TRUE, size)
	B <- rand_matrix(TRUE, size)

	return (system.time(A %*% B)[1])
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

# Reasonable test data: 5000000*30:50
divisibility_bench <- function(size)
{
	data <- sample(-100:100, size, replace=TRUE)
	moduli <- sample(1:10, size, replace=TRUE)

	return (system.time(data %% moduli)[1])
}

# Reasonable test data: 5000000*30:50
incrementation_bench <- function(size)
{
	# Vector of 0's of amount size.
	data <- rep(0,size)

	return (system.time(data <- data + 1)[1])
}

# 5000000*30:50
multiplication_bench <- function(size)
{
	data <- sample(-100:100, size, replace=TRUE)
	operand <- sample(-40:40)

	return(system.time(operand * data)[1])
}

logic_bench <- function(size)
{
	data <- (sample(0:1, size,replace=TRUE) %% 2)
	
}

#
#vector_column_synthesis_bench <- function(size)
#{
#	vectors <- c()	
#
#	for (i in 1:size)
#	{
#		vectors[i] <- rand_matrix(FALSE, size)
#	}
#
#	return (system.time(cbind(vectors))[1])
#}

# 5000000*15:25
significant_bench <- function(size)
{
	data <- runif(size, -10.0,10.0)
	sf <- sample(1:8, size, replace=TRUE)

	return(system.time(round(data,sf))[1])
}

# Function calls:

aggregate <- 0.0

# Allow tests to range 0.1 to 0.2 on Intel
aggregate <- aggregate + benchmark(multiplication_bench, 5000000*6:14, 1)
aggregate <- aggregate + benchmark(significant_bench, 100000*14:28, 1)
aggregate <- aggregate + benchmark(matrix_arithmetic_bench, 60*10:15, 1)
aggregate <- aggregate + benchmark(incrementation_bench, 5000000*8:23, 1)
aggregate <- aggregate + benchmark(matrix_multiplication_bench, 6*87:98, 1)
aggregate <- aggregate + benchmark(divisibility_bench, 1000000*8:16, 1)


