# Rohan McLure
# Initial R micro-benchmark

#x <- seq(1,10)
#y <- factorial (x)

#plot(x,y)

#print(paste("You're working in: ", getwd()))


# Matrix algebra subroutine
# Affine transformation matrices, solves.

Time <- Sys.time()


# tests is the size of the data being input.
# samplesize is the number of trials allocated to each load size.
benchmark <- function(tests, samplesize)
{
	ttypes <- c()
	for (i in 1:length(tests))
	{
		ttypes[i] <- c()

		for (trial in 1:samplesize)
		{
			ttypes[i][trial] <- matrix_bench(tests[i], TRUE)
		}
	}

	for (i in 1:length(ttypes))
	{
		ttypes[i] <- mean(ttypes[i])
	} 

	plot(tests, ttypes)
}

matrix_bench <- function(size, computt)
{
	# Minus overhead, the time taken to plot and solve the matrix equation.
	sigmat = 0

	A = rand_matrix(TRUE, size)
	b = rand_matrix(FALSE, size)

	print("Matrices generated")
	print(module_time())

	y <- solve(A,b)
	ta <- module_time()
	sigmat <- sigmat + ta
	print(ta)

	barplot(1:length(y),y)

	print("Cartesian plot with respect to discrete x")
	ta <- module_time()
	sigmat <- sigmat + ta
	print(ta)

	if (computt)
	{
		return(sigmat)
	}
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

module_time <- function()
{
	dt <- Sys.time() - Time
	Time <- Time + dt
	return(dt)
}

benchmark(2**(1:10), 10)
