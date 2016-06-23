# Rohan McLure
# Initial R micro-benchmark

#x <- seq(1,10)
#y <- factorial (x)

#plot(x,y)

#print(paste("You're working in: ", getwd()))


# Matrix algebra subroutine
# Affine transformation matrices, solves.
matrix_bench <- function()
{
	data <- read_file("matrix","vector")
	
	#A <- data[1]
	#b <- data[2]

	#A <- matrix(c(10,4,-6,3),2)

	print(solve(A))
}

read_file <- function(matrix, vector)
{
	A <- matrix(get_data("matrix"), nrow=2)
	#b <- matrix(get_data("vector"), nrow=2)
	

	return( A )
}

get_data <- function(name)
{
	# Read.Table returns list on white-space
	return(read.table( paste(getwd(), name, sep = '/')) )
}

rand_data <- function(is_matrix, slength)
{	
	n = slength
	
	if (is_matrix)
	{
		n <- n ** 2
	}
	
	members <- c()
	
	# Generates a line of members for the matrix.
	for (i in 1:n)
	{
		members[i] <- sample(-10:10)
	}

	return( matrix(members), slength )
}

matrix_bench()

