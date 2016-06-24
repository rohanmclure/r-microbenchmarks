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
	print(solve(data))
}

read_file <- function(matrix, vector)
{
	A <- matrix(get_data("matrix"), nrow=2)
	#b <- matrix(get_data("vector"), nrow=2)
	

	return( A )
}

rand_matrix <- function(name)
{
	# Read.Table returns list on white-space
	return(read.table( paste(getwd(), name, sep = '/')) )
}

rand_matrix <- function(is_matrix, slength)
{	
	n = slength
	
	if (is_matrix)
	{
		n <- n ** 2
	}
	
	# Generates a line of members for the matrix.
	members <- sample(-10:10, n)

	print("Ermagersh it werkhs")

	return( matrix((members), slength ))
}

matrix_bench()

