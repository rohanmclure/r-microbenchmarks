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

		samplemeans[i] <- mean(dataset)
	}

	#pdf(file="performance_curve.pdf")
	#plot(testsizes,samplemeans)

	# For use in calculating aggregate data, only the median value of 'testsizes' is deployed, return numeric not vector.
	return (samplemeans[1])
}

# Generates matrix A and vector b, solves for X = Ab, barplot contents of X.
matrix_arithmetic_bench <- function(size)
{
	A <- rand_matrix(TRUE, size)
	b <- rand_matrix(FALSE, size)

	# Solves matrix
	return(system.time(solve(A,b))[1])
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

list_cycle <- function(data)
{
	n <- length(data)
	i <- 0
	first <- data[1]
	while (i < n)
	{
		data[i] <- data[i+1]
		i <- i + 1
	}
	data[n] <- first
}

# Reasonable test data: 5000000*30:50
divisibility_bench <- function(size)
{
	data <- sample(-100:100, size, replace=TRUE)
	moduli <- sample(1:10, size, replace=TRUE)

	# Calculates the divisibility of ten different moduli for each item in data.
	total <- 0.0
	for (n in 1:10)
	{
		moduli <- list_cycle(size)
		total <- total + system.time(data %% moduli)
	}

	return(total)
}

# Reasonable test data: 5000000*30:50
incrementation_bench <- function(size)
{
	# Vector of 0's of amount size.
	data <- rep(0,size)

	total <- system.time(data <- data + 1)[1]
}

# 5000000*30:50
multiplication_bench <- function(size)
{
	data <- sample(-100:100, size, replace=TRUE)
	operand <- sample(-40:40, 1)

	total <- system.time(operand * data)[1]

	for (i in 1:size)
	{
		a <- data[i]
		b <- data[size+1-i]

		total <- system.time(a * b)[i]
	}

	return(total)
}

# Enormous overhead!!!!
logic_bench <- function(size)
{
	data <- sample(0:1, size,replace=TRUE)
	data <- data == 1

	total <- 0.0

	#	Inverts all items
	total <- total + system.time(data <- !data)[1]


	# Loops through members, refers to ith and -ith members
	for (i in 1:size)
	{
		a <- data[i]
		b <- data[(size+1)-i]
		print(a)

		total <- total + system.time(a && !b)[1]
		total <- total + system.time(a || !b)[1]

	}

	return(total)
}

unsorted_linear_search <- function(size)
{
	# Creates a single 'TRUE' value in list.
	data <- rep(FALSE,size)
	data[sample(1:size,1)] <- TRUE

	return(system.time(for (i in data) {if(i){break}}))[1]
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

barplot_bench <- function(size)
{
	data <- sample(1:100, size, replace=TRUE)

	return (system.time(barplot(data))[1])
}

# 5000000*15:25
significant_bench <- function(size)
{
	data <- runif(size, -10.0,10.0)
	sf <- sample(1:8, size, replace=TRUE)

	return(system.time(round(data,sf))[1])
}

get_median_data <- function()
{
	mediantime <- c()

	# Benchmarks:
	tests <- list(list("multiplication_bench",5000000*6:14),
list("significant_bench",100000*14:28),
list("matrix_arithmetic_bench",2750),
list("incrementation_bench",5000000*8:23),
list("matrix_multiplication_bench",1825),
list("divisibility_bench",50000000),
list("barplot_bench", 2400000),
list("unsorted_linear_search",100000000))

	for (i in 1:length(tests))
	{
		name <- tests[[i]][[1]]
		data <- tests[[i]][[2]]

		# Prints the mean team for 10 samples of each benchmark.
		# The data given is the central or 'median' size range as per Intel Control.
		result = benchmark(name, median(data), 10)

		cat(sprintf("Test \"%s\" has a central average performance:\t%.3f\n", name, result))

		mediantime[i] <- result
	}

	return(mediantime)
}


# Returun aggregate test data as the sum of all central values.
#aggregate <- sum(get_median_data())
#cat(sprintf("Aggregate score is %.3fs \n", aggregate))
