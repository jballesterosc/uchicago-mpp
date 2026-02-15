# Set the seed, so we all get the same results 
set.seed(60637)

# We create the population like we have before: 
pop <- rnorm(n = 100000, mean = 70, sd = 10)

# Define key parameters:
true.effect <- 5
# change to 500, then 5,000 to see how distribution changes
sample.size <- 50


# Prepare an empty list of p-values
p.values <- rep(NA, 10000)
for(i in 1:10000){
    # Take a sample
    our.samp <- sample(pop, size = sample.size)
    # Turn the vector into a data frame 
    df <- data.frame(our.samp)
    # Assign a random value between 0 and 1 to each "person" in our sample
    df$random_0_1 <- runif(sample.size)
    # Assign those with values above 0.5 into the treatment group 
    # This will roughly split the sample into 50% in each group 
    df$treatment.group <- as.numeric(df$random_0_1 > 0.5)
    # Create the post-treatment outcome by adding the treatment effect, but
    # only for those in the treatment group 
    df$outcome.post <- df$our.samp + true.effect*df$treatment.group
    # Run a t-test between the two groups (alternatively, we could have run a regression)
    evaluating <- t.test(df[df$treatment.group == 1,]$outcome.post, 
                         df[df$treatment.group == 0,]$outcome.post, 
                         alternative = 'two.sided')
    p.values[i] <- evaluating$p.value
}

hist(p.values, breaks=20)

