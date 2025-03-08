# Load necessary libraries
library(animint2)
library(ggplot2)
library(data.table)

# Define Poisson means and generate probability values
poisson.mean.diff = 0.25
poisson.mean.vec = seq(0, 5, by=poisson.mean.diff)
quantile.max = 0.99

# Here we generate probability distributions for different means
poisson.prob.list = list()
for(poisson.mean in poisson.mean.vec) {
  label.max = qpois(quantile.max, poisson.mean)
  label = 0:label.max
  probability = dpois(label, poisson.mean)
  
  poisson.prob.list[[as.character(poisson.mean)]] = data.table(
    poisson.mean,
    label,
    probability,
    cum.prob=cumsum(probability) 
  )
}

# Convert list to a single data.table
poisson.prob = rbindlist(poisson.prob.list)

# Melt the data so that we can use a single `geom_point()`
poisson.prob.melted = melt(
  poisson.prob, id.vars = c("poisson.mean", "label"), 
  measure.vars = c("probability", "cum.prob"),
  variable.name = "category", value.name = "value"
)

poisson_viz = list(
  poisson = ggplot(poisson.prob.melted, aes(x = label, y = value, color = category)) +
    geom_point(size = 3, alpha = 0.7) + 
    geom_line(aes(group = category), linetype = "dashed") + 
    labs(title = "Poisson Regression Visualization",
         x = "Label (Counts)",
         y = "Probability") +
    theme_minimal() +
    facet_wrap(~ poisson.mean)  
)

# Render the visualization again
animint2dir(poisson_viz, "poisson_regression_viz")

