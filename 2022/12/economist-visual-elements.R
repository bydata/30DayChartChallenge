library(ggplot2)
library(grid)

png("plot.png", res = 300, width = 6.5, height = 6, units = "in")

# A basic plot, set plot.margin top to create space for the visual elements
ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Iris Sepal Length by Sepal Width") +
  theme(plot.margin = margin(t = 18, b = 6, l = 6, r = 6))

# Add red line
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#D13223", lwd = 2)
)
# Add red rectangle
grid.rect(
  x = 0,
  y = 1,
  width = 0.2,
  height = 0.05,
  gp = gpar(fill = "#D13223", col = NA)
)
invisible(dev.off())