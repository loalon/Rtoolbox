# Data generating process
library(MASS)
set.seed(6543)
n <- 500
mu <- c(1,-2,3,-1,3,4)
Sigma <- diag(rep(1,length(mu)))
Sigma[3,1] <- Sigma[1,3] <- 0.1
Sigma[4,6] <- Sigma[6,4] <- 0.1
X <- as.data.frame(mvrnorm(n, mu=mu, Sigma=Sigma))

# PCA
pca <- princomp(X, scores=T, cor=T)

# Scores
scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

# Loadings
loads <- pca$loadings

# Scale factor for loadings
scale.loads <- 5

# 3D plot
library(plotly)
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers",
            marker = list(color=y, 
                          colorscale = c("#FFE1A1", "#683531"), 
                          opacity = 0.7)) 

for (k in 1:nrow(loads)) {
  x <- c(0, loads[k,1])*scale.loads
  y <- c(0, loads[k,2])*scale.loads
  z <- c(0, loads[k,3])*scale.loads
  p <- p %>% add_trace(x=x, y=y, z=z,
                       type="scatter3d", mode="lines",
                       line = list(width=8),
                       opacity = 1) 
}
print(p)


add_markers(~x, ~y) %>%
  add_annotations( x = ~arrow.x.end,
                   y = ~arrow.y.end,
                   xref = "x", yref = "y",
                   axref = "x", ayref = "y",
                   text = "",
                   showarrow = T,
                   ax = ~arrow.x.start,
                   ay = ~arrow.y.start)

p %>% add_markers(~x, ~y, ~z)
p %>% add_annotations(x =3,
                      y = 3,
                      z =3,
                      xref = "x", yref = "y", zref="z",
                      axref = "x", ayref = "y", azref="z",
                      text = "",
                      showarrow = T,
                      ax =0,
                      ay = 0,
                      az=0)
print