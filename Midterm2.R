rm(list=ls())

this.dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

##################
# Data Exploration
##################

# Loading Data

library(geostatsp)
library(geoR)
library(fields)
library(ggplot2)

data(wolfcamp)

x = wolfcamp$coords[,1]
y = wolfcamp$coords[,2]
z = wolfcamp$data

# Setting up grid for later

grid = NULL
for (i in seq(min(x) - 10, max(x) + 10, length = 100))
  for (j in seq(min(y) - 10, max(y) + 10, length = 100))
    grid = rbind(grid, c(i, j))
colnames(grid) = c("LONGITUDE", "LATITUDE")

# Exploratory Analysis/Visualization
  
  # Bubble plot

df = data.frame(lat = y, long = x, Z = z)
p = ggplot() + geom_point(data = df, aes(x = long, y = lat, size = Z, color = Z))
p = p + labs(x = "Longitude (km)", y = "Latitude (km)", size = "Piezometric Head (m)", colour = "Piezometric Head (m)") + coord_fixed()
p = p + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
p

  # Histograms

df = data.frame(X = z)
ggplot() + geom_histogram(data = df, aes(x = X, y = ..density..), binwidth = 25, color = "black", fill = "white") + ggtitle("Piezometric Head Distribution") + xlab("Piezometric Head (m)")
ggplot() + geom_histogram(data = df, aes(x = X, y = ..density..), binwidth = 150, color = "black", fill = "white") + ggtitle("Piezometric Head Distribution") + xlab("Piezometric Head (m)")

  # Plot against latitude and longitude

df = data.frame(X = x, Y = y, Z = z)
ggplot(df, aes(X, Z)) + geom_point() + ylab("Piezometric Head (m)") + xlab("Longitude (km)")
ggplot(df, aes(Y, Z)) + geom_point() + ylab("Piezometric Head (m)") + xlab("Latitude (km)")

  # Fit a linear trend

mod.z = lm(z ~ x + y)
summ.z = summary(mod.z)
summ.z

ggplot() + geom_histogram(data = df, aes(x = summ.z$residuals, y = ..density..), binwidth = 20, color = "black", fill = "white") + ggtitle("Linear Trend") + xlab("Piezometric Head Residuals (m)")
plot(x = mod.z$fitted.values, y = summ.z$residuals, pch = 16, main = "Linear Trend", xlab = "Piezometric Head Fitted Values (m)", ylab = "Piezometric Head Residuals (m)")
qqnorm(summ.z$residuals, pch = 16, main = "Linear Trend Normal Q-Q Plot")
qqline(summ.z$residuals, lwd = 2, lty = 2, col = "blue")

  # Linear trend without the outlier

cbind(x, y, z, summ.z$residuals)

mod.z.dim = lm(z[-78] ~ x[-78] + y[-78])
summ.z.dim = summary(mod.z.dim)
summ.z.dim

pred = cbind(1, x, y) %*% mod.z.dim$coefficients
resid = pred - z

ggplot() + geom_histogram(data = df, aes(x = resid, y = ..density..), binwidth = 19, color = "black", fill = "white") + ggtitle("Linear Trend without Outlier") + xlab("Piezometric Head Residuals (m)")
plot(x = pred, y = resid, pch = 16, main = "Linear Trend without Outlier", xlab = "Piezometric Head Fitted Values (m)", ylab = "Piezometric Head Residuals (m)")
qqnorm(resid, pch = 16, main = "Linear Trend without Outlier Normal Q-Q Plot")
qqline(resid, lwd = 2, lty = 2, col = "blue")

  # Check what happens if we do a log transform

mod.z.log = lm(log(z) ~ x + y)
summ.z.log = summary(mod.z.log)
summ.z.log

ggplot() + geom_histogram(data = df, aes(x = summ.z.log$residuals, y = ..density..), binwidth = 0.04, color = "black", fill = "white") + ggtitle("Log-Transformed Linear Trend") + xlab("Log Piezometric Head Residuals (log m)")
plot(x = mod.z.log$fitted.values, y = summ.z.log$residuals, pch = 16, main = "Log-Transformed Linear Trend", xlab = "Log Piezometric Head Fitted Values (log m)", ylab = "Log Piezometric Head Residuals (log m)")
qqnorm(summ.z.log$residuals, pch = 16, main = "Log-Transformed Linear Trend Normal Q-Q Plot")
qqline(summ.z.log$residuals, lwd = 2, lty = 2, col = "blue")

  # Log transform?

ggplot() + geom_histogram(data = df, aes(x = log(z), y = ..density..), binwidth = 0.06, color = "black", fill = "white") + ggtitle("Log-Transformed Piezometric Head") + xlab("Log Piezometric Head (log m)")

  # Square root transform?

ggplot() + geom_histogram(data = df, aes(x = sqrt(z), y = ..density..), binwidth = 1, color = "black", fill = "white") + ggtitle("Square Root-Transformed Piezometric Head") + xlab("Sqrt Piezometric Head (sqrt m)")


###################
# Residual Analysis
###################

  # Spatial plot of the residuals

z.trend = mod.z$fitted.values
z.resid = summ.z$residuals

df = data.frame(lat = y, long = x, Z = z.resid)
p = ggplot() + geom_point(data = df, aes(x = long, y = lat, size = Z, color = Z))
p = p + labs(x = "Longitude", y = "Latitude", size = "Residual Piezometric Head (m)", colour = "Residual Piezometric Head (m)") + coord_fixed()
p = p + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
p

  # Plot the residuals against latitude and longitude

df = data.frame(X = x, Y = y, Z = z.resid)
ggplot(df, aes(X, Z)) + geom_point() + ylab("Residual Piezometric Head (m)") + xlab("Longitude")
ggplot(df, aes(Y, Z)) + geom_point() + ylab("Residual Piezometric Head (m)") + xlab("Latitude")

  # Storing the data

wolfcamp.orig = wolfcamp
wolfcamp$data = z.resid

  # Prediction intervals for linear trend

pred = predict(mod.z, newdata = data.frame(x = grid[,1], y = grid[,2]), interval = "prediction", level = 0.95)
pred.point = pred[,1]
pred.sd = (pred[,3] - pred[,1]) / qnorm(0.975)


##################################
# Empirical Variogram & Covariance
##################################

  # Empirical Variogram

breaks = seq(0, 500, 25)
gamma.hat = variog(wolfcamp, breaks = breaks, estimator.type = "classical")
plot(x = gamma.hat$u, y = gamma.hat$v)
gamma.hat$n

breaks = seq(0, 300, 25)
gamma.hat = variog(wolfcamp, breaks = breaks, estimator.type = "classical")
plot(x = gamma.hat$u, y = gamma.hat$v)
gamma.hat$n

breaks = c(seq(0, 200, 25), 250, 300)

gamma.hat = variog(wolfcamp, breaks = breaks, estimator.type = "classical")
gamma.bar = variog(wolfcamp, breaks = breaks, estimator.type = "modulus")
gamma.hat$n

plot(x = gamma.hat$u, y = gamma.hat$v, pch = 19, ylim = c(0, max(gamma.hat$v, gamma.bar$v)), main = "Variogram Estimation", xlab = "Distance (km)", ylab = "Semivariance")
points(x = gamma.bar$u, y = gamma.bar$v, pch = 2, col = "red")
legend(100, 2500, pch = c(19,2), c("classic", "robust"), cex = 0.8, col = c("black", "red"))

  # Empirical covariance

get.points.indices = function(x, y, min.dist, max.dist)
{
  pts.1 = NULL
  pts.2 = NULL
  
  for (i in 1:length(x))
  {
    for (j in i:length(x))
    {
      dist = sqrt((x[i] - x[j])**2 + (y[i] - y[j])**2)
      if (min.dist < dist & dist <= max.dist)
      {
        pts.1 = c(pts.1, i)
        pts.2 = c(pts.2, j)
      }
    }
  }
  
  return(cbind(pts.1, pts.2))
}

compute.c.hat = function(x, y, z, breaks)
{
  if (breaks[1] > 0)
    breaks = c(0, breaks)
  
  n = NULL
  c = NULL
  
  for (i in 1:(length(breaks)-1))
  {
    points = get.points.indices(x, y, breaks[i], breaks[i+1])
    n = c(n, length(points[,1]))
    c = c(c, sum(z[points[,1]] * z[points[,2]]) / n[i])
  }
  
  u = NULL
  for (i in 1:(length(breaks)-1))
    u = c(u, (breaks[i] + breaks[i+1]) / 2)
  
  ret = list(u = u, c = c, n = n)
  return(ret)
}

# Compute & plot covariance

c.hat = compute.c.hat(x, y, wolfcamp$data, breaks)
plot(x = c.hat$u, y = c.hat$c, pch = 19, ylim = c(min(c.hat$c), max(c.hat$c)), main = "Covariance Estimation", xlab = "Distance (km)", ylab = "Covariance")

# Compute & plot methods of estimating variogram

c.0 = sum(wolfcamp$data**2) / length(wolfcamp$data)
plot(x = gamma.bar$u, y = gamma.bar$v, pch = 19, ylim = c(0, max(gamma.bar$v, c.0 - c.hat$c)), main = "Estimation of Variogram", xlab = "Distance (km)", ylab = "Semivariance")
points(x = c.hat$u, y = c.0 - c.hat$c, pch = 2, col = "red")
legend(100, 2000, pch = c(19,2), c("gamma.hat", "c.0 - c.hat"), cex = 0.8, col = c("black", "red"))


####################################
# Directional Variogram & Covariance
####################################

# Compute directional variogram

par(mfrow = c(1,1))
gamma.hat.a0 = variog(wolfcamp, estimator.type = "modulus", breaks = breaks, direction = 0)
gamma.hat.a45 = variog(wolfcamp, estimator.type = "modulus", breaks = breaks, direction = pi/4)
gamma.hat.a90 = variog(wolfcamp, estimator.type = "modulus", breaks = breaks, direction = pi/2)
gamma.hat.a135 = variog(wolfcamp, estimator.type = "modulus", breaks = breaks, direction = 3/4*pi)

# Plot directional variogram

par(mfrow = c(1,1))
max.x = max(c(gamma.hat.a0$u, gamma.hat.a45$u, gamma.hat.a90$u, gamma.hat.a135$u))
min.y = min(c(gamma.hat.a0$v, gamma.hat.a45$v, gamma.hat.a90$v, gamma.hat.a135$v))
max.y = max(c(gamma.hat.a0$v, gamma.hat.a45$v, gamma.hat.a90$v, gamma.hat.a135$v))
plot(gamma.hat.a0, col = "red", pch = 19, type = "o", xlim = c(0, max.x), ylim = c(0, max.y), main = "Directional Variogram", xlab = "Distance (km)", ylab = "Semivariance")
lines(gamma.hat.a45, col = "blue", pch = 19, type = "o")
lines(gamma.hat.a90, col = "green", pch = 19, type = "o")
lines(gamma.hat.a135, col = "orange", pch = 19, type = "o")
legend(80, 2800, col = c("red", "blue", "green", "orange"), c(0, 45, 90, 135), lwd = c(2, 2, 2, 2), cex = 0.8)

# Effect of decreasing tolerance angle to pi/16

gamma.hat.a0 = variog(wolfcamp, estimator.type = "modulus", breaks = breaks, direction = 0, tolerance = pi/16)
gamma.hat.a45 = variog(wolfcamp, estimator.type = "modulus", breaks = breaks, direction = pi/4, tolerance = pi/16)
gamma.hat.a90 = variog(wolfcamp, estimator.type = "modulus", breaks = breaks, direction = pi/2, tolerance = pi/16)
gamma.hat.a135 = variog(wolfcamp, estimator.type = "modulus", breaks = breaks, direction = 3/4*pi, tolerance = pi/16)
max.x = max(c(gamma.hat.a0$u, gamma.hat.a45$u, gamma.hat.a90$u, gamma.hat.a135$u))
min.y = min(c(gamma.hat.a0$v, gamma.hat.a45$v, gamma.hat.a90$v, gamma.hat.a135$v))
max.y = max(c(gamma.hat.a0$v, gamma.hat.a45$v, gamma.hat.a90$v, gamma.hat.a135$v))
plot(gamma.hat.a0, col = "red", pch = 19, type = "o", xlim = c(0, max.x), ylim = c(0, max.y), main = "Directional Variogram", xlab = "Distance (km)", ylab = "Semivariance")
lines(gamma.hat.a45, col = "blue", pch = 19, type = "o")
lines(gamma.hat.a90, col = "green", pch = 19, type = "o")
lines(gamma.hat.a135, col = "orange", pch = 19, type = "o")
legend(150, 11000, col = c("red", "blue", "green", "orange"), c(0, 45, 90, 135), lwd = c(2, 2, 2, 2), cex = 0.8)

# Manual function for directional covariance

get.points.indices.directional = function(x, y, min.dist, max.dist, angle, tolerance = pi/8)
{
  pts.1 = NULL
  pts.2 = NULL
  
  for (i in 1:(length(x)-1))
  {
    for (j in (i+1):length(x))
    {
      dist = sqrt((x[i] - x[j])**2 + (y[i] - y[j])**2)
      ang = atan((y[j] - y[i]) / (x[j] - x[i]))
      if (ang < 0)
        ang = ang + pi
      if (angle - tolerance < 0)
        angle.cond = (angle - tolerance <= ang & ang <= angle + tolerance) | (angle - tolerance + pi <= ang & ang <= angle + tolerance + pi)
      else if (angle + tolerance > pi)
        angle.cond = (angle - tolerance <= ang & ang <= angle + tolerance) | (angle - tolerance - pi <= ang & ang <= angle + tolerance - pi)
      else
        angle.cond = angle - tolerance <= ang & ang <= angle + tolerance
      if (angle.cond & min.dist < dist & dist <= max.dist)
      {
        pts.1 = c(pts.1, i)
        pts.2 = c(pts.2, j)
      }
    }
  }
  
  return(cbind(pts.1, pts.2))
}

compute.c.hat.directional = function(x, y, z, breaks, angle, tolerance = pi/8, angle.type = "radian")
{
  if (angle.type == "degree")
  {
    angle = angle * pi / 180
    tolerance = tolerance * pi / 180
  }
  
  if (breaks[1] > 0)
    breaks = c(0, breaks)
  
  n = NULL
  c = NULL
  
  for (i in 1:(length(breaks)-1))
  {
    points = get.points.indices.directional(x, y, breaks[i], breaks[i+1], angle, tolerance)
    n = c(n, length(points[,1]))
    c = c(c, sum(z[points[,1]] * z[points[,2]]) / n[i] / 2)
  }
  
  u = NULL
  for (i in 1:(length(breaks)-1))
    u = c(u, (breaks[i] + breaks[i+1]) / 2)
  
  ret = list(u = u, c = c, n = n)
  return(ret)
}

# Compute directional covariance

c.hat.a0 = compute.c.hat.directional(x, y, wolfcamp$data, breaks, 0)
c.hat.a45 = compute.c.hat.directional(x, y, wolfcamp$data, breaks, pi/4)
c.hat.a90 = compute.c.hat.directional(x, y, wolfcamp$data, breaks, pi/2)
c.hat.a135 = compute.c.hat.directional(x, y, wolfcamp$data, breaks, pi*3/4)

# Plot directional covariance

min.val = min(na.omit(c(c.hat.a0$c, c.hat.a45$c, c.hat.a90$c, c.hat.a135$c)))
max.val = max(na.omit(c(c.hat.a0$c, c.hat.a45$c, c.hat.a90$c, c.hat.a135$c)))
plot(x = c.hat.a0$u, y = c.hat.a0$c, pch = 19, type = "o", col = "red", ylim = c(min.val, max.val), main = "Directional Covariance Estimation", xlab = "Distance (km)", ylab = "Covariance")
lines(x = c.hat.a45$u, y = c.hat.a45$c, pch = 19, type = "o", col = "blue")
lines(x = c.hat.a90$u, y = c.hat.a90$c, pch = 19, type = "o", col = "green")
lines(x = c.hat.a135$u, y = c.hat.a135$c, pch = 19, type = "o", col = "orange")
legend(100, 900, col = c("red", "blue", "green", "orange"), c(0, 45, 90, 135), lwd = c(2, 2, 2, 2), cex = 0.8)

###################
# Variogram Fitting
###################

# Computes the sum of squared errors for the first three points for a given
# variogram
sse3 = function(v, u, points)
{
  
  u = u[1:3]
  points = points[1:3]
  
  tausq = v$nugget
  sigmasq = v$cov.pars[1]
  phi = 1/v$cov.pars[2]
  
  if (v$cov.model == "linear")
    pred = tausq + sigmasq * u
  else
  {
    if (v$cov.model == "spherical")
      pred = tausq + sigmasq * (3/2*phi*u - 1/2*(phi*u)**3)
    else
    {
      if (v$cov.model == "matern")
      {
        nu = v$kappa
        pred = tausq + sigmasq * (1 - ((phi * u)**nu)/(2**(nu-1) * gamma(nu)) * besselK(phi * u, nu))
      }
      else
      {
        if (v$cov.model == "wave")
          pred = tausq + sigmasq * (1 - sin(phi * u) / (phi * u))
      }
    }
  }
  
  return(sum((points-pred)**2))
  
}

# Cressie-style weights

gamma.hat.lin = variofit(gamma.bar, cov.model = "linear", fix.nugget = FALSE, weights = "cressie")
gamma.hat.sph = variofit(gamma.bar, cov.model = "spherical", fix.nugget = FALSE, weights = "cressie")
gamma.hat.mat = variofit(gamma.bar, cov.model = "matern", fix.nugget = FALSE, kap = 1.5, fix.kappa = FALSE, weights = "cressie")
gamma.hat.wav = variofit(gamma.bar, cov.model = "wave", fix.nugget = FALSE, weights = "cressie")

plot(x = gamma.bar$u, y = gamma.bar$v, ylim = c(0, max(gamma.bar$v)), pch = 19, main = "Variogram Estimation", xlab = "Distance (km)", ylab = "Semivariance")
lines(gamma.hat.lin, col = "red", lwd = 2, lty = 1)
lines(gamma.hat.sph, col = "blue", lwd = 2, lty = 2)
lines(gamma.hat.mat, col = "green", lwd = 2, lty = 3)
lines(gamma.hat.wav, col = "orange", lwd = 2, lty = 4)
legend(100, 2000, col = c("red", "blue", "green", "orange"), c("linear", "spherical", "matern", "wave"), lty = c(1,2,3,4), cex = 0.8, lwd = 2)

sse3(gamma.hat.lin, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.sph, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.mat, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.wav, gamma.bar$u, gamma.bar$v)

# npairs weights

gamma.hat.lin = variofit(gamma.bar, cov.model = "linear", fix.nugget = FALSE, weights = "npairs")
gamma.hat.sph = variofit(gamma.bar, cov.model = "spherical", fix.nugget = FALSE, weights = "npairs")
gamma.hat.mat = variofit(gamma.bar, cov.model = "matern", fix.nugget = FALSE, kap = 1.5, fix.kappa = FALSE, weights = "npairs")
gamma.hat.wav = variofit(gamma.bar, cov.model = "wave", fix.nugget = FALSE, weights = "npairs")

plot(x = gamma.bar$u, y = gamma.bar$v, ylim = c(0, max(gamma.bar$v)), pch = 19, main = "Variogram Estimation", xlab = "Distance (km)", ylab = "Semivariance")
lines(gamma.hat.lin, col = "red", lwd = 2, lty = 1)
lines(gamma.hat.sph, col = "blue", lwd = 2, lty = 2)
lines(gamma.hat.mat, col = "green", lwd = 2, lty = 3)
lines(gamma.hat.wav, col = "orange", lwd = 2, lty = 4)
legend(100, 2000, col = c("red", "blue", "green", "orange"), c("linear", "spherical", "matern", "wave"), lty = c(1,2,3,4), cex = 0.8, lwd = 2)

sse3(gamma.hat.lin, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.sph, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.mat, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.wav, gamma.bar$u, gamma.bar$v)

# equal weights

gamma.hat.lin = variofit(gamma.bar, cov.model = "linear", fix.nugget = FALSE, weights = "equal")
gamma.hat.sph = variofit(gamma.bar, cov.model = "spherical", fix.nugget = FALSE, weights = "equal")
gamma.hat.mat = variofit(gamma.bar, cov.model = "matern", fix.nugget = FALSE, kap = 1.5, fix.kappa = FALSE, weights = "equal")
gamma.hat.wav = variofit(gamma.bar, cov.model = "wave", fix.nugget = FALSE, ini.cov.pars = c(3919, 4.8), weights = "equal")

plot(x = gamma.bar$u, y = gamma.bar$v, ylim = c(0, max(gamma.bar$v)), pch = 19, main = "Variogram Estimation", xlab = "Distance (km)", ylab = "Semivariance")
lines(gamma.hat.lin, col = "red", lwd = 2, lty = 1)
lines(gamma.hat.sph, col = "blue", lwd = 2, lty = 2)
lines(gamma.hat.mat, col = "green", lwd = 2, lty = 3)
lines(gamma.hat.wav, col = "orange", lwd = 2, lty = 4)
legend(100, 2000, col = c("red", "blue", "green", "orange"), c("linear", "spherical", "matern", "wave"), lty = c(1,2,3,4), cex = 0.8, lwd = 2)

sse3(gamma.hat.lin, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.sph, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.mat, gamma.bar$u, gamma.bar$v)
sse3(gamma.hat.wav, gamma.bar$u, gamma.bar$v)

  # Lowest sse3 - spherical variogram, Cressie-style weights, optim minimization
  # Try with nlm minimization

gamma.hat.sph = variofit(gamma.bar, cov.model = "spherical", fix.nugget = FALSE, minimisation.function = "nlm", weights = "cressie")

plot(x = gamma.bar$u, y = gamma.bar$v, ylim = c(0, max(gamma.bar$v)), pch = 19, main = "Variogram Estimation", xlab = "Distance (km)", ylab = "Semivariance")
lines(gamma.hat.sph, col = "blue", lwd = 2, lty = 2)
legend(100, 2000, col = "blue", "spherical", lty = 2, cex = 0.8, lwd = 2)

sse3(gamma.hat.sph, gamma.bar$u, gamma.bar$v)

  # This sse3 is slightly lower, so this is the final variogram

# Check parameter estimates
gamma.hat.sph

#########
# Kriging
#########

kc = krige.control(type = "sk", obj.model = gamma.hat.sph)
sk = krige.conv(wolfcamp, locations = grid, krige = kc)

  # Fixing scale for plots
min.val = min(sk$predict - qnorm(0.975) * sqrt(sk$krige.var))
max.val = max(sk$predict + qnorm(0.975) * sqrt(sk$krige.var))

# Plotting point estimates of residuals

quilt.plot(grid, sk$predict, zlim = c(min.val, max.val), main = "Residual Point Estimates", xlab = "Longitude", ylab = "Latitude", legend.args = list(text = "Residual Piezometric Head (m)", side = 2))
points(wolfcamp, pch = 21, col = "white", add = TRUE)

# Plotting the kriging standard deviation of residuals

quilt.plot(grid, sqrt(sk$krige.var), main = "Standard Deviation of Residuals", xlab = "Longitude", ylab = "Latitude", legend.args = list(text = "Standard Deviation Residual Piezometric Head (m)", side = 2))
points(wolfcamp.orig, pch = 21, col = "white", add = TRUE)

# Plotting lower 95% confidence estimates of residuals

quilt.plot(grid, sk$predict - qnorm(0.975) * sqrt(sk$krige.var), zlim = c(min.val, max.val), main = "Residual Lower 95% Confidence Bound", xlab = "Longitude", ylab = "Latitude", legend.args = list(text = "Residual Piezometric Head (m)", side = 2))
points(wolfcamp.orig, pch = 21, col = "white", add = TRUE)

# Plotting upper 95% confidence estimates of residuals

quilt.plot(grid, sk$predict + qnorm(0.975) * sqrt(sk$krige.var), zlim = c(min.val, max.val), main = "Residual Upper 95% Confidence Bound", xlab = "Longitude", ylab = "Latitude", legend.args = list(text = "Residual Piezometric Head (m)", side = 2))
points(wolfcamp.orig, pch = 21, col = "white", add = TRUE)

  # Fixing scale for plots
min.val = min((pred.point + sk$predict) - (qnorm(0.975) * sqrt(sk$krige.var + pred.sd**2)))
max.val = max((pred.point + sk$predict) + (qnorm(0.975) * sqrt(sk$krige.var + pred.sd**2)))

# Plotting point estimates on the original scale

quilt.plot(grid, pred.point + sk$predict, main = "Point Estimates", xlab = "Longitude", ylab = "Latitude", legend.args = list(text = "Piezometric Head (m)", side = 2))
points(wolfcamp.orig, pch = 21, col = "white", add = TRUE)

# Plotting the kriging standard deviation on the original scale

quilt.plot(grid, sqrt(pred.sd**2 + sk$krige.var), main = "Standard Deviation", xlab = "Longitude", ylab = "Latitude", legend.args = list(text = "Standard Deviation of Piezometric Head (m)", side = 2))
points(wolfcamp.orig, pch = 21, col = "white", add = TRUE)

# Plotting lower 95% confidence estimates on original scale

quilt.plot(grid, (pred.point + sk$predict) - (qnorm(0.975) * sqrt(sk$krige.var + pred.sd**2)), zlim = c(min.val, max.val), main = "Lower 95% Confidence Bound", xlab = "Longitude", ylab = "Latitude", legend.args = list(text = "Piezometric Head (m)", side = 2))
points(wolfcamp.orig, pch = 21, col = "white", add = TRUE)

# Plotting upper 95% confidence estimates on original scale

quilt.plot(grid, (pred.point + sk$predict) + (qnorm(0.975) * sqrt(sk$krige.var + pred.sd**2)), zlim = c(min.val, max.val), main = "Upper 95% Confidence Bound", xlab = "Longitude", ylab = "Latitude", legend.args = list(text = "Piezometric Head (m)", side = 2))
points(wolfcamp.orig, pch = 21, col = "white", add = TRUE)


#####################
# Pressure Difference
#####################

# Plot of two points
df = data.frame(lat = y, long = x, Z = z)
p = ggplot() + geom_point(data = df, aes(x = long, y = lat, size = Z, color = Z))
p = p + labs(x = "Longitude (km)", y = "Latitude (km)", size = "Piezometric Head (m)", colour = "Piezometric Head (m)") + coord_fixed()
p = p + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
p

df = df[c(15,59),]
p = ggplot() + geom_point(data = df, aes(x = long, y = lat, size = Z, color = Z))
p = p + labs(x = "Longitude (km)", y = "Latitude (km)", size = "Piezometric Head (m)", colour = "Piezometric Head (m)") + coord_fixed()
p = p + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
p = p + xlim(min(x), max(x)) + ylim(min(y), max(y))
p = p + expand_limits(z = min(z))
p = p + lims(size = c(min(z), max(z)), color = c(min(z), max(z)))
p

true.diff = wolfcamp.orig$dat[15] - wolfcamp.orig$dat[59]
true.diff

loc1 = wolfcamp.orig$coords[15,]
loc2 = wolfcamp.orig$coords[59,]
locs = cbind(loc1, loc2)
sk = krige.conv(wolfcamp, locations = cbind(loc1, loc2), krige = kc)
pred = predict(mod.z, newdata = data.frame(x = locs[,1], y = locs[,2]), interval = "prediction", level = 0.95)
pred.point = pred[,1]
pred.sd = (pred[,3] - pred[,1]) / qnorm(0.975)

pred.diff = (pred.point[1] + sk$predict[1]) - (pred.point[2] + sk$predict[2])
pred.sd = sqrt(pred.sd[1]**2 + sk$krige.var[1] + pred.sd[2]**2 + sk$krige.var[2])
pred.diff
pred.sd

p = 1 - pnorm(pred.diff / pred.sd)
p





















