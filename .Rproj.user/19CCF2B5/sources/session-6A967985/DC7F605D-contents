n_samples_1= 17
n_samples_2= 30
n_samples_3= 300
n_samples_4= 3000
pop_sd = 2.4
pop_mean = 10.4

dat_1 = rnorm(n = n_samples_1, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples_2, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples_3, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples_4, mean = pop_mean, sd = pop_sd)


require(here)
png(
  filename = here("lab_04_hist_01.png"),
  width = 1500, height = 1600, 
  res = 180, units = "px")


hist(dat_1, main= "Histogram of 17 randomly generated numbers")
hist(dat_2, main= "Histogram of 30 randomly generated numbers")
hist(dat_3, main= "HIstogram of 300 randomly generated numbers")
hist(dat_4, main= "Histogram of 3000 randomly generated numbers")
dev.off()


require(here)
svg(
  filename = here("norm_1.svg"),
  width = 10, height = 10)

x = seq(-100, 100, length.out = 1000)
y = dnorm(x, mean= 10.4, sd= 2.4)

plot(x, y, main = "Normal PDF with Mean = 10.4, SD = 2.4", type = "l", xlim = c(3, 19))
abline(h = 0)
dev.off()



require(here)
svg(
  filename = here("quad_plot.svg"),
  width = 10, height = 10)

set.seed(1)
dat_unif_1 = runif(n = 111, min = 0, max = 10)
set.seed(1)
dat_unif_2 = runif(n = 111, min = 0, max = 10)
set.seed(1)
dat_unif_3= runif(n= 111, min= 0, max = 10)
set.seed(1)
dat_unif_4= runif(n= 111, min= 0, max = 10)

hist(dat_unif_1)
hist(dat_unif_2)
plot(dat_unif_3, xlab= "x", ylab= "y")
plot(dat_unif_4, xlab= "x", ylab= "y")
dev.off()

n_pts = 111
x_min = 0
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)



require(here)
svg(
  filename = here("lin_function.svg"),
  width = 10, height = 10)

guess_x = 5
guess_y = 0
guess_slope = -0.001

plot(y ~ x, data = dat_random, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
dev.off()

y_predicted= line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random$y_predicted<- y_predicted

resids= dat_random$y_predicted- dat_random$y

dat_random$resids<- resids


hist(dat_random$resids, xlab= "residuals", main= "Histogram of linear model residuals")

plot(dat_random$resids~dat_random$y_predicted, xlab= "Predicted Values", ylab= "Residual Values", main= "Linear Model Residual Scatterplot")

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}