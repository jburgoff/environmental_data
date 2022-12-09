install.packages("psych")
require(psych)
require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)
dat_all= merge(dat_bird, dat_habitat)

plot(x= dat_all$ba.tot, y= cbch_present_absent, ylab= "Likelihood of Chestnut-bk Chickadee Presence", xlab= "Total Basal Area", main="Plot of Chestnut-bk Chickadee Presence Versus Total Basal Area")
curve(logistic_midpoint_slope(x, midpoint = 50, slope = .5), add = TRUE)



plot(x= dat_all$ba.tot, y= bcch_present_absent, ylab= "Likelihood of Black-cap Chickadee Presence", xlab= "Total Basal Area", main= "Plot of Black-cap Chickadee Presence Versus Total Basal Area")
curve(logistic_midpoint_slope(x, midpoint = 5, slope = -.1), add = TRUE)


my_vec=dat_all$CEWA
my_vec>=1
cewa_present_absent= as.numeric(my_vec>=1)
plot(x = dat_all$elev, y = cewa_present_absent)

my_vec_2=dat_all$BCCH
bcch_present_absent= as.numeric(my_vec_2>=1)

plot(cbch_present_absent, data= dat_habitat)
# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}
require(psych)
pairs.panels(dat_habitat[, c("elev", "slope", "aspect", "ba.tot")])
sum(dat_all$GRJA)

my_vec_grja= dat_all$GRJA
grja_present_absent= as.numeric(my_vec_grja>=1)
sum(grja_present_absent)


