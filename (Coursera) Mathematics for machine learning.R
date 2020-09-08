############################################################
### Power series (Taylor series, Maclaurin series)
############################################################
# f(x) = exp(x^2), about x=0, using the the Maclaurin series formula, 
# obtain an approximation up to the first three non zero terms
x <- seq(from = -2, to = 2, by = 0.1)
df <- data.frame(x = x, y = exp(x^2), g = 1 + x^2 + x^4)
ggplot(df) + geom_line(aes(x = x, y = y), color = 'blue') + 
  geom_line(aes(x, g), color = "red")

# Use the Taylor series formula to approximate the first three terms of the function 
# f(x)=1/x, expanded around the point p=4.
x <- seq(from = -5, to = 5, by = 0.1)
y <- 1/x
df <- data.frame(x = x, y = y, 
                 g2 = 1/4 - (x-4)/16 + ((x-4)^2)/64,
                 g3 = 1/4 - (x-4)/16 + ((x-4)^2)/64 - ((x-4)^3)/256)
ggplot(df) + 
  geom_line(aes(x,y), color = 'blue') + 
  geom_line(aes(x,g2), color = 'red') +
  geom_line(aes(x,g3), color = 'green')

# By finding the first three terms of the Taylor series shown above for the function 
# f(x)=ln(x) (green line) about x=10, determine the magnitude of the difference 
# of using the second order taylor expansion against the first order Taylor 
# expansion when approximating to find the value of f(2).
x <- seq(from = 0, to = 20, by = 0.1)
y <- log(x)
p <- 10
df <- data.frame(x = x, y = y, 
                 g1 = log(p) + (1/p)*(x-p),
                 g2 = log(p) + (1/p)*(x-p) - (1/2)*(1/p^2)*(x-p)^2)
ggplot(df) + 
  geom_line(aes(x,y), color = 'blue') + 
  geom_line(aes(x,g1), color = 'red') +
  geom_line(aes(x,g2), color = 'green')
x2 <- 2
delta_g_x2 <- (1/2)*(1/p^2)*(x2-p)^2; delta_g_x2

# For the function f(x)=xsin(x) shown below, determine what order approximation 
# is shown by the orange curve, where the Taylor series approximation was centered about x=0.
x <- seq(from = -10, to = 10, by = 0.1)
y <- x * sin(x)
df <- data.frame(x = x, y = y, 
                 g2 = x^2, 
                 g4 = x^2 - x^4/6,
                 g6 = x^2 - x^4/6 + x^6/120)
ggplot(df) + 
  geom_line(aes(x,y), color = "blue") +
  geom_line(aes(x,g2), color = "pink") +
  geom_line(aes(x,g4), color = "red") +
  geom_line(aes(x,g6), color = "orange") + 
  ylim(-5,10)

# Find the first four non zero terms of the Taylor expansion for the 
# function f(x)=exp(x)+x+sin(x) about x=0.
x <- seq(from = -4, to = 4, by = 0.1)
y <- exp(x)+x+sin(x)
df <- data.frame(x = x, y = y,
                 g0 = 1,
                 g1 = 1 + 3*x,
                 g2 = 1 + 3*x + x^2/2, 
                 g4 = 1 + 3*x + x^2/2 + x^4/24,
                 g6 = 1 + 3*x + x^2/2 + x^4/24 + x^5/60)
ggplot(df) + 
  geom_line(aes(x,y), color = "blue") +
  geom_line(aes(x,g0), color = "pink") +
  geom_line(aes(x,g1), color = "red") +
  geom_line(aes(x,g2), color = "orange") + 
  geom_line(aes(x,g4), color = "green") +
  geom_line(aes(x,g6), color = "magenta") +
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = 0, color = "black")
  ylim(-10,60)

# In this quiz we shall explore using the Newton-Raphson method for root finding
x <- seq(from = -4, to = 4, by = 0.1)
y <-  x^6/6 - 3*x^4 - 2*x^3/3 + 27*x^2/2 + 18*x-30
f <- function(x){x^6/6 - 3*x^4 - 2*x^3/3 + 27*x^2/2 + 18*x-30}
dydx <- x^5 - 12*x^3 - 2*x^2 + 27*x + 18
dfdx <- function(x){x^5 - 12*x^3 - 2*x^2 + 27*x + 18}
d <- data.frame(x = x, y = y, dydx = dydx)
ggplot(d) + 
  geom_line(aes(x,y), color = "blue") +
  geom_line(aes(x,dydx), color = "red") +
  geom_hline(yintercept = 0, color = "grey") + geom_vline(xintercept = 0, color = "grey")
# We'll first try to find the location of the root near x=1
# By using x0=1 as a starting point and calculating −f(1)/f′(1) by hand, 
# find the first iteration of the Newton-Raphson method, i.e., find x1
x0 <- 1
x1 <- x0 - f(x0)/dfdx(x0); x1
# Let's use code to find the other root, near x=−4
x_i <- -4.0
d <- data.frame(x = c(x_i), f = c(f(x_i))); d
for (i in seq(from = 0, to = 20, by = 1)) {
  x_i <- x_i - f(x_i)/dfdx(x_i)
  d[nrow(d) + 1,] <- c(x_i, f(x_i))
}
d

# Let's explore where things can go wrong with Newton-Raphson
# Since the step size is given by δx=−f(x)/f′(x) , this can get big when f'(x) is very small. In fact 
# f′(x) is exactly zero at turning points of f(x). This is where 
# Newton-Raphson behaves the worst since the step size is infinite.
x_i <- 1.99
d <- data.frame(x = c(x_i), f = c(f(x_i))); d
for (i in seq(from = 0, to = 20, by = 1)) {
  x_i <- x_i - f(x_i)/dfdx(x_i)
  d[nrow(d) + 1,] <- c(x_i, f(x_i))
}
d



