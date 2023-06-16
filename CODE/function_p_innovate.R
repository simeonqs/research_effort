# Find better function to simulate p_innovate

b = -10:30

x = -7 + 0.3 * b

m = 1000000

p_innovate = function(x) 1 / (1 + exp(-x))

plot(b, p_innovate(x) * m, col = 1, type = 'l')

lines(b, exp(7 + 0.3 * b), col = 2)
