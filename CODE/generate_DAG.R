# This script outputs a set of directed acyclic graphs. 

library(rethinking)
library(dagitty)

pdf('RESULTS/figures/DAG.pdf', 6, 4)
par(mfrow = c(2, 4))

dag = dagitty('dag{
              R -> O;
              I -> O;
              B}') 
coordinates(dag) <- list(x = c(R = 1, O = 2, B = 1, I = 2) , 
                         y = c(R = 2, O = 2, B = 1, I = 1))
drawdag(dag, xlim = c(0.8, 2.2), ylim = c(-2.2, -0.8))

dag = dagitty('dag{
              R -> O;
              I -> O;
              B -> I}') 
coordinates(dag) <- list(x = c(R = 1, O = 2, B = 1, I = 2) , 
                         y = c(R = 2, O = 2, B = 1, I = 1))
drawdag(dag, xlim = c(0.8, 2.2), ylim = c(-2.2, -0.8))

dag = dagitty('dag{
              R -> O;
              I -> O;
              B -> R}') 
coordinates(dag) <- list(x = c(R = 1, O = 2, B = 1, I = 2) , 
                         y = c(R = 2, O = 2, B = 1, I = 1))
drawdag(dag, xlim = c(0.8, 2.2), ylim = c(-2.2, -0.8))

dag = dagitty('dag{
              R -> O;
              I -> O;
              I -> R;
              B}') 
coordinates(dag) <- list(x = c(R = 1, O = 2, B = 1, I = 2) , 
                         y = c(R = 2, O = 2, B = 1, I = 1))
drawdag(dag, xlim = c(0.8, 2.2), ylim = c(-2.2, -0.8))

dag = dagitty('dag{
              R -> O;
              I -> O;
              B -> I;
              B -> R}') 
coordinates(dag) <- list(x = c(R = 1, O = 2, B = 1, I = 2) , 
                         y = c(R = 2, O = 2, B = 1, I = 1))
drawdag(dag, xlim = c(0.8, 2.2), ylim = c(-2.2, -0.8))

dag = dagitty('dag{
              R -> O;
              I -> O;
              B -> I;
              I -> R}') 
coordinates(dag) <- list(x = c(R = 1, O = 2, B = 1, I = 2) , 
                         y = c(R = 2, O = 2, B = 1, I = 1))
drawdag(dag, xlim = c(0.8, 2.2), ylim = c(-2.2, -0.8))
dag = dagitty('dag{
              R -> O;
              I -> O;
              B -> R;
              I -> R}') 
coordinates(dag) <- list(x = c(R = 1, O = 2, B = 1, I = 2) , 
                         y = c(R = 2, O = 2, B = 1, I = 1))
drawdag(dag, xlim = c(0.8, 2.2), ylim = c(-2.2, -0.8))

dag = dagitty('dag{
              R -> O;
              I -> O;
              B -> R;
              I -> R;
              B -> I}') 
coordinates(dag) <- list(x = c(R = 1, O = 2, B = 1, I = 2) , 
                         y = c(R = 2, O = 2, B = 1, I = 1))
drawdag(dag, xlim = c(0.8, 2.2), ylim = c(-2.2, -0.8))

dev.off()