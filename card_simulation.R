N = 26
pop = rep("red", N)
gen = 20
results = matrix(NA, nrow= gen, ncol = 3, dimnames = list(NULL, c("red", "black", "infections")))

n_infected = 1
for(i in 1:gen){
  pop = sample(pop)
  new_infected = floor(2.4 * sum(pop[1:n_infected] == "red"))
  if(n_infected == 0) break
  pop[1:n_infected] = "black"
  n_infected = new_infected
  counts = table(pop)
  results[i, "red"] = counts["red"]
  results[i, "black"] = counts["black"]
  results[i, "infections"] = new_infected
}

plot(1:gen, results[,"red"], col = "red", pch = 19, ylim = c(0, N))
points(1:gen, results[,"black"], col = "black", pch = 19)
points(1:gen, results[,"infections"], col = "orange", pch = 19)
