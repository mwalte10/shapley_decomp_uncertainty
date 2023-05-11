library(data.table)
library(ggplot2)

out_dir = 'C:/Users/mwalters/Documents/UNAIDS/decomp/test/results/'

##Model that will be decomposed
model <- function(a = 0.5, b = 0.5, x){
  y <- a * x^2 + a * b * x + b * x
  return(y)
  
}

##Parameters that contribute to uncertainty
var_vec = c('a', 'b')

#distribution of these parameters 
avec <- rnorm(100, mean = 0.5, sd =0.1)
bvec <- rnorm(100, mean = 0.5, sd = 0.05)

##Generate all combinations that need to be run 
input = vector('list', length = length(var_vec))
names(input) <- var_vec
for(i in 1:length(var_vec)){
  ##1: standard variation
  ##0: no variation
  input[[i]] <- c(1,0)
}
input <- expand.grid(input)
input <- data.table(input)
input[,index := 1:nrow(input)]


##Run the different scenarios
for(scenario in 1:nrow(input)){
    if(!dir.exists(paste0(out_dir, scenario))){
      dir.create(paste0(out_dir, scenario), recursive = T)
    }
    
    out_sd <- array(dim = c(10,100))
    for(i in 1:100){
      a = 0.5
      b = 0.5
      
      if(input[scenario,  b]){
        b = bvec[i]
      }
      
      if(input[scenario, a]){
        a = avec[i]
      }
      
      out_sd[,i] <-  model(a, b, x = 1:10)
      
    }

    saveRDS(out_sd, file = paste0(out_dir, scenario, '.RDS'))
    print(scenario)
  }


x =  lapply(1:length(var_vec), function(index){
  var = var_vec[index]
  stvar <- input[get(var) == 1,]
  no_var <- input[get(var) == 0,]
  stvar[,pair := paste0(1:nrow(stvar))]
  no_var[,pair := paste0(1:nrow(stvar))]
  
  pairwise <- merge(stvar[,.(`standard var index` = index, pair)], no_var[,.(`no var index` = index, pair)], by = c('pair'))
  pairwise[,pair := as.integer(pair)]
  
  ##get weights, for ease of calculation pull these from the inputs list where the variable is being altered
  weights = input[get(var) == 0,]
  weights[,index := NULL]
  weights[,c := rowSums(weights)]
  weights[,n := length(var_vec)]
  weights[,weight := (factorial(c) * factorial(n - c - 1)) / factorial(n)]
  assertthat::assert_that(sum(weights$weight) == 1, msg = 'Total weights for this variable are equal to one')
  
  weights[,pair := 1:nrow(weights)]
  
  ##Attach weights to pairwise dt
  pairwise <- merge(pairwise, weights[,.(pair, weight)], by = 'pair')
  pairwise[,variable := var]
})
x <- data.table(rbindlist(x))


######Find contribution to outcome from each parameter
dt <- data.table()
for(i in 1:length(var_vec)){
  y <- x[variable == unique(x$variable)[i]]
  out_vec <- rep(0, 10)
  for(pair.x in 1:nrow(y)){
    a <- readRDS(paste0(out_dir, y[pair == pair.x,`standard var index`],'.RDS'))
    b <- readRDS(paste0(out_dir, y[pair == pair.x,`no var index`],'.RDS'))
    
    ##Can substitute in with whatever outcome you are decomposing, here: the difference between the 97.5 and 2.5 percentile
    range_a = apply(a, MARGIN = 1, FUN = function(x) quantile(x, 0.975) - quantile(x, 0.025))
    range_b = apply(b, MARGIN = 1, FUN = function(x) quantile(x, 0.975) - quantile(x, 0.025))
    
    out <- range_a - range_b
    out <- out * y[pair.x,weight]
    
    out_vec <- out_vec + out
    
  }
  dt <- rbind(dt, data.table(cbind(x = 1:10, out = out_vec, var = unique(x$variable)[i])))
}

###Calculate total uncertainty from decomposed part
dt[, out := as.numeric(out)]
dt <- dcast(dt, x ~ var, value.var = 'out')
dt[,tot := a + b]
dt = dt[order(as.integer(x)),]

##compare to expected total uncertainty (will always be the first index of the index table)
baseline <- readRDS(paste0(out_dir, '/1.RDS'))
lower = apply(baseline, MARGIN = 1, FUN = function(x) quantile(x, 0.025))
upper = apply(baseline, MARGIN = 1, FUN = function(x) quantile(x, 0.975))
obs <- upper - lower
dt[,obs := obs]

##Check that difference is approx equal to zero
dt[,diff := obs - tot]


dt2 <- data.table()
for(i in 1:4){
  baseline <- readRDS(paste0(out_dir,i,'.RDS'))
  lower = apply(baseline, MARGIN = 1, FUN = function(x) quantile(x, 0.025))
  upper = apply(baseline, MARGIN = 1, FUN = function(x) quantile(x, 0.975))
  mean = apply(baseline, MARGIN = 1, FUN = 'mean')
  dt_example <- data.table(x = 1:10, scenario = i, mean , lower, upper)
  dt2 <- rbind(dt2, dt_example)
}

##Plot all different decomposition scenarios
ggplot(dt2, aes(x, mean)) + geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4) + facet_wrap(~scenario) +
  theme_bw() + labs(x = '', y = '') + 
  theme(legend.position = "bottom", strip.text = element_text(size = 13), plot.title = element_text(size = 16), 
        axis.text = element_text(size = 12), axis.title = element_text(size = 14,  face = "bold"), legend.text = element_text(size = 12), 
        panel.background = element_rect(fill = NA,color = "black" ),   strip.background = element_blank(),
        strip.text.x = element_blank()) 






