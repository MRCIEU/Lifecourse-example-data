library(dplyr)
library(here)
library(ggplot2)

set.seed(1234)

# Simulate longitudinal dataset

growth_curve1 <- function(ages, slope, intercept) {
    y <- ages * slope + intercept
    y <- y + rnorm(length(ages), sd=y/(max(y)))
    return(y)
}

growth_curve2 <- function(x, phi1=20, phi2=-2.4, phi3=0.3) {
    g <- phi1 / (1 + exp(-(phi2 + phi3 * x)))
    g + rnorm(length(g), sd=g/(max(g)*10))
}

nid <- 1000
nrep <- 30

bmi <- lapply(1:nid, function(i) {
    tibble(
        id=i,
        age=sample(0:80, 30, replace=FALSE),
        value=growth_curve2(age, rnorm(1, mean=23, sd=5), -2.4, rnorm(1, mean=0.3, sd=0.1))
    ) %>% arrange(age)
}) %>% bind_rows()

ldl <- lapply(1:nid, function(i) {
    tibble(
        id=i,
        age=sample(0:80, 30, replace=FALSE),
        value=growth_curve2(age, rnorm(1, mean=140, sd=5), -2.4, rnorm(1, mean=1, sd=0.001))
    ) %>% arrange(age)
}) %>% bind_rows()

covs <- tibble(
    id = 1:nid,
    yob = runif(nid, 1920, 2020) %>% round(),
    sex = sample(1:2, nid, replace=T),
    parity = rpois(nid, lambda=1.4)+1,
    pc1 = rnorm(nid),
    pc2 = rnorm(nid),
    pc3 = rnorm(nid),
    pc4 = rnorm(nid),
    pc5 = rnorm(nid),
    pc6 = rnorm(nid),
    pc7 = rnorm(nid),
    pc8 = rnorm(nid),
    pc9 = rnorm(nid),
    pc10 = rnorm(nid)
)

write.table(bmi, file=here("phenotype", "bmi.txt"), row=F, col=T, qu=F)
write.table(ldl, file=here("phenotype", "ldl.txt"), row=F, col=T, qu=F)
write.table(covs, file=here("phenotype", "covs.txt"), row=F, col=T, qu=F)


ggplot(bmi %>% filter(id < 11), aes(x=age, y=value)) +
geom_point(aes(colour=as.factor(id))) +
geom_smooth(aes(colour=as.factor(id)), se=FALSE)


ggplot(ldl %>% filter(id < 11), aes(x=age, y=value)) +
geom_point(aes(colour=as.factor(id))) +
geom_smooth(aes(colour=as.factor(id)), se=FALSE)

