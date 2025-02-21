# 1.  Using stratified random sampling (STSRS) and the strata identified on pages
# 6--9 of dataset.pdf for one of the populations that have data in a file
# *fv.csv, estimate the total and mean number of eforensics-fraudulent votes
# (variable "Nfraudmean") using a sample size of n=1500.  Compare the bounds on
# the error of estimation achieved using STSRS with n=1500 to the bounds from at
# least one simple random sampling (SRS) estimate for the same sample size.  Also
# compute STSRS estimates designed to achieve a bound of B=5 for the mean,
# evaluating what bound on the error of estimation your design actually achieves
# (when designing the sample you may treat population variables "NVoters",
# "NValid" and "Votes" as known).  Compare the sample size and actually achieved
# bound of the error of estimation for the STSRS method designed to have a bound
# of B=5 to at least one SRS sample designed to the same bound.  Note that you
# may choose to combine a few strata that have too few observations to be handy
# to work with.

# work with California 2006 data
setwd("C:/Ian School/Math/Stats 480")
dat <- read.csv("California2006GOVfv.csv", row.names=1);
names(dat);
dim(dat);
#> names(dat);
# [1] "cname"       "cnum"        "county"      "precinct"    "eftype"     
# [6] "NVoters"     "NValid"      "Votes"       "Ntfraudmean" "Nfraudmean" 
#> dim(dat);
#[1] 22820    10

N <- dim(dat)[1];
N;

n <- 1500;

### approach 1:  boost small stratum sample sizes

Ns <- xtabs(~ cname, dat);
Ns;

strata <- names(Ns);

# sample size n=1500 (slightly larger used)
sum(ns <- round((1500/N) * Ns));
sum(ns <- ifelse(ns<2,2,ns));
ns;

sstrata <- vector(mode="character");
ef <- vector(mode="numeric");
for (j in strata) {
  sstrata <- c(sstrata,rep(j,ns[j]));
  ef <- c(ef,dat$Nfraudmean[dat$cname==j][sample(Ns[j],ns[j])]);
}
sdat <- data.frame(sstrata,ef);

source("../lectures/stmean.R");
STM <- stmean(sdat, "ef", "sstrata", Ns);
STM$mean;  # STSRS mean
STM$bound; # STSRS mean bound
STM$mean*N;  # STSRS total
STM$bound*N; # STSRS total bound

# SRS mean
nSRS <- sum(ns);
nSRS;
ybar <- mean(SRSy <- dat$Nfraudmean[sample(N,nSRS)]);
ybar;  # SRS mean
varybar <- var(SRSy)/nSRS * (N-nSRS)/N;
SRSbound <- 2*sqrt(varybar);
SRSbound;

# STSRS mean for B=5
# sdev using .25*Votes to approximate frauds
sdevs <- sapply(strata,function(z){ sqrt(var(.25*dat$Votes[dat$cname==z])) });
sdevs;

source("../lectures/stallocm.R");
# stallocm <- function(popsizes, sdevs, bound, costs, type="mean") {
stallocm(Ns, sdevs, 5, 1)
nsB5 <- round(stallocm(Ns, sdevs, 5, 1)$ni);
sum(nsB5 <- ifelse(nsB5<2,2,nsB5));
nsB5;
sum(nsB5);
sstrata <- vector(mode="character");
ef <- vector(mode="numeric");
for (j in strata) {
  sstrata <- c(sstrata,rep(j,nsB5[j]));
  ef <- c(ef,dat$Nfraudmean[dat$cname==j][sample(Ns[j],nsB5[j])]);
}
sdatB5 <- data.frame(sstrata,ef);

#source("../lectures/stmean.R");
STMB5 <- stmean(sdatB5, "ef", "sstrata", Ns);
STMB5$mean;  # STSRS mean for B=5
STMB5$bound; # STSRS mean bound for B=5
sum(nsB5);

# SRS sample size for mean B=5
# n <- N*sigma^2 / ((N-1)(B^2/4)+sigma^2)
B <- 5;
sigma2 <- var(.25 * dat$Votes);  # guesstimate for sigma^2
sigma2;
nn <- N*sigma2 / ((N-1)*(B^2/4)+sigma2);
nn;
nBSRS <- ceiling(nn);
nBSRS;
# sample
sB <- sample(N,nBSRS);
yB <- dat$Nfraudmean[sB];

# mean
ybarB <- sum(yB)/nBSRS;
ybarB;
s2B <- sum((yB-ybarB)^2)/(nBSRS-1);
boundybarB <- 2*sqrt(s2B/nBSRS * (N-nBSRS)/N);
c(nBSRS,ybarB,boundybarB);  # SRS sample size, mean, bound for B=5

### approach 2:  combine small strata

Ns <- xtabs(~ cname, dat);
Ns;
cset <- names(Ns)[idx <- round((1500/N) * Ns)<35];
cset;
dat$cname2 <- ifelse(dat$cname %in% cset,"combined",as.character(dat$cname));
Ns <- xtabs(~ cname2, dat);
Ns;

strata <- names(Ns);

# sample size n=1500
sum(ns <- round((1500/N) * Ns));
ns;

sstrata <- vector(mode="character");
ef <- vector(mode="numeric");
for (j in strata) {
  sstrata <- c(sstrata,rep(j,ns[j]));
  ef <- c(ef,dat$Nfraudmean[dat$cname2==j][sample(Ns[j],ns[j])]);
}
sdat <- data.frame(sstrata,ef);

source("../lectures/stmean.R");
STM <- stmean(sdat, "ef", "sstrata", Ns);
STM$mean;  # STSRS mean
STM$bound; # STSRS mean bound
STM$mean*N;  # STSRS total
STM$bound*N; # STSRS total bound

# SRS mean
nSRS <- sum(ns);
nSRS;
ybar <- mean(SRSy <- dat$Nfraudmean[sample(N,nSRS)]);
ybar;  # SRS mean
varybar <- var(SRSy)/nSRS * (N-nSRS)/N;
SRSbound <- 2*sqrt(varybar);
SRSbound;

# STSRS mean for B=5
# sdev using .25*Votes to approximate frauds
sdevs <- sapply(strata,function(z){ sqrt(var(.25*dat$Votes[dat$cname2==z])) });
sdevs;

source("../lectures/stallocm.R");
# stallocm <- function(popsizes, sdevs, bound, costs, type="mean") {
stallocm(Ns, sdevs, 5, 1)
sum(nsB5 <- round(stallocm(Ns, sdevs, 5, 1)$ni));
nsB5;
sum(nsB5);
sstrata <- vector(mode="character");
ef <- vector(mode="numeric");
for (j in strata) {
  sstrata <- c(sstrata,rep(j,nsB5[j]));
  ef <- c(ef,dat$Nfraudmean[dat$cname2==j][sample(Ns[j],nsB5[j])]);
}
sdatB5 <- data.frame(sstrata,ef);

#source("../lectures/stmean.R");
STMB5 <- stmean(sdatB5, "ef", "sstrata", Ns);
STMB5$mean;  # STSRS mean for B=5
STMB5$bound; # STSRS mean bound for B=5
sum(nsB5);

# SRS sample size for mean B=5
# n <- N*sigma^2 / ((N-1)(B^2/4)+sigma^2)
B <- 5;
sigma2 <- var(.25 * dat$Votes);  # guesstimate for sigma^2
sigma2;
nn <- N*sigma2 / ((N-1)*(B^2/4)+sigma2);
nn;
nBSRS <- ceiling(nn);
nBSRS;
# sample
sB <- sample(N,nBSRS);
yB <- dat$Nfraudmean[sB];

# mean
ybarB <- sum(yB)/nBSRS;
ybarB;
s2B <- sum((yB-ybarB)^2)/(nBSRS-1);
boundybarB <- 2*sqrt(s2B/nBSRS * (N-nBSRS)/N);
c(nBSRS,ybarB,boundybarB);  # SRS sample size, mean, bound for B=5
