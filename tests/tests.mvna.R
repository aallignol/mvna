### First test with fake data

time <- 1:10
from <- rep(0, 10)
to <- c(1, 1, "cens", 1, "cens", rep(1, 5))
id <- 1:10
d1 <- data.frame(id, from, to, time)

tra1 <- matrix(FALSE, 2, 2)
tra1[1, 2] <- TRUE

test1 <- mvna(d1, c("0", "1"), tra1, "cens")

tna <- cumsum(as.numeric(to != "cens") / 10:1)
tvarna <- cumsum(as.numeric(to != "cens") / (10:1)^2)

all.equal(c(0, tna), test1$"0 1"$na)
all.equal(c(0, tvarna), test1$"0 1"$var1)

## same test but with left-truncation

from <- rep(0, 10)
to <- c(1, 1, "cens", 1, "cens", rep(1, 5))
entry <- c(rep(1, 3), rep(2, 3), rep(4, 4))
exit <- c(2, 7, 7, 4, 6, 9, 7, 9, 6, 7)
id <- 1:10
d2 <- data.frame(id, from, to, entry, exit)

test2 <- mvna(d2, c("0", "1"), tra1, "cens")

tna <- cumsum(c(1, 1, 1, 3, 2) /
              c(3, 5, 8, 6, 2))

all.equal(test2$"0 1"$na, c(0, tna))

## with a fake illness-death model with recovery

id <- c(1:20, 11:20)
from <- c(rep(0, 20), rep(1, 10))
to <- c(rep(2, 5), rep("cens", 5), rep(1, 10), rep(2, 5), rep("cens", 5))
entry <- c(rep(0, 20), rep(10, 10))
exit <- c(rep(9, 5), rep(8, 5), rep(10, 10), rep(15, 5), rep(13, 5))
d3 <- data.frame(id, from, to, entry, exit)

tra2 <- matrix(FALSE, 3, 3)
tra2[1, 2:3] <- TRUE
tra2[2, 3] <- TRUE

test3 <- mvna(d3, c("0", "1", "2"), tra2, "cens")

tna01 <- c(0, cumsum(c(0, 0, 10) / c(20, 15, 10)))
tna02 <- c(0, cumsum(c(0, 5, 0) / c(20, 15, 10)))
tna12 <- cumsum(c(0, 5) / c(10, 5))

all.equal(tna01, test3$"0 1"$na)
all.equal(tna02, test3$"0 2"$na)
all.equal(tna12, test3$"1 2"$na)

### tests with the included data sets


