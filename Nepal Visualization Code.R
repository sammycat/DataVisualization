library(ggplot2)

donations_count$per <- round(100*donations_count$count/(sum(donations_count$count)),1)

barplot(donations_count$count, main="Donation Tag Type", horiz=TRUE, space=.4,
        names.arg=donations_count$tag, xlim=c(0, 60))
