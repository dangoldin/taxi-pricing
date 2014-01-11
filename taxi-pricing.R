# http://www.worldtaximeter.com/
# Singapore, Edinburgh, Houston, Paris

library(ggplot2)
library(grid)
library(gridExtra)
library(reshape)
library(reshape2)
library(scales)
library(lattice)
library(ggthemes)
library(directlabels)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

d.price <- read.csv("taxi-prices.csv", check.names=FALSE)
d.price[2,8] <- as.numeric(d.price[2,8])

d.price$usd_base_fare <- d.price$base_fare * d.price$fare_coversion
d.price$usd_per_mile <- d.price$price_per_distance * d.price$distance_conversion * d.price$fare_coversion
d.price$usd_per_minute <- d.price$price_per_minute * d.price$fare_coversion
d.price$ratio <- d.price$usd_per_mile/d.price$usd_per_minute
d.price$hourly_usd <- 60 * d.price$usd_per_minute
d.price$usd_min_and_mile <- d.price$usd_per_minute + d.price$usd_per_mile

m <- as.matrix(cbind(d.price$usd_per_mile, d.price$usd_per_minute),ncol=2)
cl <- kmeans(m,4)
d.price$cluster <- factor(cl$cluster)
centers <- as.data.frame(cl$centers)

quantiles <- quantile(d.price$usd_min_and_mile)
d.price$quantile <- cut(d.price$usd_min_and_mile, breaks = quantiles, include.lowest=TRUE)

get_rate_function <- function(base_fare,included_distance,included_time,price_per_distance,price_per_minute,distance_conversion,fare_coversion, env = parent.frame()) {
  args <- alist(distance=, time=)
  body <- substitute({fare <- base_fare + max(0, distance * distance_conversion - included_distance) * price_per_distance + max(0, time - included_time) * price_per_minute; fare <- fare * fare_coversion; fare}, list(base_fare = base_fare, distance_conversion = distance_conversion, included_distance = included_distance, price_per_distance = price_per_distance, included_time = included_time, price_per_minute = price_per_minute, fare_coversion = fare_coversion))
  as.function(c(args, body), env)
}

distances <- seq(0,50,0.5)
times <- seq(0,240,2)

d <- expand.grid(distances,times)
d <- rename(d, c("Var1"="distance", "Var2"="time"))

# palette <- colorRampPalette(c("red","green"))(length(d.price$city))
# plot(x, y, col = palette[cut(x, maxColorValue)])

city_fares <- apply(d.price[,2:8], 1, function(x) {
  r <- get_rate_function( x['base_fare'], x['included_distance'], x['included_time'], x['price_per_distance'], x['price_per_minute'], x['distance_conversion'], x['fare_coversion'])
  apply(d[,c('distance','time')], 1, function(y) r(y['distance'], y['time']) )
})

city_fares <- data.frame(city_fares)
names(city_fares) <- d.price$city
d.m <- melt(cbind(d, city_fares), id.vars=c('distance', 'time'))

max_miles_10 <- sapply(d.price$city, function(c) {
  d <- d.m[d.m$value <= 10 & d.m$variable == c,]
  max(d$distance)
})

max_minutes_10 <- sapply(d.price$city, function(c) {
  d <- d.m[d.m$value <= 10 & d.m$variable == c,]
  max(d$time)
});

d.price <- d.price[,!(names(d.price) %in% c("max_miles_10","max_minutes_10"))]
d.price <- cbind(d.price, max_miles_10, max_minutes_10)

write.table(d.price, file = "data-prices-final.csv", sep = ",")

png('taxi-mile-vs-distance.png', width=800, height=800)
ggplot(d.price, aes(x=usd_per_mile,y=usd_per_minute,color=city)) +
  geom_text(aes(label=city, size = 2)) +
  theme_tufte() +
  xlab("USD per Mile") +
  ylab("USD per Minute") +
  ggtitle("USD per Min vs USD per Mile") +
  theme(legend.position="none")
dev.off()

png('taxi-fixed-distance-4.png', width=800, height=1200)
ggplot(d.m[d.m$distance==4,], aes(x=time, y=value, fill=variable)) +
  geom_line(aes(fill=variable, color=variable)) +
  theme_tufte() +
  xlab("Minutes") +
  ylab("USD") +
  ggtitle("Cost of 4 mile ride by time") +
  theme(legend.position="none") +
  geom_dl(aes(label=variable, color=variable), list(last.points, hjust = 1, cex=1))
dev.off()

png('taxi-fixed-time-10.png', width=800, height=1200)
ggplot(d.m[d.m$time==10,], aes(x=distance, y=value, fill=variable)) +
  geom_line(aes(fill=variable, color=variable)) +
  theme_tufte() +
  xlab("Miles") +
  ylab("USD") +
  ggtitle("Cost of 10 minute ride by distance") +
  theme(legend.position="none") +
  geom_dl(aes(label=variable, color=variable), list(last.points, hjust = 1, cex=1))
dev.off()

png('taxi-max-miles-max-minutes.png', width=800, height=800)
ggplot(d.price, aes(x=max_miles_10,y=max_minutes_10,color=city)) +
  geom_text(aes(label=city,size = 4)) +
  theme_tufte() +
  xlab("Max miles") +
  ylab("Max minutes") +
  ggtitle("What can you get for $10?") +
  theme(legend.position="none")
dev.off()

png('taxi-max-miles-max-minutes-zoom.png', width=800, height=800)
ggplot(d.price, aes(x=max_miles_10,y=max_minutes_10,color=city)) +
  geom_text(aes(label=city,size = 4)) +
  theme_tufte() +
  xlab("Max miles") +
  ylab("Max minutes") +
  ggtitle("What can you get for $10?") +
  theme(legend.position="none") +
  xlim(c(0,7.5)) +
  ylim(c(0,25))
dev.off()

png('taxi-mile-min-ratios.png', width=800, height=800)
ggplot(d.price, aes(x=city,y=ratio,group=ratio,fill=ratio)) +
  geom_bar(stat="identity") +
  theme_tufte() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size=16, angle = 90, hjust = 1, vjust = 0)) +
  xlab("City") +
  ylab("Ratio") +
  ggtitle("Ratio of $ per Mile vs $ per Min")
dev.off()

max_price <- max(d.m$value)
num_cols <- round(sqrt(length(d.price$city)))
plots <- lapply( d.price$city,
  function(c) {
    dd <- d.m[d.m$variable == c,]
    ggplot(dd, aes(distance, time)) +
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low = "red", high = "green", limits=c(0,max_price)) +
      ggtitle(c) +
      theme_tufte() +
      xlab(NULL) +
      ylab(NULL) +
      theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.margin=unit(c(0,0,0,0), "cm"),
        plot.margin=unit(c(0,0,0,0), "cm"),
        plot.background=element_blank(),
        legend.margin=unit(c(0,0,0,0), "cm"),
        plot.title = element_text(size = 10))
})
png('taxi-heatmap-fares.png', width=800, height=800)
multiplot(plotlist = plots, cols = num_cols)
dev.off()

lapply(unique(d.price$quantile),
  function(q) {
    cities <- d.price$city[ d.price$quantile == q ]
    d.mt <- d.m[ which(d.m$variable %in% cities), ]
    max_price <- max( d.mt$value )
    num_cols <- round(sqrt(length(cities)))
    plots <- lapply( cities,
      function(c) {
        dd <- d.mt[ d.mt$variable == c, ]
        ggplot(dd, aes(distance, time)) +
          geom_tile(aes(fill = value)) +
          scale_fill_gradient(low = "red", high = "green", limits=c(0,max_price)) +
          ggtitle(c) +
          theme_tufte() +
          xlab(NULL) +
          ylab(NULL) +
          theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.margin=unit(c(0,0,0,0), "cm"),
            plot.margin=unit(c(0,0,0,0), "cm"),
            plot.background=element_blank(),
            legend.margin=unit(c(0,0,0,0), "cm"),
            plot.title = element_text(size = 10))
    })
    png(sprintf('taxi-heatmap-fares-%s.png',q), width=800, height=800)
    multiplot(plotlist = plots, cols = num_cols)
    dev.off()
  }
)

quit();

ggplot(d.m, aes(distance, time)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_grid(. ~ variable)

ggplot(d.m[d.m$variable == 'NYC' | d.m$variable == 'London' | d.m$variable == 'Tokyo' | d.m$variable == 'Mumbai',], aes(distance, time)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_grid(variable ~ .)

num_cols <- round(sqrt(length(d.price$city)))
plots <- lapply(d.price$city,
  function(c) {
    dd <- d.m[d.m$variable == c,]
    ggplot(dd, aes(distance, time)) +
      geom_tile(aes(fill = value), colour = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      ggtitle(c)
  })
multiplot(plotlist = plots, cols = num_cols)

png('all.png', width=4000, height=4000)
multiplot(plotlist = plots, cols = num_cols)
dev.off()

print(d.m[d.m$distance==2 & d.m$time==5,])

names(d2)[3:7] <- d.price$city

z <- by(d.price[,2:8], d.price$city, function(x) {
    d$price <- apply(d[,c('distance','time')], 1, function(y) get_rate_function(x['base_fare'], x['included_distance'], x['included_time'], x['price_per_distance'], x['price_per_minute'], x['distance_conversion'], x['fare_coversion'])(y['distance'],y['time']) )
  }
)

cbind(d, x)

g <- get_rate_function(2.5,0,0,2.5,1,1,1)

sapply(seq_len(nrow(d.price)), function(i) {
  apply(d[,c('distance','time')], 1, function(y) {
    x <- d.price[i,]
    distance <- y['distance']
    time <- y['time']
    # print(x)
    fare <- x['base_fare'] + max(0, distance * x['distance_conversion'] - x['included_distance']) * x['price_per_distance'] + max(0, time - x['included_time']) * x['price_per_minute']
    fare <- fare * x['fare_coversion']
    fare
  })
})

apply(d.price, 1, function(x) {
  apply(d[,c('distance','time')], 1, function(y) {
    distance <- as.numeric(y['distance'])
    time <- y['time']
    fare <- x['base_fare'] + max(0, distance * x['distance_conversion'] - x['included_distance']) * x['price_per_distance'] + max(0, time - x['included_time']) * x['price_per_minute']
    fare <- fare * x['fare_coversion']
    fare
    }
  )
})

quit()

price_mumbai <- function(distance, time) {
	fare <- 0.32 + max(0, distance * 1.61 - 1.5) * 0.21 + 0.5 * time / 60
	fare
}

price_nyc <- function(distance, time) {
	fare <- 2.5 + 0.5 * distance/0.2 + 0.5 * time
	fare
}

price_london <- function(distance, time) {
  fare <- 2.20 + max(0, distance * 1.61 - 0.252) * 1.7 + 6.7/13 * time + 0.2
  fare <- fare * 1.64
  fare
}

price_amsterdam <- function(distance, time) {
  fare <- 7.5 + max(0, distance * 1.61 - 2) * 2.2
  fare <- fare * 1.36
  fare
}

price_tokyo <- function(distance, time) {
  fare <- 712 + distance * 1.61 * 188 + time * 56
  fare <- fare * 0.0096
  fare
}

price_singapore <- function(distance, time) {
  fare <- 3 * max(0, distance * 1.61 - 1) * 0.6 + 3.03/10 * time
  fare <- fare * 0.79
  fare
}

d$price_mumbai <- apply(d[,c('distance','time')], 1, function(x) price_mumbai(x['distance'],x['time']) )
d$price_nyc <- apply(d[,c('distance','time')], 1, function(x) price_nyc(x['distance'],x['time']) )
d$price_london <- apply(d[,c('distance','time')], 1, function(x) price_london(x['distance'],x['time']) )
d$price_amsterdam <- apply(d[,c('distance','time')], 1, function(x) price_amsterdam(x['distance'],x['time']) )
d$price_tokyo <- apply(d[,c('distance','time')], 1, function(x) price_tokyo(x['distance'],x['time']) )
d$price_singapore <- apply(d[,c('distance','time')], 1, function(x) price_singapore(x['distance'],x['time']) )

d.m <- melt(d, id.vars=c('distance', 'time')) #, measure.vars=c('price_nyc', 'price_london', 'price_amsterdam', 'price_mumbai'))

ggplot(d.m, aes(distance, time)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_grid(variable ~ .)

#d$diff <- d$price_nyc / d$price_mumbai

ggplot(d, aes(distance, time)) + geom_tile(aes(fill = price_mumbai), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")
ggplot(d, aes(distance, time)) + geom_tile(aes(fill = price_nyc), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")

ggplot(d, aes(distance, time)) + geom_tile(aes(fill = diff), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")

# matrix(outer(distances, times, price_mumbai), nrow=length(distances))

x <- acast(d, distance~time, value.var="price_mumbai")
contour(distances, times, x)
persp(distances, times, x)

wireframe(price_mumbai ~ distance * time, data=d)
wireframe(price_nyc ~ distance * time, data=d)

open3d()
plot3d(d$distance, d$time, d$price_nyc, col=rainbow(1000))

open3d()
plot3d(d$distance, d$time, d$price_mumbai, col=rainbow(1000))

open3d()
plot3d(d$distance, d$time, d$price_mumbai)
plot3d(d$distance, d$time, d$price_nyc)
