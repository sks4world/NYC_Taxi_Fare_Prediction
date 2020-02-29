# Map 1-based optional input ports to variables
dataset1 <- maml.mapInputPort(1) # class: data.frame

library(ggplot2)


# Sample operation
data.set = rbind(dataset1);

with(data.set, plot(fare_amount ~ trip_distance, col = "blue", pch=20, main="Variation of fare by distance"));

data.set = data.set[which(data.set$fare_amount<160),]
hist(data.set$fare_amount, main="Distribution of fares", col="orange")
axis(side=1, at=seq(0,160, 20), labels=seq(0,160,20))

data.set$passenger_count = as.factor(data.set$passenger_count)
p=ggplot(data = data.set, aes(x=passenger_count, y=fare_amount, fill=passenger_count)) + 
  geom_boxplot() + ggtitle("Distribution of fares by passenger count") + scale_y_continuous(name="Fare", breaks=seq(0,400,10))
p

data.set$vendor_id = as.factor(data.set$vendor_id)
data.set$rate_code = as.factor(data.set$rate_code)
data.set$payment_type = as.factor(data.set$payment_type)

p1=ggplot(data = data.set, aes(x=rate_code, y=fare_amount, fill=rate_code)) + 
  geom_boxplot() + ggtitle("Distribution of fares by rate code") + scale_y_continuous(name="Fare", breaks=seq(0,400,10))
p1

p2=ggplot(data = data.set, aes(x=payment_type, y=fare_amount, fill=payment_type)) + 
  geom_boxplot() + ggtitle("Distribution of fares by rate code") + scale_y_continuous(name="Fare", breaks=seq(0,400,10))
p2

data.set = subset(data.set, select=c('vendor_id', 'rate_code', 'passenger_count', 'trip_distance', 'payment_type', 'fare_amount'))

model_a = lm(fare_amount ~ vendor_id + rate_code + passenger_count + trip_distance + payment_type, data = data.set)
summary(model_a)

# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("data.set");