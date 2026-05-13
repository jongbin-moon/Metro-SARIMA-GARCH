################### load data ########################

library(readr)
library(dplyr)
library(lubridate)

df <- read_csv("C:/Users/pc/Desktop/JB/졸업논문/데이터/[260216] 2023~2025 역별 승차인원_연휴일수(수치형)&요일처리_수정.csv",
               locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE
)
head(df)
tail(df)
length(df$datetime)

attr(df$datetime, "tzone") # 시간대 확인 : UTC
df$datetime <- force_tz(df$datetime, "Asia/Seoul")   # KST로 변경

# 시간대별로 정리
df_by_hour <- df %>%
  mutate(hour = hour(datetime)) %>%
  group_split(hour)

df_by_hour[[0 +1]]
df_by_hour[[23 +1]]


#################### time series #########################

time = 8 # 출근길 시간대 지정

name_station_line = unique(df$station_line)   # 역명이름 추출
df_by_station_train = list()
df_by_station_test = list()

length(name_station_line)


# train-test split
for (station in name_station_line){
  df_by_station_train[[station]] <- df_by_hour[[time + 1]] %>%
    filter(
      station_line == station,
      datetime >= as.POSIXct("2023-01-01 00:00:00", tz="Asia/Seoul"),
      datetime <  as.POSIXct("2025-06-24 00:00:00", tz="Asia/Seoul")
    ) %>%
    arrange(datetime)
  
  df_by_station_test[[station]] <- df_by_hour[[time + 1]] %>%
    filter(
      station_line == station,
      datetime >= as.POSIXct("2025-06-24 00:00:00", tz="Asia/Seoul")
    ) %>%
    arrange(datetime)
}

# 행의 개수가 905개& 7개가 아닌 데이터프레임은 제거
df_by_station_train <- Filter(function(x) nrow(x) == 905, df_by_station_train)
df_by_station_test  <- Filter(function(x) nrow(x) == 7, df_by_station_test)

df_by_station_test <- df_by_station_test[names(df_by_station_train)]



##################### stationary check ###########################
library(ggplot2)
library(quantmod)
library(PerformanceAnalytics)
library(car)
library(tseries)

name_station_line = names(df_by_station_train)   # 역명이름 추출


# 차분 전 acf & pacf 확인
par(mfrow = c(1,2))
acf(na.omit(df_by_station_train$한양대_2호선$value), lag = 50,
    main = "ACF - 한양대 2호선")
pacf(na.omit(df_by_station_train$한양대_2호선$value), lag = 50, lwd=2,
     main = "PACF - 한양대 2호선")
par(mfrow = c(1,1))
Box.test(df_by_station_train$한양대_2호선$value,
         type = "Ljung-Box", lag = 50) # Box-test


# 모든 역에 대한 정상성 검정
adf.test(na.omit(df_by_station_train$한양대_2호선$value), k=10)    # ADF Test

for (station in name_station_line){
  b = adf.test(na.omit(df_by_station_train[[station]]$value),k=10)
  pval = b$p.value
  
  if (pval >= 0.05){
    print(station)
  }
}



# 전처리(차분)
for (station in name_station_line){
  df_by_station_train[[station]]$rtn7 <- c(rep(NA,7), diff(log(df_by_station_train[[station]]$value + 1), lag = 7))
}


# 차분 이후 acf & pacf
par(mfrow = c(1,2))
acf(na.omit(df_by_station_train$한양대_2호선$rtn7), lag = 50,
    main = "ACF - 한양대 2호선(차분)")
pacf(na.omit(df_by_station_train$한양대_2호선$rtn7), lag = 50, lwd=2,
     main = "PACF - 한양대 2호선(차분)")
par(mfrow = c(1,1))
Box.test(df_by_station_train$한양대_2호선$rtn7,
         type = "Ljung-Box", lag = 50) # Box-test

adf.test(na.omit(df_by_station_train$한양대_2호선$rtn7))

pval = 0
for (station in name_station_line){
  b = Box.test(df_by_station_train[[station]]$rtn7,
               type = "Ljung-Box", lag = 100) # Box-test
  pval = pval + (b$p.value >= 0.05)
  
  if (b$p.value >= 0.05){
    print(station)
  }
}
pval


########################## SARIMA Modeling #########################################

# SARMA modeling
library(lmtest)
library(fGarch)

sarma_model = list()
garch_model = list()

# 한양대역
station = '한양대_2호선'


# 공휴일 외생변수 넣어서 SARMA 모델링
m01 = arima(df_by_station_train[[station]]$rtn7,
            order = c(1,0,1),
            seasonal = list(order = c(3, 0, 1), period = 7),
            # xreg = xreg_mat
            
            # transform.pars = TRUE,
            # method = "CSS-ML",
            # include.mean = TRUE
)

m02 = arima(df_by_station_train[[station]]$rtn7,
            order = c(1,0,1),
            seasonal = list(order = c(0, 0, 1), period = 7),
            # xreg = xreg_mat
            
            # transform.pars = TRUE,
            # method = "CSS-ML",
            # include.mean = TRUE
)

m03 = arima(df_by_station_train[[station]]$rtn7,
            order = c(1,0,0),
            seasonal = list(order = c(0, 0, 1), period = 7),
            # xreg = xreg_mat
            
            # transform.pars = TRUE,
            # method = "CSS-ML",
            # include.mean = TRUE
)

coeftest(m01)
coeftest(m02)
coeftest(m03)


m01$aic
m02$aic
m03$aic



resi = na.omit(m02$residuals)
par(mfrow = c(1,2))
acf(resi, lag = 50,main = "ACF - 한양대")
pacf(resi, lag = 50, lwd=2,
     main = "PACF")
par(mfrow = c(1,1))

Box.test(resi,type = "Ljung-Box", lag = 50) # Box-test
adf.test(resi)


# 모든 역
m01 = list()
m02 = list()
m03 = list()
m01_1 = list()
m02_1 = list()
m03_1 = list()


m01_aic = list()
m02_aic = list()
m03_aic = list()
m01_1_aic = list()
m02_1_aic = list()
m03_1_aic = list()

m01[[station]] = arima(df_by_station_train[[station]]$rtn7,
                       order = c(1,0,1),
                       seasonal = list(order = c(3, 0, 1), period = 7),
                       include.mean = TRUE
                       # xreg = xreg_mat
                       
                       # transform.pars = TRUE,
                       # method = "CSS-ML",
                       # include.mean = TRUE
)
m01_aic[[station]] = m01[[station]]$aic
m01_1[[station]] = arima(df_by_station_train[[station]]$rtn7,
                         order = c(1,0,1),
                         seasonal = list(order = c(3, 0, 1), period = 7),
                         include.mean = FALSE
                         # xreg = xreg_mat
                         
                         # transform.pars = TRUE,
                         # method = "CSS-ML",
                         # include.mean = TRUE
)
m01_1_aic[[station]] = m01[[station]]$aic


m02[[station]] = arima(df_by_station_train[[station]]$rtn7,
                       order = c(1,0,1),
                       seasonal = list(order = c(0, 0, 1), period = 7),
                       # xreg = xreg_mat
                       
                       # transform.pars = TRUE,
                       # method = "CSS-ML",
                       # include.mean = TRUE
)
m02_aic[[station]] = m02[[station]]$aic
m02_1[[station]] = arima(df_by_station_train[[station]]$rtn7,
                         order = c(1,0,1),
                         seasonal = list(order = c(0, 0, 1), period = 7),
                         include.mean = FALSE
                         # xreg = xreg_mat
                         
                         # transform.pars = TRUE,
                         # method = "CSS-ML",
                         # include.mean = TRUE
)
m02_1_aic[[station]] = m02[[station]]$aic

m03[[station]] = arima(df_by_station_train[[station]]$rtn7,
                       order = c(1,0,0),
                       seasonal = list(order = c(0, 0, 1), period = 7),
                       # xreg = xreg_mat
                       
                       # transform.pars = TRUE,
                       # method = "CSS-ML",
                       # include.mean = TRUE
)
m03_aic[[station]] = m03[[station]]$aic
m03_1[[station]] = arima(df_by_station_train[[station]]$rtn7,
                         order = c(1,0,0),
                         seasonal = list(order = c(0, 0, 1), period = 7),
                         include.mean = FALSE
                         # xreg = xreg_mat
                         
                         # transform.pars = TRUE,
                         # method = "CSS-ML",
                         # include.mean = TRUE
)
m03_1_aic[[station]] = m03[[station]]$aic



m01[[station]]$aic
m01_1[[station]]$aic
m02[[station]]$aic
m02_1[[station]]$aic
m03[[station]]$aic
m03_1[[station]]$aic

AIC(m01[[station]])
BIC(m01[[station]])


coeftest(m01[[station]])
coeftest(m01_1[[station]])
coeftest(m02[[station]])
coeftest(m02_1[[station]])
coeftest(m03[[station]])
coeftest(m03_1[[station]])


####################3 SARIMA Prediction ##############################
# 한양대역

m01_fc <- predict(m01$한양대_2호선, n.ahead = 7)$pred
m02_fc <- predict(m02$한양대_2호선, n.ahead = 7)$pred
m03_fc <- predict(m03$한양대_2호선, n.ahead = 7)$pred
m01_1_fc <- predict(m01_1$한양대_2호선, n.ahead = 7)$pred
m02_1_fc <- predict(m02_1$한양대_2호선, n.ahead = 7)$pred
m03_1_fc <- predict(m03_1$한양대_2호선, n.ahead = 7)$pred

train_value = tail(df_by_station_train$한양대_2호선$value, 10)   # train value값
test_value = df_by_station_test$한양대_2호선$value         # test value값

m01_predicted_value = numeric()    # 복원할 value값
m02_predicted_value = numeric()
m03_predicted_value = numeric()
m01_1_predicted_value = numeric()
m02_1_predicted_value = numeric()
m03_1_predicted_value = numeric()


# 원시 데이터로 복원
for (i in 1:7){
  m01_predicted_value[i] = exp(m01_fc[i] + log(train_value[length(train_value) - 7 +i] + 1)) - 1
  m02_predicted_value[i] = exp(m02_fc[i] + log(train_value[length(train_value) - 7 +i] + 1)) - 1
  m03_predicted_value[i] = exp(m03_fc[i] + log(train_value[length(train_value) - 7 +i] + 1)) - 1
  m01_1_predicted_value[i] = exp(m01_1_fc[i] + log(train_value[length(train_value) - 7 +i] + 1)) - 1
  m02_1_predicted_value[i] = exp(m02_1_fc[i] + log(train_value[length(train_value) - 7 +i] + 1)) - 1
  m03_1_predicted_value[i] = exp(m03_1_fc[i] + log(train_value[length(train_value) - 7 +i] + 1)) - 1
}

m03_predicted_value
test_value


# RMSE 확인
m01_rmse <- sqrt(mean((test_value - m01_predicted_value)^2))
m02_rmse <- sqrt(mean((test_value - m02_predicted_value)^2))
m03_rmse <- sqrt(mean((test_value - m03_predicted_value)^2))
m01_1_rmse <- sqrt(mean((test_value - m01_1_predicted_value)^2))
m02_1_rmse <- sqrt(mean((test_value - m02_1_predicted_value)^2))
m03_1_rmse <- sqrt(mean((test_value - m03_1_predicted_value)^2))

m01_rmse
m02_rmse
m03_rmse
m01_1_rmse
m02_1_rmse
m03_1_rmse



###############################################
# 모든 역에 대한 SARIMA Modeling
for (station in name_station_line){
  m01[[station]] = arima(df_by_station_train[[station]]$rtn7,
                         order = c(1,0,1),
                         seasonal = list(order = c(3, 0, 1), period = 7),
                         include.mean = TRUE
                         # xreg = xreg_mat
                         
                         # transform.pars = TRUE,
                         # method = "CSS-ML",
                         # include.mean = TRUE
  )
  m01_aic[[station]] = m01[[station]]$aic
  m01_1[[station]] = arima(df_by_station_train[[station]]$rtn7,
                           order = c(1,0,1),
                           seasonal = list(order = c(3, 0, 1), period = 7),
                           include.mean = FALSE
                           # xreg = xreg_mat
                           
                           # transform.pars = TRUE,
                           # method = "CSS-ML",
                           # include.mean = TRUE
  )
  m01_1_aic[[station]] = m01[[station]]$aic
  
  
  m02[[station]] = arima(df_by_station_train[[station]]$rtn7,
                         order = c(1,0,1),
                         seasonal = list(order = c(0, 0, 1), period = 7),
                         # xreg = xreg_mat
                         
                         # transform.pars = TRUE,
                         # method = "CSS-ML",
                         # include.mean = TRUE
  )
  m02_aic[[station]] = m02[[station]]$aic
  m02_1[[station]] = arima(df_by_station_train[[station]]$rtn7,
                           order = c(1,0,1),
                           seasonal = list(order = c(0, 0, 1), period = 7),
                           include.mean = FALSE
                           # xreg = xreg_mat
                           
                           # transform.pars = TRUE,
                           # method = "CSS-ML",
                           # include.mean = TRUE
  )
  m02_1_aic[[station]] = m02[[station]]$aic
  
  m03[[station]] = arima(df_by_station_train[[station]]$rtn7,
                         order = c(1,0,0),
                         seasonal = list(order = c(0, 0, 1), period = 7),
                         # xreg = xreg_mat
                         
                         # transform.pars = TRUE,
                         # method = "CSS-ML",
                         # include.mean = TRUE
  )
  m03_aic[[station]] = m03[[station]]$aic
  m03_1[[station]] = arima(df_by_station_train[[station]]$rtn7,
                           order = c(1,0,0),
                           seasonal = list(order = c(0, 0, 1), period = 7),
                           include.mean = FALSE
                           # xreg = xreg_mat
                           
                           # transform.pars = TRUE,
                           # method = "CSS-ML",
                           # include.mean = TRUE
  )
  m03_1_aic[[station]] = m03[[station]]$aic
}


# Modeling Fitting Comparison
m01_aiccnt = 0
m02_aiccnt = 0
m03_aiccnt = 0
m01_1_aiccnt = 0
m02_1_aiccnt = 0
m03_1_aiccnt = 0


for (station in name_station_line) {
  
  aic_values <- c(AIC(m01[[station]]),
                  AIC(m02[[station]]),
                  AIC(m03[[station]]),
                  AIC(m01_1[[station]]),
                  AIC(m02_1[[station]]),
                  AIC(m03_1[[station]]))
  
  min_model <- which.min(aic_values)
  
  if (min_model == 1) {
    m01_aiccnt <- m01_aiccnt + 1
  } else if (min_model == 2) {
    m02_aiccnt <- m02_aiccnt + 1
  } else if (min_model == 3) {
    m03_aiccnt <- m03_aiccnt + 1
  } else if (min_model == 4) {
    m01_1_aiccnt <- m01_1_aiccnt + 1
  } else if (min_model == 5) {
    m02_1_aiccnt <- m02_1_aiccnt + 1
  } else if (min_model == 6) {
    m03_1_aiccnt <- m03_1_aiccnt + 1
  }
}

m01_aiccnt
m02_aiccnt
m03_aiccnt
m01_1_aiccnt
m02_1_aiccnt
m03_1_aiccnt


m01_biccnt = 0
m02_biccnt = 0
m03_biccnt = 0
m01_1_biccnt = 0
m02_1_biccnt = 0
m03_1_biccnt = 0


for (station in name_station_line) {
  
  bic_values <- c(BIC(m01[[station]]),
                  BIC(m02[[station]]),
                  BIC(m03[[station]]),
                  BIC(m01_1[[station]]),
                  BIC(m02_1[[station]]),
                  BIC(m03_1[[station]]))
  
  min_model <- which.min(bic_values)
  
  if (min_model == 1) {
    m01_biccnt <- m01_biccnt + 1
  } else if (min_model == 2) {
    m02_biccnt <- m02_biccnt + 1
  } else if (min_model == 3) {
    m03_biccnt <- m03_biccnt + 1
  } else if (min_model == 4) {
    m01_1_biccnt <- m01_1_biccnt + 1
  } else if (min_model == 5) {
    m02_1_biccnt <- m02_1_biccnt + 1
  } else if (min_model == 6) {
    m03_1_biccnt <- m03_1_biccnt + 1
  }
}

m01_biccnt
m02_biccnt
m03_biccnt
m01_1_biccnt
m02_1_biccnt
m03_1_biccnt

# Residual
resi = list()
for (station in name_station_line){
  resi[[station]] = m03_1[[station]]$residuals
}


# 잔차 자기상관 확인
pval = 0
for (station in name_station_line){
  b = Box.test(na.omit(resi[[station]])^2,
               type = "Ljung-Box", lag = 10) # Box-test
  pval = pval + (b$p.value >= 0.05)
  
  if (b$p.value >= 0.05){
    print(station)
  }
}
pval



##################### GARCH Modeling ###############################

garch_model = list()
garch_resi = list()
arch_model = list()
arch_resi = list()

for (station in name_station_line){
  # GARCH 모델적용
  garch_model[[station]] = garchFit(~garch(1,1), data = na.omit(resi[[station]]), trace=F)
  garch_resi[[station]] = garch_model[[station]]@residuals
}


garch_model$한양대_2호선
garch_model$한양대_2호선@fit$ics


# 잔차 자기상관 확인
pval = 0
for (station in name_station_line){
  b = Box.test(na.omit(resi[[station]]),
               type = "Ljung-Box", lag = 100) # Box-test
  pval = pval + (b$p.value < 0.05)
  
  if (b$p.value < 0.05){
    print(station)
  }
}
pval


########################################################
# 명동역에 대한 개별모델링
mm = arima(df_by_station_train$명동_4호선$rtn7,
           order = c(1,0,5),
           seasonal = list(order = c(3, 0, 1), period = 7),
           include.mean = TRUE
           # xreg = xreg_mat
           
           # transform.pars = TRUE,
           # method = "CSS-ML",
           # include.mean = TRUE
)


# 전역 모델과 비교
par(mfrow = c(1,2))
acf(na.omit(mm$residuals), lag = 50,
    main = "ACF - 명동_4호선 residuals")
pacf(na.omit(mm$residuals), lag = 50, lwd=2,
     main = "PACF - 명동_4호선 residuals")
par(mfrow = c(1,1))
Box.test(na.omit(mm$residuals), type = "Ljung-Box", lag = 50) # Box-test

par(mfrow = c(1,2))
acf(garch_resi$명동_4호선, lag = 50,
    main = "ACF - 명동_4호선 residuals")
pacf(garch_resi$명동_4호선, lag = 50, lwd=2,
     main = "PACF - 명동_4호선 residuals")
par(mfrow = c(1,1))
Box.test(garch_resi$명동_4호선, type = "Ljung-Box", lag = 50) # Box-test





########################## Final Prediction #######################################
# SARMA Prediction
fc = list()
predicted = list()

for (station in name_station_line){
  ARMAfc <- predict(sarma_model[[station]], n.ahead = 7, newxreg = future_reg_mat)$pred
  GARCHfc = predict(garch_model[[station]],7, newxreg = future_reg_mat)$meanForecast
  fc[[station]] = ARMAfc + GARCHfc
}


train_value = list()
test_value = list()

### 원시 데이터로 복원
for (station in name_station_line){
  train_value[[station]] = tail(df_by_station_train[[station]]$value, 10)   # train value값
  test_value[[station]] = df_by_station_test[[station]]$value         # test value값
  
  predicted_value = numeric()    # 복원할 value값
  for (i in 1:7){
    predicted_value[i] = exp(fc[[station]][i] + log(train_value[[station]][length(train_value[[station]]) - 7 +i] + 1)) - 1
  }
  
  predicted[[station]] = predicted_value
}



test_value[[station]]
predicted[[station]]

test_value$구산_6호선
predicted$구산_6호선

# RMSE
rmse <- sqrt(mean((test_value[[station]] - predicted[[station]])^2))
rmse


# csv로 내보내기
predicted_df <- as.data.frame(predicted)

write.csv(predicted_df,
          "C:/Users/pc/Desktop/JB/졸업논문/코드/R예측 - ARMA, GARCH/predicted_df(승차수).csv",
          row.names = FALSE,
          fileEncoding = "utf-8")




############################## Prediction Plot #######################################
plot(1:length(test_value), test_value, type="l", lwd=2,
     ylim=range(c(test_value, as.numeric(predicted_value))),
     xlab="Index", ylab="Value")

lines(1:length(predicted_value), as.numeric(predicted_value),
      col="red", lwd=2)


for (station in name_station_line){
  # 원래꺼랑 이어서 plot
  last_train_time <- max(df_by_station_train[[station]]$datetime)
  
  test_dates <- seq(
    from = last_train_time + 86400,  # 하루 뒤 = 6월 24일 08:00
    by = "1 day",
    length.out = length(test_value[[station]])
  )
  testpred_df <- data.frame(
    datetime = test_dates,
    actual = test_value[[station]],
    predicted = predicted[[station]]
  )
  
  
  plot <- ggplot() +
    
    # train 마지막 80개
    geom_line(
      data = tail(df_by_station_train[[station]], 80),
      aes(x = datetime, y = value),
      color = "steelblue",
      linewidth = 0.6
    ) +
    
    # test 실제값 (6월 24일부터)
    geom_line(
      data = testpred_df,
      aes(x = datetime, y = actual, color = "Actual"),
      linewidth = 0.8
    ) +
    
    # 예측값
    geom_line(
      data = testpred_df,
      aes(x = datetime, y = predicted, color = "Predicted"),
      linewidth = 0.8
    ) +
    
    scale_color_manual(
      name = NULL,
      values = c("Actual" = "black",
                 "Predicted" = "red")
    ) +
    
    scale_x_datetime(date_breaks = "1 week",
                     date_labels = "%m-%d") +
    
    labs(
      title = paste0(station, " ", time, "시 승차 인원"),
      x = "Date",
      y = "Passengers"
    ) +
    
    theme(
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "black"),
      plot.title = element_text(
        hjust = 0.5,   # 가운데 정렬
        face = "bold",
        size = 14
      ),
      text = element_text(family = "Malgun Gothic")
    )
  
  # 저장
  ggsave(
    filename = file.path("C:/Users/pc/Desktop/JB/졸업논문/연구기록/1. SARMA + GARCH 예측/8시_승차인원 예측 plot",
                         paste0(station, " ", time, "시 승차 인원.png")),
    plot = plot,
    width = 7, height = 4, dpi = 300)
}