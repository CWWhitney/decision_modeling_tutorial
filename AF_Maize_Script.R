# Decision analysis of agroforestry and maize monoculture 
n <- 100
set.seed(123)

#NLKH: xen canh xoai (mango) và ngo (maize)
AF_mango_yield <- runif(n, 6000, 10000) # giá trị nhỏ nhất và lớn nhất (kg/ha)

AF_maize_yield <- runif( n, 3000, 5000) # (kg/ha)

AF_cost <- runif(n, 15, 30) # (trieu VND/ha)

#Ngo doc canh

mono_maize_yield <- runif (n, 4000, 8000) # (ton/ha)

maize_cost <- runif(n, 7, 15) # 10 trieu VND/ha

maize_price <- runif(n, 0.003, 0.007) # trieu VND/kg

mango_price <- runif(n, 0.002, 0.007) # trieu VND/kg

# 2) Rủi ro hạn hán

p_drought <- runif(n, 0.10, 0.50)     # mô phỏng xác suất hạn hán 
drought <- rbinom(n, size = 1, prob = p_drought)  # 1 = năm hạn hán

loss_maize <- ifelse(drought == 1, runif(n, 0.30, 0.70), 0)
loss_mango <- ifelse(drought == 1, runif(n, 0.10, 0.50), 0)

AF_mango_yield_drought <- AF_mango_yield * (1 - loss_mango)
AF_maize_yield_drought <- AF_maize_yield * (1 - loss_maize)
mono_maize_yield_drought <- mono_maize_yield * (1 - loss_maize)


# --- Lợi nhuận ---
AF_profit_drought <- (AF_mango_yield_drought * mango_price + AF_maize_yield_drought * maize_price) - AF_cost
mean(AF_profit_drought > 0)
hist(
  AF_profit_drought,
  breaks = 40,
  col = "lightblue",
  border = "white",
  main = "Nông lâm kết hợp",
  ylab = "Tần suất",
  xlab = "Lợi nhuận (triệu VND/ha)"
)

mono_profit_drought <- (mono_maize_yield_drought * maize_price) - maize_cost
mean(mono_profit_drought > 0)
hist(
  mono_profit_drought,
  breaks = 40,
  col = "rosybrown1",
  border = "white",
  main = "Ngô độc canh",
  ylab = "Tần suất",
  xlab = "Lợi nhuận (triệu VND/ha)"
)
# Kết quả kỳ vọng của quyết định
decision_profit <- AF_profit_drought - mono_profit_drought
mean(decision_profit > 0)
hist(
  decision_profit,
  breaks = 40,
  col = "cyan4",
  border = "white",
  main = NULL,
  ylab = "Tần suất",
  xlab = "Giá trị kỳ vọng (triệu VND/ha)"
)


