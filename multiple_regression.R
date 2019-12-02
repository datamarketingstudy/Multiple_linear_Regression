#.선형 회귀를 이용한 의료비 예측


##, 데이터 탐색 및 준비
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)

str(insurance)

summary(insurance$expenses)

hist(insurance$bmi)
summary(insurance$bmi)

# 상관행렬

cor(insurance[c("age","bmi","children","expenses")])

# 특정 간 관계 시각화 : 산포도 행렬

library(psych)
pairs(insurance[c("age","bmi","children","expenses")])
pairs.panels(insurance[c("age","bmi","children","expenses")])

##. 3단계 데이터에 대한 모델 훈련

ins_model <- lm(expenses ~ ., data = insurance)
ins_model

##. 4단계 모델 성능 평가

summary(ins_model)

# 독립변수 다중공선성 평가
car::vif(ins_model)
sqrt(car::vif(ins_model)) > 2

##. 5단계 모델 성능 개선

###. 'Age' 비선형 관계 추가
insurance$age2 <- insurance$age^2

###. 'BMI' 수치변수를 이진 지시 변수로 전환
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

###. 비만과 흡연의 상호작용 영향 추가 (BMI 30 x Smoker)
####. 비만 지시 변수와 흡연 지시 변수를 상호작용 시키기 위해 expenses ~ bmi30*smoker 형식으로 작성
####. R에서는 위 명령은 expenses ~ bmi30 + smokeryes + bmi30:smokeryes (-> 두 변수 간의 상호작용이라는 것을 나타냄)

ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)

### 변수 선택법 활용
ins_model3 <- step(ins_model2, direction = "backward")
summary(ins_model3)

par(mfrow <- c(2,2))
plot(ins_model3)

### 예측
library(dplyr)
test_ins <- insurance
predict_ins <- predict(ins_model2, newdata = test_ins)
result_ins <- test_ins %>% mutate(predict = predict_ins)
View(result_ins)

### 예측 결과 저장
library(data.table)
fwrite(result_ins, "result.csv")
