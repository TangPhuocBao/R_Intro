#### Bài tập 1: Function cơ bản #######

# # 1. Viết function tính diện tích hình chữ nhật
# Input: chiều dài, chiều rộng
# Output: diện tích

Area_hcn <- function(d, r){
  return (d * r)
}
Area_hcn(3, 2)
# 2. Viết function tính chu vi hình tròn
# Input: bán kính
# Output: chu vi

perimeter <- function(r){
  return (2 * r * 3.14)
}
perimeter(3)


# 3. Viết function chuyển đổi nhiệt độ từ Celsius sang Fahrenheit
# Công thức: F = C * 9/5 + 32

Celsius_Fahrenheit <- function(C){
  return (C * 9/5 + 32)
}
Celsius_Fahrenheit(15)

########## Bài tập 2: Function với validation #########
# 1. Viết function kiểm tra số chẵn/lẻ
# Input: một số nguyên
# Output: "Chẵn" hoặc "Lẻ"
# Validate: input phải là số nguyên

check_number <- function(number){
  if (number %% 2 == 0) {
    return ("Chẵn")
  } else {
    return("Lẻ")
  }
}
check_number(3)

# 2. Viết function tính điểm trung bình
# Input: vector điểm số
# Output: điểm trung bình
# Validate: 
#   - Điểm phải từ 0 đến 10
#   - Loại bỏ giá trị NA
cal_avg <- function(points){
  if (any(points < 0 | points > 10)){
    return ("Điểm phải từ 0 đến 10")
  } else {
    return (mean(points, na.rm = TRUE))
  }
}
cal_avg(c(3, 10, 5, 11))

######### Bài tập 3: Function thống kê ############3
# 1. Viết function tính toán tổng quan
# Input: vector số
# Output: list(mean, median, sd, min, max, range)

cal_all_math <- function(numbers){
  return (list(
    mean(numbers, na.rm = TRUE),
    median(numbers, na.rm = TRUE),
    sd(numbers, na.rm = TRUE),
    min(numbers, na.rm = TRUE),
    max(numbers, na.rm= TRUE),
    range(numbers, na.rm = TRUE)
  ))
}

cal_all_math(c(3, 10, 5, 11, 2))

# 2. Viết function tính hoán vị P(n, r)
# Công thức: P(n,r) = n! / (n-r)!

cal_P <- function(n, r){
  if (r == 0){
    return (1)
  } else {
    return (n * cal_P(n-1, r-1))
  }
}

cal_P(5, 2)

# 3. Viết function tính tổ hợp C(n, r)
# Công thức: C(n,r) = n! / (r! * (n-r)!)

cal_C <- function(n, r){
  if (r == 0 | r == n){
    return (1)
  }
  if (r > n) {
    return(0)
  } else {
  return(cal_C(n - 1, r - 1) + cal_C(n - 1, r))
  }
}

cal_C(5, 2)

######### Bài tập 4: Function nâng cao #########
# 1. Viết function tìm các số nguyên tố từ 1 đến n
# Input: n
# Output: vector các số nguyên tố
is_prime <- function(number){
  if (number <= 1) {
    return (FALSE)
  }
  if (number == 2){
    return (TRUE)
  }
  if (number %% 2 == 0){
    return (FALSE)
  }
  limit <- sqrt(number)
  for (i in 3:limit) {
    if (number %% i == 0) return(FALSE)
  }
  return(TRUE)
}

check_number_n <- function(n){
  primes <- c()
  for (i in 0:n){
    if (is_prime(i)){
      primes <- c(primes, i)
    }
  }
  return (primes)
}

check_number_n(20)
# 2. Viết function tạo tam giác Pascal với n hàng
# Gợi ý: Sử dụng tổ hợp C(n, k)


# 3. Viết function phân loại sinh viên dựa vào điểm
# Input: điểm số
# Output: xếp loại (Xuất sắc, Giỏi, Khá, TB, Yếu)
# Kèm theo GPA scale 4.0
do_classification <- function(n) {
  dtb <- cal_avg(n)
  
  if (dtb >= 9) {
    res <- "Xuat sac"
  } else if (dtb >= 8.5) {
    res <- "Gioi"
  } else if (dtb >= 7) {
    res <- "Kha"
  } else if (dtb >= 5) {
    res <- "Trung Binh"
  } else {
    res <- "Yeu"
  }
  
  # Trả về một list chứa cả 2 giá trị
  return(list(res, dtb * 0.4))
}
diem_so <- c(10, 5, 2, 3, 5, 2)
do_classification(diem_so)

### Bài tập 5: Ứng dụng thực tế
```r
# 1. Viết function tính lương ròng
# Input: lương cơ bản, phụ cấp, số ngày làm việc, số giờ tăng ca
# Output: lương ròng sau thuế
cal_pay <- function(pay_nom, allowance, day, hour){
  gross <- pay_nom / 26 * day + allowance + (pay_nom / (26 * 8)) * hour * 1.5
  bh <- gross * 0.105
  result <- gross - bh
  return (result)
}

cal_pay(5000000, 500000, 20, 8)


# 2. Viết function chuẩn hóa điểm thi
# Input: vector điểm thô
# Output: vector điểm chuẩn hóa (0-100)
# Công thức: (điểm - min) / (max - min) * 100
norm <- function(v_point){
  result <- (v_point - min(v_point)) / ((max(v_point) - min(v_point)) * 100)
  return (result)
}

v_point <- c(10, 5, 2, 5, 7, 9, 5)
norm(v_point)
# 3. Viết function phân tích dữ liệu sinh viên
# Input: data frame (tên, tuổi, điểm)
# Output: thống kê mô tả đầy đủ
analyze_data <- function(data){
  return (list(min(data$Điểm),
              max(data$Điểm),
              cal_avg(data$Điểm)))
}
data <- data.frame(Tên = c("A", "B", "C"),
                   Tuổi = c(16, 17, 18),
                   Điểm = c(7.5, 8.0, 9.5))
analyze_data(data)
