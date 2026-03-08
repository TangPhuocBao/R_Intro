# Load bộ dataset
data <- read.csv("C:/R/labs/data/FIFA/fifa21_raw_data.csv", na.strings = c("", "NA", "N/A", "None"))
# Xem bộ dataset 5 dòng đầu tiên
View(head(data, 10))
# Cấu trúc của bộ dataset
str(data)
# Tóm tắt thống kê 
summary(data)

# BƯỚC 2: Xử lý Missing Data
# Phát hiện missing data
# Tìm các dòng có dữ liệu thiếu
data[!complete.cases(data), ]
sum(!complete.cases(data))

# Xử lí dữ liệu thiếu tại Loan.Date.End : Ngày hết hạn hợp đồng cho mượ
# Kiểm tra tỷ lệ phần trăm NA
mean(is.na(data$Loan.Date.End)) * 100

# Tạo cột mới: on_loan (Có đang đi mượn hay không)
data$on_loan <- "No"
data$on_loan[!is.na(data$Loan.Date.End)] <- "Yes"
# Chuyển đổi dữ liệu thành factor (Dữ liệu phân loại)
data$on_loan <- factor(data$on_loan)
# Loại bỏ cột loan_date_end trong bộ FIFA
data$Loan.Date.End <- NULL
#Liệt kê tên các cột có NA
colnames(data)[colSums(is.na(data)) > 0]

# BƯỚC 3 : Xử lí kiểu dữ liệu
# Xử lí kiểu dữ liệu Nationality
summary(data$Nationality)
data$Nationality <- factor(data$Nationality)

# Xử lí dữ liệu Positions
summary(factor(data$Positions))
data$Positions <- factor(data$Positions)

# Xử lí dữ liệu Team...Contract 
summary(factor(data$Team...Contract))
data$Team...Contract <- factor(data$Team...Contract)

# Xử lí dữ liệu height
# Tách ra thành 2 cột tạm thời (feet và inches)
# Chúng ta dùng hàm strsplit để "cắt" chuỗi
h <- do.call(rbind, strsplit(as.character(data$Height), "['\"]"))
# cột 1 là feet, cột 2 là inches của h
# Tính theo công thức (feet * 12 + inches) * 2.54
data$height_cm <- (as.numeric(h[, 1]) * 12 + as.numeric(h[, 2])) * 2.54
# xóa cột Height
data$Height <- NULL
summary(data$height_cm)

# Xử lí dữ liệu weight
# Tách lấy phần số trước chữ "lbs"
# Dùng strsplit để cắt tại chữ "l"
w <- do.call(rbind, strsplit(as.character(data$Weight), "lbs"))
# Chuyển đổi sang kg
data$weight_kg <- as.numeric(w[, 1]) * 0.4536
# Xóa cột Weight
data$Weight <- NULL
summary(data$weight_kg)

# XỬ lí dữ liệu Foot
summary(factor(data$foot))
data$foot <- factor(data$foot)

# Xử lí dữ liệu BP : Best Position (Vị trí tốt nhất)
summary(factor(data$BP))
data$BP <- factor(data$BP)

# Xử lí cột dữ liệu Growt
# Không chuyển đổi sang dạng factor vì Growth là khoảng cách giữa hai con số (Potential - Overall)
summary(data$Growth)

# Chuyển đổi dữ liệu Joined
summary((data$Joined))
data$Joined <- as.Date(data$Joined, format = "%b %d, %Y")

# Xử lí cột Value
# 1. Lấy phần số ra (bỏ ký tự €, M, K)
num_val <- as.numeric(gsub("[€MK]", "", data$Value))

# 2. Tạo bộ hệ số nhân: M=1000000, K=1000, còn lại=1
# Dùng hàm 'sub' để lấy ký tự cuối cùng, rồi chuyển nó thành hệ số
char_val <- substr(data$Value, nchar(data$Value), nchar(data$Value))
multiplier <- c("M" = 1000000, "K" = 1000)[char_val]
multiplier[is.na(multiplier)] <- 1 # Nếu không có M hay K thì nhân với 1

# 3. Tính giá trị cuối cùng
data$Value <- num_val * multiplier
summary(data$Value)

# Xử lí cột Wage : Lương tuần
# 1. Lấy phần số ra (bỏ ký tự €, M, K)
num_val <- as.numeric(gsub("[€MK]", "", data$Wage))

# 2. Tạo bộ hệ số nhân: M=1000000, K=1000, còn lại=1
# Dùng hàm 'sub' để lấy ký tự cuối cùng, rồi chuyển nó thành hệ số
char_val <- substr(data$Wage, nchar(data$Wage), nchar(data$Wage))
multiplier <- c("M" = 1000000, "K" = 1000)[char_val]
multiplier[is.na(multiplier)] <- 1 # Nếu không có M hay K thì nhân với 1

# 3. Tính giá trị cuối cùng
data$Wage <- num_val * multiplier
summary(data$Wage)

# Xử lí cột Release.Clause
# 1. Lấy phần số ra
num_val <- as.numeric(gsub("[€MK]", "", data$Release.Clause))

# 2. Tạo bộ hệ số nhân
char_val <- substr(data$Release.Clause, nchar(data$Release.Clause), nchar(data$Release.Clause))
multiplier <- c("M" = 1000000, "K" = 1000)[char_val]

multiplier[is.na(multiplier)] <- 1 # nếu không có M hay K thì nhân 1

# 3. Tính giá trị
data$Release.Clause <- num_val * multiplier
# Xem lại dữ liệu
summary(data$Release.Clause)

# 
summary(data)

# Xử lí cột Hits
# 1. Lấy phần số ra
num_val <- as.numeric(gsub("[K]", "", data$Hits))

# 2. Tạo bộ hệ số nhân
char_val <- substr(data$Hits, nchar(data$Hits), nchar(data$Hits))
multiplier <- c("K" = 1000)[char_val]

multiplier[is.na(multiplier)] <- 1 # nếu không có K thì nhân 1

# 3. TÍnh giá trị
data$Hits <- num_val * multiplier

# Xem lại dữ liệu
summary(data$Hits)


# Xử lí cột Hit
# 1. Lấy phần số ra
num_val <- as.numeric(gsub("[K]", "", data$Hit))

# 2. Tạo bộ hệ số nhân
char_val <- substr(data$Hit, nchar(data$Hit), nchar(data$Hit))
multiplier <- c("K" = 1000)[char_val]

multiplier[is.na(multiplier)] <- 1 # nếu không có K thì nhân 1

# 3. TÍnh giá trị
data$Hit <- num_val * multiplier

# Xem lại dữ liệu
summary(data$Hit)

# Xử lí cột W.F
# 1. Kiểm tra có phù hợp với kiệu dữ liệu factor
summary(factor(data$W.F))
# 2. Chỉnh sửa kiểu dữ liệu
data$W.F <- factor(data$W.F,
                   levels = c("1 ★", "2 ★", "3 ★", "4 ★", "5 ★"),
                   ordered = TRUE)
summary(data$W.F)
# Xử lí cột SM
# 1. Kiểm tra có phù hợp với kiểu dữ liệu factor
summary(factor(data$SM))
# 2. Chỉnh sử kiểu dữ liệu
data$SM <- factor(data$SM,
                   levels = c("1★", "2★", "3★", "4★", "5★"),
                   ordered = TRUE)
summary(data$SM)

# Xử lí cột A.W, D.W
levels_workrate <- c("Low", "Medium", "High")
# 1. Xử lí cột A.W
summary(factor(data$A.W))
data$A.W <- factor(data$A.W,
                  levels = levels_workrate,
                  ordered = TRUE)
summary(data$A.W)

# 2. Xử lí cột D.W
# 1. Xử lí cột A.W
summary(factor(data$D.W))
data$D.W <- factor(data$D.W,
                   levels = levels_workrate,
                   ordered = TRUE)
summary(data$D.W)

# Xử lí cột IR 
# 1. Kiểm tra có phù hợp với kiệu dữ liệu factor
summary(factor(data$IR))
# 2. Chỉnh sửa kiểu dữ liệu
data$IR <- factor(data$IR,
                   levels = c("1 ★", "2 ★", "3 ★", "4 ★", "5 ★"),
                   ordered = TRUE)

summary(data$IR)

