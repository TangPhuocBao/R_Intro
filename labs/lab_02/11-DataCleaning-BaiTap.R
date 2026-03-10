### LẤY ĐIỂM THỰC HÀNH
### Các bài tập ######################################################################
# 1. Đọc mô tả dữ liệu phân tích tính cách khách hàng và tải 
# vào R (file clients.csv) với tên biến là "clients".
<<<<<<< HEAD
data <- read.csv("../R/labs/data/clients.csv")
View(data)
# 2. Xem qua cấu trúc dữ liệu và kiểm tra các lớp (classes) đã được gán 
# cho các biến trong bộ dữ liệu.
str(data)
summary(data)
# 3. Kiểm tra xem có giá trị nào bị thiếu trong bộ dữ liệu không.
# a) Những biến nào có chứa giá trị bị thiếu?
data[!complete.cases(data), ]
# xét dữ liệu từng cột
colSums(is.na(data))
# b) Điền các giá trị bị thiếu bằng giá trị trung bình hoặc trung vị của biến đó.
# Trước khi điền, hãy xem xét bản chất của biến. Nếu là số nguyên (ví dụ: năm sinh),
# thì hãy điền giá trị phù hợp với bản chất của biến (chúng ta không muốn năm sinh là 1995.832, phải không? ;))

# Response
summary(data$Response)
data$Response[data$Response != 0 & data$Response != 1] <- NA
data$Response[is.na(data$Response)] <- 0

# Năm sinh
summary(data$Year_Birth)
data$Year_Birth[is.na(data$Year_Birth)] <- median(data$Year_Birth, na.rm = TRUE)

#MntWines
summary(data$MntWines)
data$MntWines[is.na(data$MntWines)] <- 0
# c) Bạn sử dụng đoạn mã nào để điền các giá trị bị thiếu của Year_Birth (nếu có)?

# 4. a) Kiểm tra xem tất cả các giá trị bị thiếu đã được điền đầy đủ chưa. Nếu chưa, lặp lại bước 3.
data[!complete.cases(data), ]
# b) Bạn sẽ dùng đoạn mã nào để hiển thị tất cả các dòng vẫn còn chứa dữ liệu bị thiếu?
data[!complete.cases(data), ]
=======

# 2. Xem qua cấu trúc dữ liệu và kiểm tra các lớp (classes) đã được gán 
# cho các biến trong bộ dữ liệu.

# 3. Kiểm tra xem có giá trị nào bị thiếu trong bộ dữ liệu không.
# a) Những biến nào có chứa giá trị bị thiếu?
# b) Điền các giá trị bị thiếu bằng giá trị trung bình hoặc trung vị của biến đó.
# Trước khi điền, hãy xem xét bản chất của biến. Nếu là số nguyên (ví dụ: năm sinh),
# thì hãy điền giá trị phù hợp với bản chất của biến (chúng ta không muốn năm sinh là 1995.832, phải không? ;)).
# c) Bạn sử dụng đoạn mã nào để điền các giá trị bị thiếu của Year_Birth (nếu có)?

# 4. a) Kiểm tra xem tất cả các giá trị bị thiếu đã được điền đầy đủ chưa. Nếu chưa, lặp lại bước 3.
# b) Bạn sẽ dùng đoạn mã nào để hiển thị tất cả các dòng vẫn còn chứa dữ liệu bị thiếu?

>>>>>>> upstream/main
# 5. a) Xem xét những biến nào nên chuyển đổi thành kiểu "factor"?
# Gợi ý: Đây thường là các biến văn bản có một số giá trị cụ thể và lặp lại.
# Chúng cũng có thể là các biến được biểu diễn bằng số nhưng không mang "ý nghĩa số học"
# - ví dụ: biến "education" với các giá trị 2, 3, 4 thực chất đại diện cho các cấp độ
# giáo dục liên tiếp (ý nghĩa logic) thay vì số năm học tập chính xác (ý nghĩa số học).
<<<<<<< HEAD

# Những biến cần biến đổi kiểu dữ liệ :u
# AcceptedCmp5, AcceptedCmp4, AcceptedCmp3, AcceptedCmp2, AcceptedCmp1, Education, Response
# b) Bạn sẽ dùng đoạn mã ngắn nhất nào để chuyển đổi biến Marital_Status?
summary(factor(data$Marital_Status))

data$Marital_Status <- factor(data$Marital_Status)
=======
# b) Bạn sẽ dùng đoạn mã ngắn nhất nào để chuyển đổi biến Marital_Status?
>>>>>>> upstream/main

# 6. a) Xem xét biến nào trong số các biến đã xác định ở trên nên được
# chuyển đổi thành kiểu 'ordered factor' (biến phân loại có thứ tự).
# Gợi ý: Biến kiểu 'ordered factor' nên chứa các mức có thứ tự logic
# - ví dụ: biến 'education' với các giá trị 'primary', 'secondary'
# và 'tertiary'. Trong trường hợp này, việc giữ thứ tự các mức là quan trọng.
# Một ví dụ điển hình khác của biến ordered factor là các câu trả lời
# khảo sát sử dụng thang đo Likert (https://en.wikipedia.org/wiki/Likert_scale).
<<<<<<< HEAD

# Biến về dạng order factor AcceptedCmp5, AcceptedCmp4, AcceptedCmp3, AcceptedCmp2, AcceptedCmp1, Education
# b) Bạn sẽ dùng đoạn mã nào để chuyển đổi biến Education? Giả sử rằng
# 2n nghĩa là giáo dục trung học và graduation tương đương với bảo vệ bằng cử nhân
summary(factor(data$Education))

data$Education <- factor(data$Education,
                         levels = c("2n", "Basic", "Graduation", "Master", "PhD"),
                         ordered = TRUE)

# 7. Chuyển đổi các biến đã xác định trong bước 5 và 6 thành các lớp thích hợp.
data$AcceptedCmp1 <- factor(data$AcceptedCmp1)

data$AcceptedCmp2 <- factor(data$AcceptedCmp2)

data$AcceptedCmp3 <- factor(data$AcceptedCmp3)

data$AcceptedCmp4 <- factor(data$AcceptedCmp4)

data$AcceptedCmp5 <- factor(data$AcceptedCmp5)

data$Response <- factor(data$Response)

# 8. Lưu kết quả để tham khảo sau này! Sử dụng file RData với tên "clientsInR".
save(data, file = "../R/R_Intro//labs/lab_02/clientsInR.RData")
=======
# b) Bạn sẽ dùng đoạn mã nào để chuyển đổi biến Education? Giả sử rằng
# 2n nghĩa là giáo dục trung học và graduation tương đương với bảo vệ bằng cử nhân.

# 7. Chuyển đổi các biến đã xác định trong bước 5 và 6 thành các lớp thích hợp.

# 8. Lưu kết quả để tham khảo sau này! Sử dụng file RData với tên "clientsInR".
>>>>>>> upstream/main
