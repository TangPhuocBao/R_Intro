### BÀI TẬP THÊM
data("freeny")
View(freeny)
str(freeny)

# Visialization
plot(as.numeric(freeny$y), 
     freeny$price.index, 
     type = "o", 
     main = "Biểu đồ thể hiện Price",
     xlab= "X",
     ylab= "Y",
     pch = 16,
     col = "skyblue")

lines(as.numeric(freeny$y),
     freeny$income.level,
     type = "o",
     pch = 17,
     col = "green")

