# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

# ------------------------------------------------------------------------------
# Bài tập 1: Bar Chart
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu sau:
subjects <- c("Toán", "Lý", "Hóa", "Văn", "Anh")
scores <- c(8, 7.5, 9, 8.5, 7)

# Yêu cầu:
# 1. Vẽ bar chart cơ bản
barplot(scores)

# 2. Thêm tiêu đề "Điểm thi của bạn"
barplot(
  scores,
  main = "Điểm thi của bạn"
)
# 3. Tô màu khác nhau cho mỗi môn
colors_subjects <- c("red", "blue", "green", "orange", "purple")

barplot(
  scores,
  names.arg = subjects,
  col = colors_subjects,
  main = "Điểm thi của bạn",
  xlab = "Môn học",
  ylab = "Điểm",
  ylim = c(0, 10)
)

# 4. Thêm giá trị điểm lên đầu mỗi cột
bar_positions <- barplot(
  scores,
  names.arg = subjects,
  col = colors_subjects,
  main = "Điểm thi của bạn",
  xlab = "Môn học",
  ylab = "Điểm",
  ylim = c(0, 10)
)

text(
  x = bar_positions,
  y = scores,
  labels = scores,
  pos = 3,
  cex = 0.9
)

# 5. Vẽ bar chart ngang
barplot(
  scores,
  names.arg = subjects,
  col = colors_subjects,
  main = "Điểm thi của bạn - Biểu đồ cột ngang",
  xlab = "Điểm",
  ylab = "Môn học",
  xlim = c(0, 10),
  horiz = TRUE
)


# ------------------------------------------------------------------------------
# Bài tập 2: Histogram
# ------------------------------------------------------------------------------

# Tạo dữ liệu: Điểm thi của 100 sinh viên
set.seed(2024)
exam_scores <- rnorm(100, mean = 70, sd = 10)

# Yêu cầu:
# 1. Vẽ histogram với 10 bins
hist(
  exam_scores,
  breaks = 10
)
# 2. Thêm tiêu đề và nhãn trục phù hợp
# 3. Tô màu xanh lam
hist(
  exam_scores,
  breaks = 10,
  col = "lightblue",
  main = "Biểu đồ Histogram điểm thi (10 bins)",
  xlab = "Điểm thi",
  ylab = "Tần số",
  border = "white"
)
# 4. Thêm đường thẳng đứng màu đỏ tại vị trí điểm trung bình
abline(
  v = mean(exam_scores),
  col = "red",
  lwd = 2
)
# 5. Vẽ histogram khác với 20 bins, so sánh sự khác biệt
hist(
  exam_scores,
  breaks = 20,
  col = "lightblue",
  main = "Biểu đồ Histogram điểm thi (20 bins)",
  xlab = "Điểm thi",
  ylab = "Tần số",
  border = "white"
)

abline(
  v = mean(exam_scores),
  col = "red",
  lwd = 2
)

# ------------------------------------------------------------------------------
# Bài tập 3: Box Plot
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu iris

# Yêu cầu:
# 1. Vẽ box plot so sánh Petal.Length giữa 3 loài
# 2. Tô màu khác nhau cho mỗi loài
# 3. Thêm tiêu đề phù hợp
boxplot(
  Petal.Length ~ Species,
  data = iris,
  col = c("pink", "lightgreen", "lightblue"),
  main = "So sánh Petal.Length giữa 3 loài hoa iris",
  xlab = "Loài",
  ylab = "Petal.Length"
)

# 4. Nhìn vào biểu đồ và trả lời:
#    - Loài nào có petal dài nhất? ->> virginica
#    - Loài nào có độ biến thiên lớn nhất? ->> virginica
#    - Có outliers không? Ở loài nào? ->> quan sát trên boxplot, thường xuất hiện ở versicolor hoặc virginica

# ------------------------------------------------------------------------------
# Bài tập 4: Scatter Plot
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu mtcars

# Yêu cầu:
# 1. Vẽ scatter plot giữa hp (horsepower) và mpg
# 2. Tô màu các điểm theo số cy-lanh (cyl)

cyl_colors <- c("red", "blue", "darkgreen")
point_colors <- cyl_colors[as.factor(mtcars$cyl)]

plot(
  mtcars$hp,
  mtcars$mpg,
  col = point_colors,
  pch = 19,
  main = "Biểu đồ phân tán giữa hp và mpg",
  xlab = "Mã lực (hp)",
  ylab = "Số dặm/gallon (mpg)"
)

# 3. Thêm đường hồi quy tuyến tính
model <- lm(mpg ~ hp, data = mtcars)
abline(model, col = "black", lwd = 2)

# 4. Thêm legend giải thích màu
legend(
  "topright",
  legend = levels(as.factor(mtcars$cyl)),
  col = cyl_colors,
  pch = 19,
  title = "Số xy-lanh"
)

# 5. Nhận xét về mối quan hệ giữa hp và mpg
#->> hp và mpg có mối quan hệ nghịch
#->> Xe có hp càng cao thì mpg thường càng thấp



# ------------------------------------------------------------------------------
# Bài tập 5: Nhiều biểu đồ
# ------------------------------------------------------------------------------

# Sử dụng dữ liệu mtcars
par(mfrow = c(2, 2))

# Yêu cầu:
# Tạo một figure với 4 biểu đồ (2x2) để phân tích biến hp:
# 1. Histogram của hp
hist(
  mtcars$hp,
  col = "lightblue",
  main = "Histogram của hp",
  xlab = "hp",
  border = "white"
)

# 2. Box plot của hp
boxplot(
  mtcars$hp,
  col = "lightgreen",
  main = "Box plot của hp",
  ylab = "hp"
)

# 3. Box plot so sánh hp theo cyl
boxplot(
  hp ~ cyl,
  data = mtcars,
  col = c("pink", "orange", "lightblue"),
  main = "So sánh hp theo cyl",
  xlab = "Số xy-lanh",
  ylab = "hp"
)

# 4. Scatter plot hp vs mpg

plot(
  mtcars$hp,
  mtcars$mpg,
  col = "purple",
  pch = 19,
  main = "Scatter plot giữa hp và mpg",
  xlab = "hp",
  ylab = "mpg"
)

abline(lm(mpg ~ hp, data = mtcars), col = "red", lwd = 2)


# ------------------------------------------------------------------------------
# Bài tập 6: Tổng hợp
# ------------------------------------------------------------------------------

# Tạo dữ liệu bán hàng của 4 quý
Q1 <- c(100, 120, 110, 130)
Q2 <- c(150, 140, 160, 155)
Q3 <- c(180, 170, 190, 185)
Q4 <- c(200, 210, 195, 220)
products <- c("Sản phẩm A", "Sản phẩm B", "Sản phẩm C", "Sản phẩm D")

sales_matrix <- rbind(Q1, Q2, Q3, Q4)
colnames(sales_matrix) <- products
rownames(sales_matrix) <- c("Q1", "Q2", "Q3", "Q4")

# Yêu cầu:
# 1. Vẽ grouped bar chart so sánh doanh thu 4 quý
barplot(
  sales_matrix,
  beside = TRUE,
  col = c("red", "blue", "green", "orange"),
  main = "Biểu đồ cột nhóm doanh thu 4 quý",
  xlab = "Sản phẩm",
  ylab = "Doanh thu",
  legend.text = rownames(sales_matrix)
)

# 2. Vẽ line plot cho từng sản phẩm qua 4 quý
matplot(
  t(sales_matrix),
  type = "b",
  pch = 1:4,
  lty = 1,
  col = 1:4,
  xaxt = "n",
  main = "Biểu đồ đường doanh thu của từng sản phẩm qua 4 quý",
  xlab = "Quý",
  ylab = "Doanh thu"
)

axis(1, at = 1:4, labels = rownames(sales_matrix))
legend(
  "topleft",
  legend = products,
  col = 1:4,
  lty = 1,
  pch = 1:4
)

# 3. Tính tổng doanh thu mỗi quý, vẽ bar chart
total_by_quarter <- rowSums(sales_matrix)

barplot(
  total_by_quarter,
  col = c("red", "blue", "green", "orange"),
  main = "Tổng doanh thu theo quý",
  xlab = "Quý",
  ylab = "Tổng doanh thu"
)

# 4. Tạo figure 2x2 hiển thị:
par(mfrow = c(2, 2))
#    4.1 Grouped bar chart
#    4.2 Line plot tất cả sản phẩm
#    4.3 Pie chart tổng doanh thu mỗi quý
#    4.4 Bar chart tổng doanh thu mỗi sản phẩm


# 4.1 Grouped bar chart
barplot(
  sales_matrix,
  beside = TRUE,
  col = c("red", "blue", "green", "orange"),
  main = "Biểu đồ cột nhóm",
  xlab = "Sản phẩm",
  ylab = "Doanh thu",
  legend.text = rownames(sales_matrix)
)

# 4.2 Line plot tất cả sản phẩm
matplot(
  t(sales_matrix),
  type = "b",
  pch = 1:4,
  lty = 1,
  col = 1:4,
  xaxt = "n",
  main = "Biểu đồ đường các sản phẩm",
  xlab = "Quý",
  ylab = "Doanh thu"
)
axis(1, at = 1:4, labels = rownames(sales_matrix))
legend(
  "topleft",
  legend = products,
  col = 1:4,
  lty = 1,
  pch = 1:4,
  cex = 0.8
)

# 4.3 Pie chart tổng doanh thu mỗi quý
pie(
  total_by_quarter,
  col = c("red", "blue", "green", "orange"),
  main = "Pie chart tổng doanh thu theo quý"
)

# 4.4 Bar chart tổng doanh thu mỗi sản phẩm
total_by_product <- colSums(sales_matrix)

barplot(
  total_by_product,
  col = c("purple", "cyan", "gold", "brown"),
  main = "Tổng doanh thu theo sản phẩm",
  xlab = "Sản phẩm",
  ylab = "Tổng doanh thu"
)
