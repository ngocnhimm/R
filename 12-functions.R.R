# ==============================================================================
# BÀI TẬP THỰC HÀNH
# ==============================================================================

# ------------------------------------------------------------------------------
# Bài tập 1: Function cơ bản
# ------------------------------------------------------------------------------

# 1. Viết function tính diện tích hình chữ nhật
# Input: chiều dài, chiều rộng
# Output: diện tích

calculate_rectangle_area <- function(length, width) {
  if (!is.numeric(length) || !is.numeric(width)) {
    stop("Length and width must be numeric.")
  }
  if (length <= 0 || width <= 0) {
    stop("Length and width must be greater than 0.")
  }
  
  area <- length * width
  return(area)
}

# Sample data
length_value <- 8
width_value <- 5
calculate_rectangle_area(length_value, width_value)


# 2. Viết function tính chu vi hình tròn
# Input: bán kính
# Output: chu vi

calculate_circle_circumference <- function(radius) {
  if (!is.numeric(radius)) {
    stop("Radius must be numeric.")
  }
  if (radius <= 0) {
    stop("Radius must be greater than 0.")
  }
  
  circumference <- 2 * pi * radius
  return(circumference)
}

# Sample data
radius_value <- 7
calculate_circle_circumference(radius_value)

# 3. Viết function chuyển đổi nhiệt độ từ Celsius sang Fahrenheit
# Công thức: F = C * 9/5 + 32

celsius_to_fahrenheit <- function(celsius) {
  if (!is.numeric(celsius)) {
    stop("Temperature must be numeric.")
  }
  
  fahrenheit <- celsius * 9/5 + 32
  return(fahrenheit)
}

# Sample data
celsius_value <- 30
celsius_to_fahrenheit(celsius_value)

# ------------------------------------------------------------------------------
# Bài tập 2: Function với validation
# ------------------------------------------------------------------------------

# 1. Viết function kiểm tra số chẵn/lẻ
# Input: một số nguyên
# Output: "Chẵn" hoặc "Lẻ"
# Validate: input phải là số nguyên

check_even_odd <- function(x) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
    stop("Input must be a single numeric value.")
  }
  if (x %% 1 != 0) {
    stop("Input must be an integer.")
  }
  
  if (x %% 2 == 0) {
    return("Even")
  } else {
    return("Odd")
  }
}

# Sample data
number_value <- 15
check_even_odd(number_value)


# 2. Viết function tính điểm trung bình
# Input: vector điểm số
# Output: điểm trung bình
# Validate: 
#   - Điểm phải từ 0 đến 10
#   - Loại bỏ giá trị NA

calculate_average_score <- function(scores) {
  if (!is.numeric(scores)) {
    stop("Scores must be a numeric vector.")
  }
  
  scores <- scores[!is.na(scores)]
  
  if (length(scores) == 0) {
    stop("No valid scores available after removing NA values.")
  }
  
  if (any(scores < 0 | scores > 10)) {
    stop("Scores must be between 0 and 10.")
  }
  
  average_score <- mean(scores)
  return(average_score)
}

# Sample data
score_vector <- c(8, 7.5, 9, NA, 6.5, 10)
calculate_average_score(score_vector)

# ------------------------------------------------------------------------------
# Bài tập 3: Function thống kê
# ------------------------------------------------------------------------------

# 1. Viết function tính toán tổng quan
# Input: vector số
# Output: list(mean, median, sd, min, max, range)

calculate_summary_statistics <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    stop("The vector has no valid numeric values.")
  }
  
  result <- list(
    mean   = mean(x),
    median = median(x),
    sd     = sd(x),
    min    = min(x),
    max    = max(x),
    range  = max(x) - min(x)
  )
  
  return(result)
}

# Sample data
numeric_vector <- c(12, 15, 18, 20, 25, NA, 30)
calculate_summary_statistics(numeric_vector)


# 2. Viết function tính hoán vị P(n, r)
# Công thức: P(n,r) = n! / (n-r)!

calculate_permutation <- function(n, r) {
  if (!is.numeric(n) || !is.numeric(r) || n %% 1 != 0 || r %% 1 != 0) {
    stop("n and r must be integers.")
  }
  if (n < 0 || r < 0) {
    stop("n and r must be non-negative.")
  }
  if (r > n) {
    stop("r must be less than or equal to n.")
  }
  
  permutation <- factorial(n) / factorial(n - r)
  return(permutation)
}

# Sample data
n_value <- 5
r_value <- 3
calculate_permutation(n_value, r_value)

# 3. Viết function tính tổ hợp C(n, r)
# Công thức: C(n,r) = n! / (r! * (n-r)!)

calculate_combination <- function(n, r) {
  if (!is.numeric(n) || !is.numeric(r) || n %% 1 != 0 || r %% 1 != 0) {
    stop("n and r must be integers.")
  }
  if (n < 0 || r < 0) {
    stop("n and r must be non-negative.")
  }
  if (r > n) {
    stop("r must be less than or equal to n.")
  }
  
  combination <- factorial(n) / (factorial(r) * factorial(n - r))
  return(combination)
}

# Sample data
n_value <- 6
r_value <- 2
calculate_combination(n_value, r_value)

# ------------------------------------------------------------------------------
# Bài tập 4: Function nâng cao
# ------------------------------------------------------------------------------

# 1. Viết function tìm các số nguyên tố từ 1 đến n
# Input: n
# Output: vector các số nguyên tố

find_prime_numbers <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n %% 1 != 0) {
    stop("n must be a single integer.")
  }
  if (n < 2) {
    return(integer(0))
  }
  
  is_prime <- function(num) {
    if (num < 2) return(FALSE)
    for (i in 2:floor(sqrt(num))) {
      if (num %% i == 0) return(FALSE)
    }
    return(TRUE)
  }
  
  primes <- c()
  for (i in 2:n) {
    if (is_prime(i)) {
      primes <- c(primes, i)
    }
  }
  
  return(primes)
}

# Sample data
prime_limit <- 30
find_prime_numbers(prime_limit)


# 2. Viết function tạo tam giác Pascal với n hàng
# Gợi ý: Sử dụng tổ hợp C(n, k)


generate_pascals_triangle <- function(n) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n %% 1 != 0) {
    stop("n must be a single integer.")
  }
  if (n <= 0) {
    stop("n must be greater than 0.")
  }
  
  triangle <- list()
  
  for (i in 0:(n - 1)) {
    row <- c()
    for (k in 0:i) {
      row <- c(row, calculate_combination(i, k))
    }
    triangle[[i + 1]] <- row
  }
  
  return(triangle)
}

# Sample data
rows_value <- 6
generate_pascals_triangle(rows_value)


# 3. Viết function phân loại sinh viên dựa vào điểm
# Input: điểm số
# Output: xếp loại (Xuất sắc, Giỏi, Khá, TB, Yếu)
# Kèm theo GPA scale 4.0

classify_student <- function(score) {
  if (!is.numeric(score) || length(score) != 1 || is.na(score)) {
    stop("Score must be a single numeric value.")
  }
  if (score < 0 || score > 10) {
    stop("Score must be between 0 and 10.")
  }
  
  if (score >= 9) {
    classification <- "Excellent"
    gpa <- 4.0
  } else if (score >= 8) {
    classification <- "Good"
    gpa <- 3.5
  } else if (score >= 6.5) {
    classification <- "Fair"
    gpa <- 3.0
  } else if (score >= 5) {
    classification <- "Average"
    gpa <- 2.0
  } else {
    classification <- "Poor"
    gpa <- 1.0
  }
  
  return(list(
    score = score,
    classification = classification,
    GPA_4_scale = gpa
  ))
}
# Sample data
student_score <- 8.4
classify_student(student_score)


# ------------------------------------------------------------------------------
# Bài tập 5: Ứng dụng thực tế
# ------------------------------------------------------------------------------

# 1. Viết function tính lương ròng
# Input: lương cơ bản, phụ cấp, số ngày làm việc, số giờ tăng ca
# Output: lương ròng sau thuế

calculate_net_salary <- function(base_salary, allowance, working_days, overtime_hours) {
  if (!all(sapply(list(base_salary, allowance, working_days, overtime_hours), is.numeric))) {
    stop("All inputs must be numeric.")
  }
  if (base_salary < 0 || allowance < 0 || working_days < 0 || overtime_hours < 0) {
    stop("Inputs must not be negative.")
  }
  
  daily_salary <- base_salary / 26
  working_salary <- daily_salary * working_days
  overtime_pay <- overtime_hours * 50000
  
  total_income <- working_salary + allowance + overtime_pay
  tax <- total_income * 0.10
  net_salary <- total_income - tax
  
  return(list(
    working_salary = working_salary,
    allowance = allowance,
    overtime_pay = overtime_pay,
    total_income = total_income,
    tax = tax,
    net_salary = net_salary
  ))
}

# Sample data
base_salary_value <- 12000000
allowance_value <- 1500000
working_days_value <- 24
overtime_hours_value <- 12
calculate_net_salary(base_salary_value, allowance_value, working_days_value, overtime_hours_value)


# 2. Viết function chuẩn hóa điểm thi
# Input: vector điểm thô
# Output: vector điểm chuẩn hóa (0-100)
# Công thức: (điểm - min) / (max - min) * 100


normalize_scores <- function(scores) {
  if (!is.numeric(scores)) {
    stop("Input must be a numeric vector.")
  }
  
  valid_scores <- scores[!is.na(scores)]
  
  if (length(valid_scores) == 0) {
    stop("No valid scores available.")
  }
  
  min_score <- min(valid_scores)
  max_score <- max(valid_scores)
  
  if (min_score == max_score) {
    return(rep(100, length(scores)))
  }
  
  normalized_scores <- (scores - min_score) / (max_score - min_score) * 100
  return(normalized_scores)
}

# Sample data
raw_scores <- c(45, 60, 75, 90, 100, NA)
normalize_scores(raw_scores)


# 3. Viết function phân tích dữ liệu sinh viên
# Input: data frame (tên, tuổi, điểm)
# Output: thống kê mô tả đầy đủ

analyze_student_data <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  
  required_columns <- c("name", "age", "score")
  if (!all(required_columns %in% names(df))) {
    stop("Data frame must contain the following columns: name, age, score.")
  }
  
  if (!is.numeric(df$age) || !is.numeric(df$score)) {
    stop("Columns 'age' and 'score' must be numeric.")
  }
  
  result <- list(
    number_of_students = nrow(df),
    average_age = mean(df$age, na.rm = TRUE),
    min_age = min(df$age, na.rm = TRUE),
    max_age = max(df$age, na.rm = TRUE),
    average_score = mean(df$score, na.rm = TRUE),
    median_score = median(df$score, na.rm = TRUE),
    sd_score = sd(df$score, na.rm = TRUE),
    min_score = min(df$score, na.rm = TRUE),
    max_score = max(df$score, na.rm = TRUE),
    passed_students = sum(df$score >= 5, na.rm = TRUE),
    failed_students = sum(df$score < 5, na.rm = TRUE)
  )
  
  return(result)
}

# Sample data
student_data <- data.frame(
  name = c("Alice", "Bob", "Charlie", "David", "Emma"),
  age = c(20, 21, 19, 22, 20),
  score = c(8.5, 6.0, 9.2, 4.8, 7.3)
)

analyze_student_data(student_data)
