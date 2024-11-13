library(httr)
library(jsonlite)

library(httr)
library(jsonlite)
library(dplyr)

library(httr)
library(jsonlite)
library(dplyr)

# # 设置请求的基础 URL
# base_url <- "https://data.transportation.gov/resource/85tf-25kj.json"
# 
# # 每次请求的记录数
# limit <- 1000
# # 初始化偏移量
# offset <- 0
# # 初始化一个空的数据框以存储结果
# all_data <- data.frame()
# 
# repeat {
#   # 格式化 offset 为整数字符串，避免科学计数法
#   formatted_offset <- format(offset, scientific = FALSE)
#   
#   # 构建带有 $limit 和 $offset 参数的请求 URL
#   url <- paste0(base_url, "?$limit=", limit, "&$offset=", formatted_offset)
#   
#   # 发送 GET 请求
#   response <- GET(url)
#   
#   # 检查响应状态
#   if (status_code(response) != 200) {
#     print(paste("Error:", status_code(response)))
#     print(content(response, as = "text"))
#     break
#   }
#   
#   # 将 JSON 内容转换为数据框
#   data <- fromJSON(content(response, "text"))
#   
#   # 合并当前页数据到总数据框中
#   all_data <- bind_rows(all_data, data)
#   
#   # 检查是否获取到少于 `limit` 条记录，如果是，说明已到达数据末尾
#   if (nrow(data) < limit) {
#     break
#   }
#   
#   # 增加偏移量以获取下一页数据
#   offset <- offset + limit
# }
# 
# # 打印总记录数
# print(paste("Total records fetched:", nrow(all_data)))
# print(all_data)


library(httr)
library(jsonlite)
library(dplyr)

# 设置请求的基础 URL
base_url <- "https://data.transportation.gov/resource/85tf-25kj.json"

# 每次请求的记录数
limit <- 1000
# 初始化偏移量
offset <- 0
# 初始化一个空的数据框以存储结果
all_data <- data.frame()

# 设置需要获取的年份并将其转换为字符串格式
year_filter <- c("2021", "2023", "2024")
year_filter_query <- paste0("'", paste(year_filter, collapse = "','"), "'")

# 编码 $where 子句，确保格式正确
where_clause <- URLencode(paste0("year IN(", year_filter_query, ")"))

repeat {
  # 格式化 offset 为整数字符串，避免科学计数法
  formatted_offset <- format(offset, scientific = FALSE)
  
  # 构建带有 $limit、$offset 和 $where 参数的请求 URL
  url <- paste0(base_url, "?$limit=", limit, "&$offset=", formatted_offset, "&$where=", where_clause)
  
  # 发送 GET 请求
  response <- GET(url)
  
  # 检查响应状态
  if (status_code(response) != 200) {
    print(paste("Error:", status_code(response)))
    print(content(response, as = "text"))
    break  
  }
  
  # 将 JSON 内容转换为数据框
  data <- fromJSON(content(response, "text"))
  
  # 合并当前页数据到总数据框中
  all_data <- bind_rows(all_data, data)
  
  # 检查是否获取到少于 `limit` 条记录，如果是，说明已到达数据末尾
  if (nrow(data) < limit) {
    break
  }
  
  # 增加偏移量以获取下一页数据
  offset <- offset + limit
}

# 打印总记录数
print(paste("Total records fetched for years", paste(year_filter, collapse = ", "), ":", nrow(all_data)))
print(all_data)



