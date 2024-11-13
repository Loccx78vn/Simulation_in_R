---
  title: "Simulation in R"
subtitle: "Việt Nam, 2024"
categories: ["SupplyChainManagement", "Simulation","Agent Based Modeling"]
description: "Đây là bài viết của tôi về cách sử dụng R trong việc giả lập"
author: "Cao Xuân Lộc"
date: "2024-10-06"
number-sections: true
fig-cap-location: bottom
title-block-banner: img/autumn.jpg
title-block-banner-color: "white"
format: 
  html:
  code-fold: true
code-tools: true
theme: 
  - theme.scss 
bibliography: references.bib
---
  
  Hôm nay chúng ta sẽ qua 1 phần khác trong **Supply chain management** đó là: giả lập (*simulation*).

## Tổng quát:

```{r}
#| include: false
#| message: false
#| warning: false
pacman::p_load(
  janitor,
  tidyverse,
  dplyr,
  tidyr,
  magrittr,
  shiny,
  leaflet,
  reactable,
  leaflet.extras,
  ggplot2,
  quarto,
  reactablefmtr
)
```

### Định nghĩa:

**Simulation - giả lập** là một kĩ thuật máy tính nhằm tạo ra dữ liệu một cách ngẫu nhiên và dựa vào đó dự đoán quá trình phát triển, hoạt động của một sự vật, hiện tượng trong một đơn vị thời gian. Mục đích nhằm đánh giá mức độ hiệu quả của các giải pháp cho vấn đề, rủi ro trong tương lai.

Ví dụ giả lập mà bạn gặp thường ngày như dự báo thời tiết hay dự báo tình trạng lũ lụt, sạt lở trong thời gian mưa bão. Gần đây nhất là mô hình dự đoán số ca mắc Covid-19 nhằm đánh giá mức độ lây nhiễm ở các khu vực như ảnh dưới đây là kết quả dự báo ở Mỹ.

![Hình 1: Mô hình giả lập số lượng người bị nhiễm Covid-19](img/covid19.jpg) Thực chất nhiều đánh giá cho rằng các mô hình dự báo Covid-19 đều thất bại, tốn thời gian mà chẳng đưa ra *insight* gì đáng giá. Nhưng luôn nhớ rằng *"All models are wrong but some are useful" - George Box* nghĩa là mọi mô hình đều có sự sai lệch và nhiệm vụ của bạn là tìm ra mô hình phù hợp nhất và tạo ra giá trị trong đời thực.

Các lợi ích của việc *simulation* là:
  
  -   **Kiểm tra trực giác thống kê** hoặc minh họa các đặc tính toán học mà bạn không thể dễ dàng dự đoán. Ví dụ: Kiểm tra xem liệu có hơn 5% tác động có ý nghĩa đối với một biến trong mô hình khi dữ liệu giả ngẫu nhiên được tạo ra.

-   **Hiểu lý thuyết mẫu và phân phối xác suất** hoặc kiểm tra xem bạn có hiểu các quá trình cơ bản của hệ thống của mình hay không. Ví dụ: Xem liệu dữ liệu mô phỏng lấy từ các phân phối cụ thể có thể so sánh với dữ liệu thực tế hay không.

-   **Thực hiện phân tích độ mạnh của mẫu**. Ví dụ: Đánh giá xem kích thước mẫu (trong mỗi lần lặp của mô phỏng) có đủ lớn để phát hiện tác động mô phỏng trong hơn 80% các trường hợp hay không.

-   **Chuẩn bị kế hoạch phân tích trước**. Để tự tin về các phân tích thống kê (xác nhận) mà bạn muốn thực hiện trước khi thu thập dữ liệu (ví dụ: thông qua việc đăng ký trước hoặc báo cáo đã đăng ký), việc thực hành các phân tích trên một bộ dữ liệu mô phỏng là rất hữu ích! Nếu bạn vẫn chưa chắc chắn về bài kiểm tra thống kê phù hợp nhất để áp dụng cho dữ liệu của mình, việc cung cấp bộ dữ liệu mô phỏng cho một nhà thống kê hoặc người hướng dẫn sẽ giúp họ đưa ra các gợi ý cụ thể! Mã nguồn chứa các phân tích dữ liệu mô phỏng có thể được nộp cùng với việc đăng ký trước hoặc báo cáo đã đăng ký để các nhà phản biện hiểu rõ chính xác các phân tích bạn dự định thực hiện. Khi bạn có dữ liệu thực tế, bạn chỉ cần cắm chúng vào mã này và ngay lập tức có kết quả của các phân tích xác nhận!
  
  ### Phân loại:
  
  Theo lịch sự phát triển, *simulation* có thể đượa chia thành các phân lớp như sau:
  
  ![Hình 2: Phân nhánh của simulation](img/simulationmodel.png) {fig-location = "center"}

Trong đó:
  
  -   2 nhánh lớn là *Deterministic* vs *Stochastic*: sự khác nhau rõ ràng nhất chính là *outcome* của 2 mô hình này. Đối với *Deterministic* thì *outcome* hoàn toàn dự đoán được, ví dụ với 1 lượng thông tin đầu vào như hình dạng tay chân, kích thước, màu sắc,... thì kết quả cuối cùng là phân biệt được đâu là hình ảnh của con chó hoặc con mèo và quan trọng là không có *randomness* từ mô hình - nghĩa là kết quả chỉ có là con chó hoặc con mèo. Còn với *Stochastic* thì thông tin đầu vào sẽ không rõ ràng như *Deterministic* - có thể là do sai lệch từ việc đo lường hoặc do chưa đủ thông tin nên *outcome* của nó cũng nằm trong 1 khoảng hoặc 1 tập hợp các giá trị khả thi.

-   2 nhánh nhỏ là *Static* vs *Dynamic*: mô hình *Static* thì diễn tả một quá trình mô phỏng tại 1 điểm, 1 mốc thời gian cụ thể và mô hình *Dynamic* cũng diễn tả như vậy nhưng quá trình mô phỏng thay đổi theo thời gian. Tiêu biểu của *Static* là phương pháp *Monte Carlo* dựa trên nền tảng về *random sampling*.

## Các mô hình thông dụng trong Supply Chain Simulation:

Ứng dụng của **Simulation** trong *Supply chain* là rất nhiều, đặc biệt ở phân mảng *planning*. Việc giả lập trước các trường hợp, tình huống có thể gặp phải là quan trọng và nó giúp người quản lí đưa ra được các phương án thay thế hoặc phương án *backup* để đảm bảo chuỗi cung ứng hoạt động bình thường.

Riêng trong ngành Supply Chain thì các mô hình được sử dụng nhiều nhất bao gồm: *DES - Discrete Event Simulation* và *ABM - Agent Based Modeling*.

![Hình 2: Các mô hình phổ biến trong ngành Supply Chain](img/simulation.png){fig-location = "center"}

<a href="https://www.supplychaindataanalytics.com/simulation-methods-for-scm-analysts/" 
style="text-decoration: underline; font-family: 'Times New Roman', Times, serif; font-style: italic; 
          position: fixed; bottom: 10px; right: 10px;"> (Nguồn: Simulation Methods for SCM Analysts) </a>
  
  -   *DES*: hướng tới mô hình hóa quy trình hoạt động của đối tượng, tựa như flowchart của process mà bạn có thể học trong quản trị vận hành. Ví dụ quá trình vận chuyển hàng từ khi có đơn bao gồm: KH đặt hàng -\> CS nhận đơn và thông báo -\> WH nhận thông tin đơn hàng và chuẩn bị hàng -\> WH giao hàng cho Shipper -\> Shipper vận chuyển đến tận nhà hoặc tới DC -\> KH nhận hàng từ shipper hoặc ra DC lấy.

Vậy dựa vào các đặc tính đó, *DES* có thể giúp bạn trả lời các câu hỏi như: Khi nào xe chở hàng? ETA và ETD cụ thể bao nhiêu ?, ...

![Hình 3: Mô hình DES trong dự báo nhu cầu cho Volvo](img/DESmodel.png){fig-align = "center"}

Như hình trên này là 1 ứng dụng *DES* vào việc dự báo nhu cầu khách hàng của Volvo trong nghiên cứu của [@jagathishvarjayakumar2020].

-   *ABM*: là mô hình dựa vào *behaviors - hành vi* của từng object và đưa chúng vào *enviroment* để tự tương tác và thu lại dữ liệu. Về lí thuyết,*ABM* sẽ chi tiết và đánh giá kĩ hơn *DES* nên đó cũng là lý do *ABM* được ưu tiên sử dụng trong thời gian gần đây nhưng điều này cũng đòi hỏi độ chính xác cao về dữ liệu đầu vào để tránh sự sai lệch do độ nhạy cảm cao của mô hình này. Còn trên thực tế, mô hình nào tốt hơn còn tùy vào trường hợp mà bạn đối mặt.

Một ví dụ thực tế của mô hình *ABM* là trong nghiên cứu [@hiroyasuinoue2023] về giả lập sự tác động của trận động đất [GEJE](https://en.wikipedia.org/wiki/2011_T%C5%8Dhoku_earthquake_and_tsunami) nổi tiếng từng cản quết Nhật Bản đến hoạt động sản xuất của công ty.

![Hình 4: Mô hình DES trong dự báo nhu cầu cho Volvo](img/ABMmodel.png){fig-align = "center"}

Mô hình trên đã giả lập sự mất mát về kinh tế (*economic loss*) gây bởi *supply chain disruption* theo thời gian tính từ lúc cơn động đất xảy ra. Đồng thời kết quả cũng cho thấy sự khác biệt so với nghiên cứu trước đây và lý do của sự chênh lệch này là việc mất điện kéo dài ảnh hưởng nặng đến hoạt động sản xuất và gây tổn thất cho ngành sản xuất Nhật Bản.

Như vậy, các bạn có thể thấy tính ứng dụng cao của *simulation* trong đời thực. Đối với các bài toán thực tế, tùy vào mức độ phức tạp, người dùng có thể ưu tiên *ABM* hay *DES* hoặc có thể phối hợp cả hai thuật toán này.

Vậy để việc *simulation* diễn ra tốt, mình sẽ cập nhập thêm các kiến thức sau đây.

### Lý thuyết hàng chờ:

Đầu tiên là *Queueing* hay còn gọi là hàng chờ là một chuỗi các đối tượng đang chờ đợi được phục vụ và hoàn thành yêu cầu của mình từ các *server* trong hệ thống. Nói một cách đơn giản là giống như bạn đi mua trà sữa và đang đứng chờ tới lượt mình để order, sau đó đợi trà sữa được làm xong và lấy trà sữa và trở về nhà. Với góc nhìn của khách hàng thì không ai muốn chờ đợi, còn góc nhìn của chủ cửa hàng thì không muốn hàng chờ phải dài (mặc dù như vậy đồng nghĩa với cửa hàng đang đông khách nhưng hàng chờ dài sẽ làm khó chịu khách hàng và dẫn đến mất khách).

![Hình 5: Ví dụ về queueing trong quản lí nhà kho](img/step.png)

Như vậy, mục tiêu là: đảm bảo hàng chờ ở mức **tối ưu nhất** để cân bằng giữa cấp độ phục vụ (Vd: đảm bảo khách hàng hài lòng) và chi phí. Lý do vì sao mình nói rằng là **tối ưu nhất** chứ không phải là giảm tới mức hàng chờ = 0 là vì hàng chờ luôn luôn tồn tại cho dù bạn có tốn bao nhiêu tài nguyên vào *server*. Nó giống như nghịch lý quy hoạch đô thị rằng: "Xây thêm đường sẽ không giảm kẹt xe" Và nghịch lý vận tải: Đường càng rộng thì càng kẹt xe" của [Matthew A. Turner](https://vivo.brown.edu/display/mturner1) và [Gilles Duranton](https://knowledge.wharton.upenn.edu/faculty/gilles-duranton/), nghĩa là bạn chỉ giảm khoảng cách của hàng kẹt xe chứ không hoàn toàn xóa bỏ tình trạng kẹt xe. Vì vậy, ví dụ nếu cửa hàng của bạn luôn tồn tại hàng chờ cỡ 5-6 người/hàng thì bạn chỉ cần thuê thêm 1 nhân viên đứng trực là hàng chờ giảm còn 2-3 người thay vì 5 nhân viên để hàng chờ giảm còn 0 bởi như vậy là tốn chi phí và không hiệu quả.

Vậy làm sao để xây dựng mô hình hàng chờ cho cửa hàng của bạn ? Đầu tiên ta cần xác định được lượng khách hàng trong một khoảng thời gian cụ thể (Vd: ngày, tuần, tháng,...) tuân theo phân bố gì, trong đó các phân bố như phân phối hàm mũ, phân phối Poisson được sử dụng phổ biến.

```{r}
#| warning: false
#| message: false
# Load necessary library
set.seed(123)  

# Define customer flow pattern for different times of the day
# Morning rush: 8:30 AM - 10:30 AM
# Midday: 10:30 AM - 2:00 PM
# Afternoon: 2:00 PM - 4:30 PM
# Evening rush: 4:30 PM - 7:00 PM
# Late evening: 7:00 PM - 10:00 PM

# Function to generate number of customers based on time of day
generate_customers <- function(time) {
  hour <- as.numeric(format(time, "%H"))
  
  if (hour >= 8 && hour < 10) {
    # Morning rush (8:30 AM - 10:30 AM)
    return(sample(20:35, 1))  # 20 to 35 customers
  } else if (hour >= 10 && hour < 14) {
    # Midday (10:30 AM - 2:00 PM)
    return(sample(20:32, 1))  # 20 to 32 customers
  } else if (hour >= 14 && hour < 16) {
    # Afternoon slump (2:00 PM - 4:30 PM)
    return(sample(10:18, 1))  # 10 to 18 customers
  } else if (hour >= 16 && hour < 19) {
    # Evening rush (4:30 PM - 7:00 PM)
    return(sample(30:50, 1))  # 30 to 50 customers
  } else if (hour >= 19 && hour <= 22) {
    # Late evening (7:00 PM - 10:00 PM)
    return(sample(12:18, 1))  # 12 to 18 customers
  }
}

time_intervals<-seq(from = as.POSIXct("2024-11-11 08:30:00",tz = "UTC"),

                    to = as.POSIXct("2024-11-11 22:00:00",tz = "UTC"),

                    by = "30 min")

# Apply the function to generate customer data for each time interval
customer_data <- sapply(time_intervals, generate_customers)

# Create a data frame with time and customer count
coffee_data <- data.frame(
  Time = as.numeric(time_intervals)*1000,
  Customers = customer_data
)

library(highcharter)
hchart(
  coffee_data,
  'line', 
  hcaes(y = Customers,
        x = Time)
  ) |> 
  hc_xAxis(title = FALSE,type = "datetime") |> 
  hc_yAxis(
    title = list(text = "Number of Customers")
  ) |> 
  hc_title(text = "No.Customer through a normal day in Coffee Shop")
```

Cách xác định là chia lượng người vào cửa hàng theo từng khoảng thời gian bằng nhau, như trên ví dụ đây là 30 phút trong khoảng thời gian hoạt động từ 8h00 AM đến 22h00 PM. Kết quả kiểm định được trình bày bên dưới với giá trị p \< 0.05 cho thấy số lượng khách hàng tuân theo phân bố poisson (Mặc dù dữ liệu này được mình tạo ngẫu nhiên bởi chatGPT 😮😮😮).

::: panel-tabset
```{r}
#| warning: false
#| message: false
# Table of observed frequencies
obs_freq <- table(coffee_data$Customers)
# Expected frequencies based on Poisson distribution
lambda <- mean(coffee_data$Customers)
exp_freq <- dpois(as.numeric(names(obs_freq)), lambda) * length(data)

# Perform Chi-squared test
result <- chisq.test(obs_freq, p = exp_freq, rescale.p = TRUE)

# Create a data frame for gt()
result_df <- data.frame(
  Metric = c("Chi-squared Statistic", 
             "Degrees of Freedom", "p-value"),
  Value = c(round(result$statistic,4), 
            result$parameter, 
            format(result$p.value, 
                   scientific = TRUE, 
                   digits = 2))
)

counts_df <- data.frame(
Category = as.numeric(names(obs_freq)),
Observed = as.vector(obs_freq),
`P-value` = round(exp_freq,4)
)
```

### Count table:

```{r}
#| warning: false
#| message: false
library(gt)
# Print Observed vs. Expected Counts Table
gt(counts_df) %>%
  tab_header(
    title = "Observed Counts vs P-value"
  ) %>%
  tab_spanner(
    label = "Counts",
    columns = c("Observed", "P.value")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )
```

### Chi-squared test:

```{r}
# Print Chi-squared Test Results Table
gt(result_df) %>%
  tab_header(
    title = "Chi-squared Test Results"
  ) %>%
  tab_spanner(
    label = "Test Statistics",
    columns = c("Value")
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = everything())
  )
```
:::

Tiếp theo là xác định năng suất của *server* trong 1 đơn vị thời gian. Giả sử nhân viên của bạn có năng suất là như nhau và phục vụ trung bình được 1 đơn hàng/10 phút hay 6 đơn hàng/giờ thì ta có $\mu$ = 6

Sau đó chúng ta sẽ bắt đầu quá trình giả lập và bạn có thể sử dụng 2 cách: (i) Phương pháp giải tích (giải phương trình toán học bằng tay) hoặc (ii) Phương pháp máy tính (giả lập bằng máy). Với **R** thì bạn hoàn toàn làm điều này dễ hơn.

```{r}
simulate_shop <- function() {
  labor_capacity <- 12 / 60  # Service rate per minute
  
  # Initialize the simulation
  sim <- simmer("CoffeeShop")
  
  # Define the customer process
  customer_process <- trajectory("customer") %>%
    seize("barista", 1) %>%
    timeout(function() rexp(1, rate = labor_capacity)) %>%
    release("barista", 1)
  
  # Add barista resource to the simulation
  sim <- sim %>%
    add_resource("barista", capacity = 2)
  
  # Schedule customer arrivals
  start_time <- as.numeric(time_intervals[1])  # Convert start time to POSIX numeric

  customer_id <- 1  # Initialize a counter for unique customer naming

  for (j in 1:length(customer_data)) {
    num_customers <- customer_data[j]
    interval_start <- as.numeric(time_intervals[j])  # Start of each 30-min interval in POSIX numeric

    for (k in 1:num_customers) {
      arrival_offset <- runif(1, 0, 1800)  # Random arrival within 30 mins
      arrival_time <- interval_start + arrival_offset  # Actual arrival time in POSIX numeric
      
      # Convert to relative time (in minutes) from start_time
      relative_arrival <- (arrival_time - start_time) / 60
      
      # Generate a unique name for each customer
      customer_name <- paste0("customer_", customer_id)
      customer_id <- customer_id + 1  # Increment the unique ID
      
      # Add customer generator with a unique name
      sim <- sim %>%
        add_generator(customer_name, 
                      customer_process, 
                      at(relative_arrival))
    }
  }
  
  # Run the simulation until 13.5 hours (8:30 AM to 10:00 PM)
  sim %>% run(until = 13.5 * 60)
  
  return(sim)
}

# Run the simulation and get the results
sim_result <- simulate_shop()

# Summarize the simulation output
arrival_data <- sim_result %>%
  get_mon_arrivals() 

# Convert to right format datetime:
arrival_data<-arrival_data |> 
  mutate()

  
library(highcharter)
arrivals_data |> 
  dplyr::select(c(start_time,
           end_time,
           activity_time)) |> 
  pivot_longer(cols = everything(),
               values_to = "time",
               names_to = "type") |> 
  hchart(
  'line', 
  hcaes(y = time,
        group = type)
  ) 
```

### Lý thuyết thống kê:

Đầu tiên, bạn cần lý thuyết thống kê về chuyên ngành của vấn đề mà bạn đang mong muốn mô hình hóa.

Về code, bạn cần lưu ý rằng *sample size - số lượng mẫu* ảnh hưởng lớn đến độ chính xác (*precision*) của đo lường ảnh hưởng của biến đó. Điều này cũng tương tự với độ lặp lại (*replication*) nghĩa là bạn lặp lại nhiều lần việc tính toán thì kết quả sẽ tốt hoặc chính xác hơn.

## Thực hành trong R:

### Thư viện:

TIếp theo là chúng ta sẽ lập thử *DES* trong **R** để giả lập quá trình duy chuyển của xe trong chuỗi cung ứng để ứng tính ETD và ETA.

Trong **R** có package `simmer` thuần về xây dựng mô hình *DES*, đòi hỏi 3 nhân tố là: (i) Hành vi hoạt động của đối tượng, (ii) Hàm tính thời gian cho hoạt động và (iii) Môi trường để chạy mô phỏng. Bạn có thể đọc bài viết này để biết thêm [simmerR](https://www.supplychaindataanalytics.com/simmer-in-r-for-discrete-event-simulation/).

#### Mô hình DES:

Các object cần có trong mô hình *DES* là:

-   **Entitiy**: là đối tượng chính cần quan sát, ví dụ: khách hàng.

-   **Attribute**: là *property* của *entity* (có thể hiểu là đặc tính, trạng thái,...).

-   **Resource**: là một nguồn lực mà *entity* được quyền dùng, ví dụ như nhân viên quán cafe cần hỗ trợ khách hàng khi có yêu cầu.

-   **Queue**: là một chuỗi để các *entity* tham gia vào, ví dụ như hàng xếp chờ lấy nước ở quán cafe.

-   **Event**: các sự kiện thay đổi trong hệ thống

```{r}
#| warning: false
#| message: false
#| output: false
library(simmer)
set.seed(1234)
# Define the trajectory for the vehicle's process
vehicle_traj <- trajectory("Vehicle Process") %>%
  # Step 1: Take Order
  log_("Move to pickup location") |> 
  timeout(function() rnorm(n=1, mean = 30, sd = 3)) %>% # Time to take an order
  
  # Step 2: Check if goods are available in the warehouse
  seize("warehouse", 1) %>%  
  timeout(function() rexp(1, 0.3)) %>%  # Time to check if goods are available
  release("warehouse", 1) %>%  # Release the warehouse resource after checking
  
  # Step 3: If goods are not available, wait for manufacturing (conditional)
  branch(
    option = function() ifelse(runif(1) < 0.5, 1, 2),  # 50% chance
    continue = c(TRUE, TRUE),
    trajectory() %>% 
      timeout(function() rexp(1, 0.8)),  # Manufacturing time if goods aren't available
    
    trajectory("WaitForStock")  # Second branch if goods are available
  ) %>%
  
  # Step 4: Loading the goods
  seize("truck", 1) %>%  # Seize one truck for loading
  timeout(function() rexp(1, 0.6)) %>%  # Loading time
  release("truck", 1) %>%  # Release truck after loading
  
  # Step 5: Transport to store
  timeout(function() rexp(1, 1)) %>%  # Transport time
  
  # Step 6: Unload the goods at the store
  seize("store", 1) %>%  # Seize one store resource for unloading
  timeout(function() rexp(1, 0.4)) %>%  # Unloading time
  release("store", 1)  # Release store resource

# Create an environment
env <- simmer("Vehicle Simulation") %>%
  add_resource("warehouse", capacity = 1) %>%  
  add_resource("truck", capacity = 1) %>%  
  add_resource("store", capacity = 1) %>%  
  
  # Add generator for vehicles arriving with an exponential inter-arrival time
  add_generator("vehicle", vehicle_traj, function() rexp(1, 0.1))  # Average of 10 vehicles per 100 time units

# Run the simulation for a longer duration
env %>% 
  run(until = 500)

start_time <- as.POSIXct("2024-11-09 08:30:00", 
                         format="%Y-%m-%d %H:%M:%S")

arrivals<-get_mon_arrivals(env, per_resource = T)
arrivals$datetime <- start_time + (arrivals$start_time * 60)  
arrivals$finish_datetime <- start_time + (arrivals$end_time * 60)  
```

Kết quả giả lập được thể hiện như sau: các đốm thể hiện thời điểm xe đi vào, chờ giải quyết yêu cầu và đi ra khỏi hàng chờ.

```{r}
#| include: false
#| warning: false
#| message: false
library(ggplot2)
library(gganimate)

vehicle_positions <- arrivals %>%
  mutate(
    # Add the position of the vehicle at each time step
    position = case_when(
      resource == "warehouse" ~ "Warehouse",
      resource == "truck" ~ "Truck",
      resource == "store" ~ "Store"
    ),
    time_seconds = as.numeric(difftime(datetime, 
                                       as.POSIXct("2024-11-09 08:30:00", format="%Y-%m-%d %H:%M:%S"), 
                                       units = "secs"))
  )

# Create an animated plot of vehicle movement over time
animation <- ggplot(vehicle_positions, aes(x = time_seconds, y = name, color = position)) +
  geom_point(aes(size = 3)) +
  geom_segment(aes(xend = time_seconds, yend = name, color = position), size = 1) +
  scale_color_manual(values = c("Warehouse" = "blue", "Truck" = "green", "Store" = "red")) +
  labs(title = "Vehicle Movement Over Time", x = "Time (seconds)", y = "Vehicle", color = "Position") +
  theme_minimal() +
  theme(legend.position = "top") +
  transition_reveal(time_seconds) + # Create time-based transition
  ease_aes('linear')

# Animate and save the plot as a GIF
# animate(animation, 
#        nframes = 100, 
#        fps = 10, 
#        width = 800, 
#        height = 600)

##Run it to save: 
# anim_save("DES.gif")
```

![Biểu đồ 1: Discrete Event Simulation](DES.gif)

Và chúng ta sẽ có final output gồm:

::: tabset
```{r}
#| warning: false
#| message: false
# Extract arrival data (time the vehicle spends in the system, i.e., its total time)
arrivals <- get_mon_arrivals(env)

# Extract resource utilization data (how long each resource was occupied)
resources <- get_mon_resources(env)
```

### Resources:

```{r}
#| warning: false
#| message: false
#| fig-cap: "Biểu đồ 2: Quy trình di chuyển giả lập của xe"
#| fig-cap-location: bottom
library(simmer.plot)
get_palette <- scales::brewer_pal(type = "qual", 
                                  palette = 1)
plot(vehicle_traj, fill = get_palette)
```

### Simulation:

```{r}
#| warning: false
#| message: false
#| fig-cap: "Biểu đồ 3: Thời gian giả lập của xe"
#| fig-cap-location: bottom
library(highcharter)
arrivals |> 
  dplyr::select(c(start_time,
           end_time,
           activity_time)) |> 
  pivot_longer(cols = everything(),
               values_to = "time",
               names_to = "type") |> 
  hchart(
  'line', 
  hcaes(y = time,
        group = type)
  ) 
```
:::

#### Mô hình ABM:

Khác với *DES*, *ABM* quan tâm nhiều hơn đến các đặc tính, thông tin của cá thể trong quần thể này. Do đó, chỉ với lượng thông tin về số xe, số nhà kho, số cửa hàng và quy trình hoạt động là chưa đủ để xây dựng mô hình *ABM*. Các dữ liệu khả thi có thể bao gồm: thông tin của đội xe: gồm bao nhiêu người hoặc gồm những ai; dữ liệu quá khứ về KPI, học vấn,...; thông tin về thời gian hoạt động của các cửa hàng; vị trí và thông tin về đường xá có cấm tải không;... *ABM* thường được ứng dụng vào việc xác định các *risks - rủi ro* trong quản lí chuỗi cung ứng

Trong R có thư viện `NetLogoR` hỗ trợ chúng ta tốt về mảng mô hình *ABM*. Về nền tảng, *ABM* cần các yếu tố cơ bản sau:

-   *Agents*: là cá thể hoạt động trong môi trường giả lập, có thể là con người, xe cộ, ...

-   *World*: Môi trường mà các *agent* "sống" bên trong.

-   *Set of rules*: Những luật lệ mà tất cả *agent* "sống "trong *world* phải tuân theo.

-   *Loop*: Là sự lặp đi lặp lại của các hoạt động, sự tương tác. Trong lập trình thì nó giống như vòng lặp *for if* quen thuộc còn trong đời thực như cuộc sống hằng ngày của bạn sẽ được lặp đi lặp lại ví dụ: sáng thì đi học, tối thì đi ngủ và có thể có các đột biến như: sáng nay được nghỉ học, tối thứ 7 thì thức cả đêm (Đây lại là hoạt động quen thuộc của tôi!!!).

Như vậy, bạn có thể hình dung mơ hồ về quần thể giả lập bao gồm: các chiếc xe, các *nodes* trong *supply chain* như: nhà kho, cửa hàng,...

```{r}
#| warning: false
#| message: false

library(ggplot2)
library(gganimate)
library(transformr)

set.seed(42)

# Initialize grid size (a 10x10 grid for this example)
grid_size <- 10

# Initial agent position
agent_position <- c(x = 5, y = 5)

# Number of time steps to simulate
num_steps <- 50

# Function to move the agent
move_agent <- function(position) {
  direction <- sample(c("up", "down", "left", "right"), 1)
  if (direction == "up" && position[2] < grid_size) {
    position[2] <- position[2] + 1
  } else if (direction == "down" && position[2] > 1) {
    position[2] <- position[2] - 1
  } else if (direction == "left" && position[1] > 1) {
    position[1] <- position[1] - 1
  } else if (direction == "right" && position[1] < grid_size) {
    position[1] <- position[1] + 1
  }
  return(position)
}

# Simulate the movement of the agent
trajectory <- matrix(NA, 
                     nrow = num_steps, 
                     ncol = 2)
trajectory[1, ] <- agent_position

for (step in 2:num_steps) {
  agent_position <- move_agent(agent_position)
  trajectory[step, ] <- agent_position
}

# Convert the trajectory matrix into a data frame for plotting
trajectory_df <- data.frame(
  step = 1:num_steps,
  x = trajectory[, 1],
  y = trajectory[, 2]
)

# Labels for pickup and dropoff
pickup_label <- trajectory_df[1, ]
dropoff_label <- trajectory_df[num_steps, ]

# Plotting the trajectory with gganimate and using transition_reveal
p <- ggplot(trajectory_df, aes(x = x, y = y, group = 1)) +
  geom_point(color = "blue", size = 3) + 
  geom_path(color = "blue", alpha = 0.5) + 
  labs(title = "Agent Movement on a 10x10 Grid", x = "X Position", y = "Y Position") +
  xlim(1, grid_size) + ylim(1, grid_size) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  transition_reveal(step) +  # Use transition_reveal to show the path over time
  ease_aes('linear') +
  # Add labels for the pickup (first point) and dropoff (last point)
  geom_text(data = pickup_label, aes(x = x, y = y, label = "Pickup"), vjust = -1, color = "red", size = 4) +
  geom_text(data = dropoff_label, aes(x = x, y = y, label = "Dropoff"), vjust = 1.5, color = "green", size = 4)

# Animate and render the plot
#animate(p, nframes = num_steps, fps = 10, end_pause = 20, rewind = TRUE)

##Run it to save: 
#anim_save("trajectory-animation.gif")
```

![Biểu đồ 4: Simple of Agent Based Modeling](trajectory-animation.gif)

Như trên đây là ví dụ về cách *simulation* cho 1 đối tượng di chuyển trong mặt phẳng 10x10.

```{r}
library(NetLogoR)
library(lubridate)
library(tidyverse)

# Define new time parameters for the extended simulation
n_vehicles <- 5              # Number of vehicles
grid_size <- 20              # Size of the grid
speed_limit <- 2             # Speed limit (cells per tick)
time_step <- 1               # Simulation time step (1 second per tick)
order_creation_time <- c(hm("8:30"), hm("9:00"))   # Time when orders are released (8:30 AM)
n_customer <- 5

n_ticks <- 48  # Increase simulation time

# Initialize the world, orders, and vehicle positions
world <- createWorld(minPxcor = -grid_size, 
                     maxPxcor = grid_size, 
                     minPycor = -grid_size, 
                     maxPycor = grid_size)

random_coords <- randomXYcor(world, n = n_vehicles)

# Define orders and vehicles
orders <- data.frame(
  order_id = 1:n_customer,
  pickup_x = sample(1:grid_size, n_customer, replace = TRUE),
  pickup_y = sample(1:grid_size, n_customer, replace = TRUE),
  dropoff_x = sample(1:grid_size, n_customer, replace = TRUE),
  dropoff_y = sample(1:grid_size, n_customer, replace = TRUE),
  order_time = as.POSIXct("08:30:00", format = "%H:%M:%S") + runif(n_customer, 0, 1800)
)

# Initialize vehicles' position and speed
vehicles_df <- data.frame(
  vehicle_id = 1:n_vehicles,
  xcor = random_coords[, 1],
  ycor = random_coords[, 2],
  speed = rep(1.5, n_vehicles)
)

# Initialize tracking variables
vehicle_order <- rep(NA, n_vehicles)
order_status <- rep(NA, n_customer)
vehicle_state <- rep("waiting", n_vehicles)  # New: track state of each vehicle
ETA <- rep(NA, n_vehicles)  # Initialize ETA for each vehicle
ETD <- rep(NA, n_vehicles)  # Initialize ETD for each vehicle
completed_time <- rep(NA, n_vehicles)  # Initialize completed time for each vehicle

# Function to simulate vehicle movement (with speed and direction)
move_vehicle <- function(vehicle, destination) {
  angle_to_dest <- atan2(destination[2] - vehicle$ycor, destination[1] - vehicle$xcor)
  distance_to_dest <- sqrt((vehicle$xcor - destination[1])^2 + (vehicle$ycor - destination[2])^2)
  
  # Move the vehicle
  distance_to_move <- min(distance_to_dest, vehicle$speed)
  vehicle$xcor <- vehicle$xcor + distance_to_move * cos(angle_to_dest)
  vehicle$ycor <- vehicle$ycor + distance_to_move * sin(angle_to_dest)
  
  return(vehicle)
}

# Function to calculate the distance between two points
calculate_distance <- function(x1, y1, x2, y2) {
  return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
}

# Function to check if a vehicle is at a location (pickup/dropoff)
at_location <- function(vehicle, location) {
  return(abs(vehicle$xcor - location[1]) < 0.5 && abs(vehicle$ycor - location[2]) < 0.5)
}

simulation_start_time <- as.POSIXct("08:00:00", format = "%H:%M:%S")

# Initialize an empty data frame to store the results
final_results <- data.frame()

# Simulation loop with updated conditions
for (tick in 1:n_ticks) {
  current_time <- simulation_start_time + seconds(tick) * 3600 / 2
  
  # Process each vehicle
  for (i in 1:n_vehicles) {
    if (is.na(vehicle_order[i])) {  
      # Assign a vehicle to an order if it's free and an order is available
      for (j in 1:nrow(orders)) {
        if (is.na(order_status[j]) && current_time >= orders$order_time[j]) {
          order_status[j] <- "in_progress"  # Change order status to in_progress
          vehicle_order[i] <- j
          vehicle_state[i] <- "received"  # Vehicle starts receiving the order
          
          # Calculate ETA (time to reach pickup location)
          distance_to_pickup <- calculate_distance(vehicles_df$xcor[i], vehicles_df$ycor[i], 
                                                   orders$pickup_x[j], orders$pickup_y[j])
          transportation_time <- distance_to_pickup / vehicles_df$speed[i]  # Time to pickup location in hours
          ETA[i] <- orders$order_time[j] + seconds(transportation_time) * 3600  # ETA in POSIXct object
          
          break
        }
      }
    }
    
    # Get current order info for the assigned order
    assigned_order <- vehicle_order[i]
    
    if (!is.na(assigned_order)) {
      pickup_loc <- c(orders$pickup_x[assigned_order], orders$pickup_y[assigned_order])
      dropoff_loc <- c(orders$dropoff_x[assigned_order], orders$dropoff_y[assigned_order])
      
      # Handle movement to pickup location
      if (vehicle_state[i] == "received") {
        if (!at_location(vehicles_df[i, ], pickup_loc)) {
          vehicles_df[i, ] <- move_vehicle(vehicles_df[i, ], pickup_loc)
        } else {
          cat("Vehicle", i, "reached pickup location\n")
          vehicle_state[i] <- "loading"  # Transition to loading state
        }
      }
      
      # Handle loading phase (vehicle at pickup location)
      if (vehicle_state[i] == "loading") {
        if (at_location(vehicles_df[i, ], pickup_loc)) {
          cat("Vehicle", i, "loading goods at pickup location\n")
          vehicle_state[i] <- "enroute"  # Transition to enroute state
          
          # Calculate the transportation time to dropoff location
          distance_to_dropoff <- calculate_distance(vehicles_df$xcor[i], vehicles_df$ycor[i], 
                                                   dropoff_loc[1], dropoff_loc[2])
          transportation_time <- distance_to_dropoff / vehicles_df$speed[i]
          ETD[i] <- ETA[i] + seconds(transportation_time*3600 + 30*60)   # Add transportation time to dropoff location
        }
      }
      
      # Handle enroute phase (vehicle moving to dropoff location)
      if (vehicle_state[i] == "enroute") {
        if (!at_location(vehicles_df[i, ], dropoff_loc)) {
          vehicles_df[i, ] <- move_vehicle(vehicles_df[i, ], dropoff_loc)
        } else {
          cat("Vehicle", i, "unloading goods at dropoff location\n")
          vehicle_state[i] <- "completed"  # Transition to completed state
          
          # Set completed time (ETD + unloading time 40 minutes)
          completed_time[i] <- ETD[i] + seconds(40*60)  # Unloading time 40 minutes
        }
      }
      
      # Handle unloading phase (vehicle at dropoff location)
      if (vehicle_state[i] == "completed") {
        order_status[assigned_order] <- "completed"
      }
    }
  }
  
  # Store the current simulation results with time info
  tick_result <- data.frame(
    time = current_time,
    vehicle_id = 1:n_vehicles,
    xcor = vehicles_df$xcor,
    ycor = vehicles_df$ycor,
    speed = vehicles_df$speed,
    order_status = order_status,
    vehicle_order = vehicle_order,
    vehicle_state = vehicle_state,  # Track vehicle state
    ETA = ETA,  # Estimated Time of Arrival
    ETD = ETD,  # Estimated Time of Departure
    completed_time = completed_time  # Completed Time
  ) 
  
  # Append the current results to the final_results data frame
  final_results <- bind_rows(final_results, tick_result)
}

final_results <- final_results|> 
  filter(!is.na(order_status)) |> 
    mutate(ETA = as.POSIXct(ETA, origin = "1970-01-01", tz = "UTC"),
           ETD = as.POSIXct(ETD, origin = "1970-01-01", tz = "UTC"),
           completed_time = as.POSIXct(completed_time, origin = "1970-01-01", tz = "UTC"))

final<-final_results[!duplicated(final_results[, setdiff(names(final_results), "time")]), ]
```

```{r}
#| warning: false
#| message: false
#| include: false
library(gganimate)
# Plotting the trajectory with gganimate and using transition_reveal
# Add labels for pickup and dropoff locations
pickup_data <- data.frame(
  vehicle_id = 1:5,
  x = orders$pickup_x,
  y = orders$pickup_y,
  label = paste("Pickup", 1:n_customer)
)

dropoff_data <- data.frame(
  vehicle_id = 1:5,
  x = orders$dropoff_x,
  y = orders$dropoff_y,
  label = paste("Dropoff", 1:n_customer)
)

# Define the custom color palette
vehicle_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a")


# Create the plot
p <- ggplot(final, 
            aes(x = xcor, 
                y = ycor, 
                color = factor(vehicle_id),
                group = vehicle_id)) +
  geom_point(size = 3) +  # Vehicle positions
  geom_path(aes(x = xcor, 
                y = ycor), 
            color = "grey", alpha = 0.5) + 
  scale_color_discrete(name = 'Vehicle ID') +
  xlim(0, grid_size) + 
  ylim(0, grid_size) +
  theme_minimal() +
  transition_reveal(time) +
  ease_aes('linear')+
  # Use the same color mapping for the pickup locations
  geom_text(data = pickup_data, 
            aes(x = x, y = y, label = label, color = factor(vehicle_id)), 
            vjust = -1, size = 4) +
  # Use the same color mapping for the dropoff locations
  geom_text(data = dropoff_data, 
            aes(x = x, y = y, label = label, color = factor(vehicle_id)), 
            vjust = 1.5, size = 4)+
  theme(legend.position="bottom") +
  labs(title="The simulation of ABM for Vehicles", 
       subtitle="", 
       caption="\n\nAuthor: Loccx78  \nSource: Rstudio  \n", tag="{current_frame}") +
  theme(legend.position=c(.15,-.09),
        legend.title=element_text(hjust=.5),
        plot.title=element_text(size=rel(1.5), family="sans",
                                face="bold"),
        plot.subtitle=element_text(color="#5e5855"),
        plot.caption=element_text(color="#867e7a"),
        plot.tag=element_text(hjust=0.5, color="#5e5855"),
        plot.tag.position=c(0.5, 0.16))


# animated_map <- animate(p, nframes = n_ticks, duration = 10, fps = 10, rewind = TRUE)

# anim_save("ABM.gif")
```

![Biểu đồ 5: Agent Based Modeling](ABM.gif)

Như vậy chúng ta đã kết thúc bài học ở đây.

Nếu bạn có câu hỏi hay thắc mắc nào, đừng ngần ngại liên hệ với mình qua Gmail. Bên cạnh đó, nếu bạn muốn xem lại các bài viết trước đây của mình, hãy nhấn vào hai nút dưới đây để truy cập trang **Rpubs** hoặc mã nguồn trên **Github**. Rất vui được đồng hành cùng bạn, hẹn gặp lại! 😄😄😄

```{=html}
<!DOCTYPE html>
  <html lang="en">
  <head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Contact Me</title>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css">
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/simple-icons@v6.0.0/svgs/rstudio.svg">
  <style>
  body { font-family: Arial, sans-serif; background-color: #f9f9f9; }
      .container { max-width: 400px; margin: auto; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1); }
    label { display: block; margin: 10px 0 5px; }
    input[type="email"] { width: 100%; padding: 10px; margin-bottom: 15px; border: 1px solid #ccc; border-radius: 4px; }
      .github-button, .rpubs-button { margin-top: 20px; text-align: center; }
      .github-button button, .rpubs-button button { background-color: #333; color: white; border: none; padding: 10px; cursor: pointer; border-radius: 4px; width: 100%; }
          .github-button button:hover, .rpubs-button button:hover { background-color: #555; }
              .rpubs-button button { background-color: #75AADB; }
                  .rpubs-button button:hover { background-color: #5A9BC2; }
                      .rpubs-icon { margin-right: 5px; width: 20px; vertical-align: middle; filter: brightness(0) invert(1); }
                    .error-message { color: red; font-size: 0.9em; margin-top: 5px; }
                    </style>
                      </head>
                      <body>
                      <div class="container">
                      <h2>Contact Me</h2>
                      <form id="emailForm">
                      <label for="email">Your Email:</label>
                      <input type="email" id="email" name="email" required aria-label="Email Address">
                      <div class="error-message" id="error-message" style="display: none;">Please enter a valid email address.</div>
                      <button type="submit">Send Email</button>
                      </form>
                      <div class="github-button">
                      <button>
                      <a href="https://github.com/Loccx78vn/Material_Requirement_Planning" target="_blank" style="color: white; text-decoration: none;">
                      <i class="fab fa-github"></i> View Code on GitHub
                    </a>
                      </button>
                      </div>
                      <div class="rpubs-button">
                      <button>
                      <a href="https://rpubs.com/loccx" target="_blank" style="color: white; text-decoration: none;">
                      <img src="https://cdn.jsdelivr.net/npm/simple-icons@v6.0.0/icons/rstudio.svg" alt="RStudio icon" class="rpubs-icon"> Visit my RPubs
                    </a>
                      </button>
                      </div>
                      </div>
                      
                      <script>
                      document.getElementById('emailForm').addEventListener('submit', function(event) {
                        event.preventDefault(); // Prevent default form submission
                        const emailInput = document.getElementById('email');
                        const email = emailInput.value;
                        const errorMessage = document.getElementById('error-message');
                        
                        // Simple email validation regex
                        const emailPattern = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
                        
                        if (emailPattern.test(email)) {
                          errorMessage.style.display = 'none'; // Hide error message
                          const yourEmail = 'loccaoxuan103@gmail.com'; // Your email
                          const gmailLink = `https://mail.google.com/mail/?view=cm&fs=1&to=${yourEmail}&su=Help%20Request%20from%20${encodeURIComponent(email)}`;
                          window.open(gmailLink, '_blank'); // Open in new tab
                        } else {
                          errorMessage.style.display = 'block'; // Show error message
                        }
                      });
                    </script>
                      </body>
                      </html>
                      ```
                    