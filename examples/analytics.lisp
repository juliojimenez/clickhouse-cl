;;;; analytics.lisp
;;;; 
;;;; Real-world analytics examples using clickhouse-cl
;;;; Demonstrates common analytics patterns, aggregations, and insights

(format t "~%=== ClickHouse Analytics Examples ===~%")

;; Load library
(unless (find-package :ch)
  (load (merge-pathnames "ch.lisp" (or *load-pathname* *default-pathname-defaults*))))

(defparameter *db* (ch:make-database :host "localhost"))

;; Test connection
(handler-case
    (ch:ping *db*)
  (error (e)
    (format t "Connection failed: ~A~%" e)
    (error "ClickHouse connection required for analytics example")))

(format t "Connected to ClickHouse!~%")

;; Setup analytics environment
(format t "~%=== SETTING UP ANALYTICS ENVIRONMENT ===~%")

(handler-case
    (ch:query *db* "CREATE DATABASE IF NOT EXISTS analytics_demo")
  (error (e) 
    (format t "Database creation note: ~A~%" e)))

(setf *db* (ch:make-database :host "localhost" :database "analytics_demo"))

;; Create analytics tables
(format t "Creating analytics tables...~%")

;; User events table
(ch:query *db* "DROP TABLE IF EXISTS user_events")
(ch:query *db* "CREATE TABLE user_events (
    event_id UInt64,
    user_id UInt32,
    session_id String,
    event_timestamp DateTime,
    event_type Enum('page_view' = 1, 'click' = 2, 'purchase' = 3, 'signup' = 4, 'logout' = 5),
    page_url String,
    referrer String,
    user_agent String,
    country String,
    city String,
    device_type Enum('desktop' = 1, 'mobile' = 2, 'tablet' = 3),
    revenue Decimal(10,2) DEFAULT 0.00
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(event_timestamp)
ORDER BY (user_id, event_timestamp)
SETTINGS index_granularity = 8192")

;; Products table  
(ch:query *db* "DROP TABLE IF EXISTS products")
(ch:query *db* "CREATE TABLE products (
    product_id UInt32,
    product_name String,
    category String,
    price Decimal(10,2),
    cost Decimal(10,2)
) ENGINE = MergeTree()
ORDER BY product_id")

;; Sales table
(ch:query *db* "DROP TABLE IF EXISTS sales")
(ch:query *db* "CREATE TABLE sales (
    sale_id UInt64,
    user_id UInt32,
    product_id UInt32,
    sale_timestamp DateTime,
    quantity UInt32,
    unit_price Decimal(10,2),
    total_amount Decimal(10,2),
    discount_amount Decimal(10,2) DEFAULT 0.00,
    payment_method Enum('credit_card' = 1, 'paypal' = 2, 'bank_transfer' = 3, 'crypto' = 4)
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(sale_timestamp)
ORDER BY (user_id, sale_timestamp)")

(format t "Analytics tables created!~%")

;; Generate sample data
(format t "~%=== GENERATING SAMPLE DATA ===~%")

;; Insert sample products
(format t "Inserting sample products...~%")
(let ((products-data "INSERT INTO products VALUES 
    (1, 'Laptop Pro', 'Electronics', 1299.99, 800.00),
    (2, 'Wireless Headphones', 'Electronics', 299.99, 150.00),
    (3, 'Smartphone X', 'Electronics', 899.99, 400.00),
    (4, 'Coffee Maker', 'Home & Kitchen', 159.99, 80.00),
    (5, 'Running Shoes', 'Sports', 129.99, 60.00),
    (6, 'Backpack', 'Travel', 89.99, 35.00),
    (7, 'Tablet', 'Electronics', 399.99, 200.00),
    (8, 'Water Bottle', 'Sports', 24.99, 8.00),
    (9, 'Desk Chair', 'Furniture', 199.99, 100.00),
    (10, 'Book Set', 'Books', 49.99, 20.00)"))
  (ch:query *db* products-data))

;; Generate user events (simplified for demo)
(format t "Generating user events...~%")
(let ((events '()))
  (dotimes (i 5000)
    (let* ((user-id (+ 1 (random 1000)))
           (event-type (nth (random 5) '("'page_view'" "'click'" "'purchase'" "'signup'" "'logout'")))
           (timestamp (format nil "'2024-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D'"
                        (+ 1 (random 12))
                        (+ 1 (random 28))
                        (random 24)
                        (random 60)
                        (random 60)))
           (country (nth (random 5) '("'US'" "'UK'" "'DE'" "'FR'" "'CA'")))
           (device (nth (random 3) '("'desktop'" "'mobile'" "'tablet'")))
           (revenue (if (string= event-type "'purchase'")
                        (* (+ 10 (random 500)) 1.0)
                        0.0)))
      (push (format nil "(~A, ~A, 'session_~A', ~A, ~A, '/page/~A', 'google.com', 'browser', ~A, 'City~A', ~A, ~A)"
              i user-id (random 500) timestamp event-type (random 20) country (random 100) device revenue)
            events)))

  ;; Insert in batches
  (let ((batch-size 1000))
    (loop for batch-start from 0 below (length events) by batch-size
          for batch-end = (min (+ batch-start batch-size) (length events))
          for batch = (subseq events batch-start batch-end)
          do (ch:query *db* (format nil "INSERT INTO user_events VALUES ~{~A~^,~}" batch)))))

;; Generate sample sales
(format t "Generating sample sales...~%")
(let ((sales '()))
  (dotimes (i 2000)
    (let* ((user-id (+ 1 (random 1000)))
           (product-id (+ 1 (random 10)))
           (quantity (+ 1 (random 5)))
           (unit-price (* (+ 20 (random 100)) 1.0))
           (total (* quantity unit-price))
           (discount (* total (random 0.2)))
           (payment (nth (random 4) '("'credit_card'" "'paypal'" "'bank_transfer'" "'crypto'")))
           (timestamp (format nil "'2024-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D'"
                        (+ 1 (random 12))
                        (+ 1 (random 28))
                        (random 24)
                        (random 60)
                        (random 60))))
      (push (format nil "(~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A)"
              i user-id product-id timestamp quantity unit-price total discount payment)
            sales)))

  ;; Insert sales in batches
  (let ((batch-size 500))
    (loop for batch-start from 0 below (length sales) by batch-size
          for batch-end = (min (+ batch-start batch-size) (length sales))
          for batch = (subseq sales batch-start batch-end)
          do (ch:query *db* (format nil "INSERT INTO sales VALUES ~{~A~^,~}" batch)))))

(format t "Sample data generation complete!~%")

;; Verify data
(let ((events-count (ch:query *db* "SELECT count() FROM user_events FORMAT JSON"))
      (sales-count (ch:query *db* "SELECT count() FROM sales FORMAT JSON")))
  (when (listp events-count)
        (format t "User events: ~A~%"
          (ch:jget (first (ch:jget events-count "data")) "count()")))
  (when (listp sales-count)
        (format t "Sales records: ~A~%"
          (ch:jget (first (ch:jget sales-count "data")) "count()"))))

(format t "~%=== BASIC ANALYTICS QUERIES ===~%")

;; 1. Daily Active Users
(format t "~%1. DAILY ACTIVE USERS:~%")
(let ((dau (ch:query *db* "SELECT 
                             toDate(event_timestamp) as date,
                             count(DISTINCT user_id) as daily_active_users
                           FROM user_events
                           WHERE event_timestamp >= today() - INTERVAL 7 DAY
                           GROUP BY date
                           ORDER BY date DESC
                           LIMIT 7
                           FORMAT JSON")))
  (when (listp dau)
        (format t "Daily Active Users (last 7 days):~%")
        (dolist (row (ch:jget dau "data"))
          (format t "  ~A: ~A users~%"
            (ch:jget row "date")
            (ch:jget row "daily_active_users")))))

;; 2. Top Pages by Traffic
(format t "~%2. TOP PAGES BY TRAFFIC:~%")
(let ((top-pages (ch:query *db* "SELECT 
                                   page_url,
                                   count() as page_views,
                                   count(DISTINCT user_id) as unique_visitors
                                 FROM user_events
                                 WHERE event_type = 'page_view'
                                 GROUP BY page_url
                                 ORDER BY page_views DESC
                                 LIMIT 10
                                 FORMAT JSON")))
  (when (listp top-pages)
        (format t "Top pages by traffic:~%")
        (dolist (row (ch:jget top-pages "data"))
          (format t "  ~A: ~A views (~A unique)~%"
            (ch:jget row "page_url")
            (ch:jget row "page_views")
            (ch:jget row "unique_visitors")))))

;; 3. Revenue Analysis
(format t "~%3. REVENUE ANALYSIS:~%")
(let ((revenue (ch:query *db* "SELECT 
                                 toYYYYMM(sale_timestamp) as month,
                                 sum(total_amount) as gross_revenue,
                                 sum(discount_amount) as total_discounts,
                                 sum(total_amount - discount_amount) as net_revenue,
                                 count() as total_orders,
                                 avg(total_amount - discount_amount) as avg_order_value
                               FROM sales
                               GROUP BY month
                               ORDER BY month DESC
                               LIMIT 12
                               FORMAT JSON")))
  (when (listp revenue)
        (format t "Monthly revenue analysis:~%")
        (dolist (row (ch:jget revenue "data"))
          (format t "  ~A: $~,2F net revenue (~A orders, $~,2F AOV)~%"
            (ch:jget row "month")
            (ch:jget row "net_revenue")
            (ch:jget row "total_orders")
            (ch:jget row "avg_order_value")))))

;; 4. Product Performance
(format t "~%4. PRODUCT PERFORMANCE:~%")
(let ((products (ch:query *db* "SELECT 
                                  p.product_name,
                                  p.category,
                                  sum(s.quantity) as units_sold,
                                  sum(s.total_amount) as gross_sales,
                                  sum(s.total_amount - s.discount_amount) as net_sales,
                                  avg(s.unit_price) as avg_selling_price,
                                  sum(s.quantity * p.cost) as total_cost,
                                  sum(s.total_amount - s.discount_amount) - sum(s.quantity * p.cost) as profit
                                FROM sales s
                                JOIN products p ON s.product_id = p.product_id
                                GROUP BY p.product_name, p.category
                                ORDER BY net_sales DESC
                                LIMIT 10
                                FORMAT JSON")))
  (when (listp products)
        (format t "Product performance:~%")
        (dolist (row (ch:jget products "data"))
          (format t "  ~A (~A): ~A units, $~,2F net sales, $~,2F profit~%"
            (ch:jget row "product_name")
            (ch:jget row "category")
            (ch:jget row "units_sold")
            (ch:jget row "net_sales")
            (ch:jget row "profit")))))

(format t "~%=== ADVANCED ANALYTICS ===~%")

;; 5. Funnel Analysis
(format t "~%5. CONVERSION FUNNEL ANALYSIS:~%")
(let ((funnel (ch:query *db* "WITH funnel_events AS (
                                SELECT 
                                  user_id,
                                  countIf(event_type = 'page_view') > 0 as viewed_page,
                                  countIf(event_type = 'click') > 0 as clicked,
                                  countIf(event_type = 'purchase') > 0 as purchased
                                FROM user_events
                                WHERE event_timestamp >= today() - INTERVAL 30 DAY
                                GROUP BY user_id
                              )
                              SELECT 
                                countIf(viewed_page) as page_views,
                                countIf(clicked) as clicks, 
                                countIf(purchased) as purchases,
                                round(countIf(clicked) / countIf(viewed_page) * 100, 2) as click_rate,
                                round(countIf(purchased) / countIf(clicked) * 100, 2) as conversion_rate
                              FROM funnel_events
                              FORMAT JSON")))
  (when (listp funnel)
        (let ((data (first (ch:jget funnel "data"))))
          (format t "Conversion funnel (last 30 days):~%")
          (format t "  Page views: ~A users~%" (ch:jget data "page_views"))
          (format t "  Clicks: ~A users (~A%% click rate)~%"
            (ch:jget data "clicks")
            (ch:jget data "click_rate"))
          (format t "  Purchases: ~A users (~A%% conversion rate)~%"
            (ch:jget data "purchases")
            (ch:jget data "conversion_rate")))))

;; 6. Cohort Analysis (Simplified)
(format t "~%6. USER COHORT ANALYSIS:~%")
(let ((cohorts (ch:query *db* "WITH user_cohorts AS (
                                 SELECT 
                                   user_id,
                                   toYYYYMM(min(event_timestamp)) as cohort_month
                                 FROM user_events
                                 GROUP BY user_id
                               )
                               SELECT 
                                 cohort_month,
                                 count(DISTINCT user_id) as cohort_size,
                                 countIf(user_id IN (
                                   SELECT DISTINCT user_id 
                                   FROM user_events 
                                   WHERE event_type = 'purchase'
                                 )) as users_with_purchases
                               FROM user_cohorts
                               WHERE cohort_month IS NOT NULL
                               GROUP BY cohort_month
                               ORDER BY cohort_month DESC
                               LIMIT 6
                               FORMAT JSON")))
  (when (listp cohorts)
    (format t "User cohorts by first visit month:~%")
    (dolist (row (ch:jget cohorts "data"))
      (format t "  ~A: ~A users, ~A made purchases~%" 
              (ch:jget row "cohort_month")
              (ch:jget row "cohort_size")
              (ch:jget row "users_with_purchases")))))

;; 7. Geographical Analysis
(format t "~%7. GEOGRAPHICAL ANALYSIS:~%")
(let ((geo (ch:query *db* "SELECT 
                             country,
                             count(DISTINCT user_id) as unique_users,
                             count() as total_events,
                             sum(revenue) as total_revenue,
                             avg(revenue) as avg_revenue_per_event
                           FROM user_events
                           WHERE country != ''
                           GROUP BY country
                           ORDER BY unique_users DESC
                           FORMAT JSON")))
  (when (listp geo)
        (format t "Performance by country:~%")
        (dolist (row (ch:jget geo "data"))
          (format t "  ~A: ~A users, ~A events, $~,2F revenue~%"
            (ch:jget row "country")
            (ch:jget row "unique_users")
            (ch:jget row "total_events")
            (ch:jget row "total_revenue")))))

;; 8. Device and Behavior Analysis
(format t "~%8. DEVICE AND BEHAVIOR ANALYSIS:~%")
(let ((device-behavior (ch:query *db* "WITH user_device_stats AS (
                                         SELECT 
                                           device_type,
                                           user_id,
                                           countIf(event_type = 'page_view') as page_views,
                                           countIf(event_type = 'purchase') as purchases,
                                           sum(revenue) as user_revenue
                                         FROM user_events
                                         GROUP BY device_type, user_id
                                       )
                                       SELECT 
                                         device_type,
                                         count(DISTINCT user_id) as unique_users,
                                         avg(page_views) as avg_page_views,
                                         sum(purchases) / sum(page_views) * 100 as conversion_rate,
                                         avg(user_revenue) as avg_revenue_per_user
                                       FROM user_device_stats
                                       WHERE page_views > 0  -- Avoid division by zero
                                       GROUP BY device_type
                                       ORDER BY unique_users DESC
                                       FORMAT JSON")))
  (when (listp device-behavior)
    (format t "User behavior by device type:~%")
    (dolist (row (ch:jget device-behavior "data"))
      (format t "  ~A: ~A users, ~,1F avg page views, ~,2F%% conversion~%" 
              (ch:jget row "device_type")
              (ch:jget row "unique_users")
              (ch:jget row "avg_page_views")
              (or (ch:jget row "conversion_rate") 0.0)))))

;; 9. Time-based Patterns
(format t "~%9. TIME-BASED PATTERNS:~%")
(let ((hourly (ch:query *db* "SELECT 
                                toHour(event_timestamp) as hour,
                                count() as events,
                                count(DISTINCT user_id) as unique_users,
                                avg(revenue) as avg_revenue
                              FROM user_events
                              GROUP BY hour
                              ORDER BY hour
                              FORMAT JSON")))
  (when (listp hourly)
        (format t "Activity patterns by hour of day:~%")
        (dolist (row (ch:jget hourly "data"))
          (format t "  ~2,'0D:00: ~A events, ~A users, $~,2F avg revenue~%"
            (ch:jget row "hour")
            (ch:jget row "events")
            (ch:jget row "unique_users")
            (ch:jget row "avg_revenue")))))

;; 10. Advanced Metrics
(format t "~%=== ADVANCED BUSINESS METRICS ===~%")

;; Customer Lifetime Value (simplified)
(handler-case
    (progn
      (format t "Creating sales data for CLV analysis...~%")
      (ch:query *db* "DROP TABLE IF EXISTS user_purchases")
      (ch:query *db* "CREATE TABLE user_purchases (
        user_id UInt32,
        purchase_date Date,
        amount Decimal(10,2)
      ) ENGINE = MergeTree()
      ORDER BY user_id")
      
      ;; Generate some sample purchase data based on user_events
      (ch:query *db* "INSERT INTO user_purchases
                      SELECT 
                        user_id,
                        toDate(event_timestamp) as purchase_date,
                        revenue as amount
                      FROM user_events 
                      WHERE event_type = 'purchase' AND revenue > 0")
      
      ;; Now calculate CLV from the purchases
      (let ((clv (ch:query *db* "SELECT 
                                   quantile(0.90)(user_total_spend) as clv_p90,
                                   quantile(0.75)(user_total_spend) as clv_p75,
                                   quantile(0.50)(user_total_spend) as clv_median,
                                   avg(user_total_spend) as clv_avg,
                                   count() as total_customers
                                 FROM (
                                   SELECT 
                                     user_id,
                                     sum(amount) as user_total_spend
                                   FROM user_purchases
                                   GROUP BY user_id
                                   HAVING user_total_spend > 0
                                 )
                                 FORMAT JSON")))
        (when (listp clv)
          (let ((data (first (ch:jget clv "data"))))
            (format t "Customer Lifetime Value distribution:~%")
            (format t "  Customers analyzed: ~A~%" (ch:jget data "total_customers"))
            (format t "  90th percentile: $~,2F~%" (ch:jget data "clv_p90"))
            (format t "  75th percentile: $~,2F~%" (ch:jget data "clv_p75"))
            (format t "  Median: $~,2F~%" (ch:jget data "clv_median"))
            (format t "  Average: $~,2F~%" (ch:jget data "clv_avg"))))))
  (error (e)
    (format t "Could not create sales table, using simplified CLV from events: ~A~%" e)
    ;; Fallback to simple revenue analysis from user_events
    (let ((clv (ch:query *db* "SELECT 
                                 quantile(0.90)(user_total_revenue) as clv_p90,
                                 quantile(0.75)(user_total_revenue) as clv_p75,
                                 quantile(0.50)(user_total_revenue) as clv_median,
                                 avg(user_total_revenue) as clv_avg,
                                 count() as total_users
                               FROM (
                                 SELECT 
                                   user_id,
                                   sum(revenue) as user_total_revenue
                                 FROM user_events
                                 WHERE revenue > 0
                                 GROUP BY user_id
                               )
                               FORMAT JSON")))
      (when (listp clv)
        (let ((data (first (ch:jget clv "data"))))
          (format t "Simplified CLV from event revenue:~%")
          (format t "  Revenue users: ~A~%" (ch:jget data "total_users"))
          (format t "  90th percentile: $~,2F~%" (ch:jget data "clv_p90"))
          (format t "  75th percentile: $~,2F~%" (ch:jget data "clv_p75"))
          (format t "  Median: $~,2F~%" (ch:jget data "clv_median"))
          (format t "  Average: $~,2F~%" (ch:jget data "clv_avg")))))))

;; Retention Analysis
(format t "~%11. USER RETENTION:~%")
(let ((retention (ch:query *db* "WITH user_activity AS (
                                   SELECT 
                                     user_id,
                                     toYYYYMM(event_timestamp) as activity_month
                                   FROM user_events
                                   GROUP BY user_id, activity_month
                                 )
                                 SELECT 
                                   activity_month,
                                   count(DISTINCT user_id) as active_users,
                                   count(DISTINCT CASE WHEN activity_month = toYYYYMM(today()) THEN user_id END) as current_month_active
                                 FROM user_activity
                                 GROUP BY activity_month
                                 ORDER BY activity_month DESC
                                 LIMIT 6
                                 FORMAT JSON")))
  (when (listp retention)
        (format t "Monthly user retention:~%")
        (dolist (row (ch:jget retention "data"))
          (format t "  ~A: ~A active users~%"
            (ch:jget row "activity_month")
            (ch:jget row "active_users")))))

;; Analytics helper functions
(format t "~%=== ANALYTICS HELPER FUNCTIONS ===~%")

(defun get-kpi-dashboard (db &optional (days 30))
  "Get key performance indicators for the last N days"
  (format t "~%KPI Dashboard (last ~A days):~%" days)

  ;; Total users and sessions
  (let ((user-metrics (ch:query db (format nil "SELECT 
                                               count(DISTINCT user_id) as total_users,
                                               count(DISTINCT session_id) as total_sessions,
                                               count() as total_events
                                             FROM user_events
                                             WHERE event_timestamp >= today() - INTERVAL ~A DAY
                                             FORMAT JSON" days))))
    (when (listp user-metrics)
          (let ((data (first (ch:jget user-metrics "data"))))
            (format t "  Total Users: ~A~%" (ch:jget data "total_users"))
            (format t "  Total Sessions: ~A~%" (ch:jget data "total_sessions"))
            (format t "  Total Events: ~A~%" (ch:jget data "total_events")))))

  ;; Revenue metrics
  (let ((revenue-metrics (ch:query db (format nil "SELECT 
                                                 sum(total_amount - discount_amount) as total_revenue,
                                                 count() as total_orders,
                                                 avg(total_amount - discount_amount) as avg_order_value
                                               FROM sales
                                               WHERE sale_timestamp >= today() - INTERVAL ~A DAY
                                               FORMAT JSON" days))))
    (when (listp revenue-metrics)
          (let ((data (first (ch:jget revenue-metrics "data"))))
            (format t "  Total Revenue: $~,2F~%" (ch:jget data "total_revenue"))
            (format t "  Total Orders: ~A~%" (ch:jget data "total_orders"))
            (format t "  Average Order Value: $~,2F~%" (ch:jget data "avg_order_value"))))))

;; Generate KPI dashboard
(get-kpi-dashboard *db* 30)

(defun analyze-user-segment (db segment-condition &optional (segment-name "Custom Segment"))
  "Analyze a specific user segment"
  (format t "~%Analyzing segment: ~A~%" segment-name)
  (let ((segment-query (format nil "WITH segment_users AS (
                                      SELECT DISTINCT user_id
                                      FROM user_events
                                      WHERE ~A
                                    )
                                    SELECT 
                                      count(DISTINCT e.user_id) as segment_size,
                                      avg(e.revenue) as avg_revenue_per_event,
                                      count(DISTINCT e.session_id) as total_sessions
                                    FROM user_events e
                                    JOIN segment_users s ON e.user_id = s.user_id
                                    FORMAT JSON" segment-condition)))
    (let ((result (ch:query db segment-query)))
      (when (listp result)
            (let ((data (first (ch:jget result "data"))))
              (format t "  Segment size: ~A users~%" (ch:jget data "segment_size"))
              (format t "  Avg revenue per event: $~,2F~%" (ch:jget data "avg_revenue_per_event"))
              (format t "  Total sessions: ~A~%" (ch:jget data "total_sessions")))))))

;; Example segment analyses
(analyze-user-segment *db* "country = 'US'" "US Users")
(analyze-user-segment *db* "device_type = 'mobile'" "Mobile Users")
(analyze-user-segment *db* "event_type = 'purchase'" "Purchasers")

(format t "~%=== ANALYTICS BEST PRACTICES ===~%")
(format t "1. Use partitioning by time for time-series data~%")
(format t "2. Create appropriate ORDER BY keys for your common queries~%")
(format t "3. Use materialized views for frequently calculated metrics~%")
(format t "4. Pre-aggregate data for real-time dashboards~%")
(format t "5. Use sampling for exploratory analysis on large datasets~%")
(format t "6. Leverage ClickHouse's statistical functions for advanced analytics~%")
(format t "7. Consider using dictionaries for dimension data~%")

(format t "~%=== CLEANUP ===~%")
(format t "To clean up analytics demo:~%")
(format t "  (ch:query *db* \"DROP DATABASE analytics_demo\")~%")

(format t "~%=== Analytics Examples Complete ===~%")
(format t "These examples demonstrate:~%")
(format t "- Basic web analytics (DAU, page views, conversions)~%")
(format t "- Revenue and business metrics~%")
(format t "- Funnel and cohort analysis~%")
(format t "- Geographic and device analysis~%")
(format t "- Customer lifetime value calculations~%")
(format t "- User retention and segmentation~%")
