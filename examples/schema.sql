-- schema.sql
-- Sample table schemas for clickhouse-cl examples
-- Run these commands in your ClickHouse instance to set up example tables

-- Create examples database
CREATE DATABASE IF NOT EXISTS examples;
USE examples;

-- =============================================================================
-- USER MANAGEMENT TABLES
-- =============================================================================

-- Users table
CREATE TABLE IF NOT EXISTS users (
    id UInt32,
    name String,
    email String,
    age UInt8,
    balance Decimal(10,2) DEFAULT 0.00,
    created_at DateTime DEFAULT now(),
    is_active Bool DEFAULT true,
    country String DEFAULT 'Unknown',
    city String DEFAULT 'Unknown'
) ENGINE = MergeTree()
ORDER BY id
SETTINGS index_granularity = 8192;

-- User sessions
CREATE TABLE IF NOT EXISTS user_sessions (
    session_id String,
    user_id UInt32,
    start_time DateTime,
    end_time DateTime,
    device_type Enum('desktop' = 1, 'mobile' = 2, 'tablet' = 3),
    user_agent String,
    ip_address String
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(start_time)
ORDER BY (user_id, start_time)
SETTINGS index_granularity = 8192;

-- =============================================================================
-- E-COMMERCE TABLES
-- =============================================================================

-- Products catalog
CREATE TABLE IF NOT EXISTS products (
    product_id UInt32,
    product_name String,
    category String,
    subcategory String DEFAULT '',
    brand String DEFAULT '',
    price Decimal(10,2),
    cost Decimal(10,2),
    weight_grams UInt32 DEFAULT 0,
    dimensions String DEFAULT '',
    description String DEFAULT '',
    created_at DateTime DEFAULT now(),
    is_active Bool DEFAULT true
) ENGINE = MergeTree()
ORDER BY (category, product_id)
SETTINGS index_granularity = 8192;

-- Orders
CREATE TABLE IF NOT EXISTS orders (
    order_id UInt64,
    user_id UInt32,
    order_timestamp DateTime,
    status Enum('pending' = 1, 'confirmed' = 2, 'shipped' = 3, 'delivered' = 4, 'cancelled' = 5),
    total_amount Decimal(10,2),
    discount_amount Decimal(10,2) DEFAULT 0.00,
    tax_amount Decimal(10,2) DEFAULT 0.00,
    shipping_amount Decimal(10,2) DEFAULT 0.00,
    payment_method Enum('credit_card' = 1, 'paypal' = 2, 'bank_transfer' = 3, 'crypto' = 4, 'cash' = 5),
    shipping_address String,
    billing_address String
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(order_timestamp)
ORDER BY (user_id, order_timestamp)
SETTINGS index_granularity = 8192;

-- Order items
CREATE TABLE IF NOT EXISTS order_items (
    order_id UInt64,
    product_id UInt32,
    quantity UInt32,
    unit_price Decimal(10,2),
    total_price Decimal(10,2),
    discount_per_item Decimal(10,2) DEFAULT 0.00
) ENGINE = MergeTree()
ORDER BY (order_id, product_id)
SETTINGS index_granularity = 8192;

-- =============================================================================
-- ANALYTICS AND EVENTS
-- =============================================================================

-- Web events tracking
CREATE TABLE IF NOT EXISTS web_events (
    event_id UInt64,
    session_id String,
    user_id Nullable(UInt32),
    event_timestamp DateTime,
    event_type Enum('page_view' = 1, 'click' = 2, 'form_submit' = 3, 'download' = 4, 'video_play' = 5, 'search' = 6),
    page_url String,
    referrer String DEFAULT '',
    user_agent String DEFAULT '',
    ip_address String DEFAULT '',
    country String DEFAULT '',
    city String DEFAULT '',
    device_type Enum('desktop' = 1, 'mobile' = 2, 'tablet' = 3),
    browser String DEFAULT '',
    os String DEFAULT '',
    screen_resolution String DEFAULT '',
    -- Additional event data as JSON
    event_properties String DEFAULT '{}'
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(event_timestamp)
ORDER BY (user_id, event_timestamp)
SETTINGS index_granularity = 8192;

-- Marketing campaigns
CREATE TABLE IF NOT EXISTS campaigns (
    campaign_id String,
    campaign_name String,
    campaign_type Enum('email' = 1, 'social' = 2, 'search' = 3, 'display' = 4, 'affiliate' = 5),
    start_date Date,
    end_date Date,
    budget Decimal(10,2) DEFAULT 0.00,
    target_audience String DEFAULT '',
    status Enum('draft' = 1, 'active' = 2, 'paused' = 3, 'completed' = 4)
) ENGINE = MergeTree()
ORDER BY (campaign_type, start_date)
SETTINGS index_granularity = 8192;

-- Campaign performance
CREATE TABLE IF NOT EXISTS campaign_performance (
    date Date,
    campaign_id String,
    impressions UInt64 DEFAULT 0,
    clicks UInt64 DEFAULT 0,
    conversions UInt64 DEFAULT 0,
    spend Decimal(10,2) DEFAULT 0.00,
    revenue Decimal(10,2) DEFAULT 0.00
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(date)
ORDER BY (campaign_id, date)
SETTINGS index_granularity = 8192;

-- =============================================================================
-- FINANCIAL DATA
-- =============================================================================

-- Transactions
CREATE TABLE IF NOT EXISTS transactions (
    transaction_id String,
    user_id UInt32,
    transaction_timestamp DateTime,
    transaction_type Enum('deposit' = 1, 'withdrawal' = 2, 'purchase' = 3, 'refund' = 4, 'fee' = 5),
    amount Decimal(15,2),
    currency String DEFAULT 'USD',
    status Enum('pending' = 1, 'completed' = 2, 'failed' = 3, 'cancelled' = 4),
    payment_processor String DEFAULT '',
    reference_id String DEFAULT '',
    description String DEFAULT ''
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(transaction_timestamp)
ORDER BY (user_id, transaction_timestamp)
SETTINGS index_granularity = 8192;

-- =============================================================================
-- TIME SERIES DATA
-- =============================================================================

-- System metrics (for monitoring examples)
CREATE TABLE IF NOT EXISTS system_metrics (
    timestamp DateTime,
    metric_name String,
    metric_value Float64,
    hostname String DEFAULT '',
    service_name String DEFAULT '',
    tags Map(String, String)
) ENGINE = MergeTree()
PARTITION BY toYYYYMMDD(timestamp)
ORDER BY (metric_name, timestamp)
SETTINGS index_granularity = 8192;

-- Stock prices (for financial analysis examples)
CREATE TABLE IF NOT EXISTS stock_prices (
    symbol String,
    price_date Date,
    open_price Decimal(10,2),
    high_price Decimal(10,2),
    low_price Decimal(10,2),
    close_price Decimal(10,2),
    volume UInt64,
    adjusted_close Decimal(10,2)
) ENGINE = MergeTree()
PARTITION BY toYYYYMM(price_date)
ORDER BY (symbol, price_date)
SETTINGS index_granularity = 8192;

-- =============================================================================
-- MATERIALIZED VIEWS (for performance examples)
-- =============================================================================

-- Daily user statistics
CREATE MATERIALIZED VIEW IF NOT EXISTS daily_user_stats
ENGINE = SummingMergeTree()
PARTITION BY toYYYYMM(date)
ORDER BY (date, user_id)
AS
SELECT
    toDate(event_timestamp) as date,
    user_id,
    count() as event_count,
    countIf(event_type = 'page_view') as page_views,
    countIf(event_type = 'click') as clicks,
    uniq(session_id) as sessions
FROM web_events
WHERE user_id IS NOT NULL
GROUP BY date, user_id;

-- Daily revenue summary
CREATE MATERIALIZED VIEW IF NOT EXISTS daily_revenue_stats
ENGINE = SummingMergeTree()
PARTITION BY toYYYYMM(date)
ORDER BY date
AS
SELECT
    toDate(order_timestamp) as date,
    sum(total_amount) as total_revenue,
    sum(discount_amount) as total_discounts,
    count() as total_orders,
    uniq(user_id) as unique_customers
FROM orders
WHERE status IN ('confirmed', 'shipped', 'delivered')
GROUP BY date;

-- =============================================================================
-- SAMPLE DATA INSERTION
-- =============================================================================

-- Insert sample users
INSERT INTO users (id, name, email, age, balance, country, city) VALUES
(1, 'Alice Johnson', 'alice@example.com', 28, 1500.00, 'US', 'New York'),
(2, 'Bob Smith', 'bob@example.com', 32, 2300.50, 'UK', 'London'),
(3, 'Charlie Brown', 'charlie@example.com', 25, 750.25, 'CA', 'Toronto'),
(4, 'Diana Prince', 'diana@example.com', 35, 5000.00, 'US', 'Los Angeles'),
(5, 'Eve Wilson', 'eve@example.com', 29, 1200.75, 'DE', 'Berlin'),
(6, 'Frank Miller', 'frank@example.com', 41, 3200.00, 'FR', 'Paris'),
(7, 'Grace Lee', 'grace@example.com', 27, 980.50, 'US', 'Chicago'),
(8, 'Henry Ford', 'henry@example.com', 33, 1800.25, 'UK', 'Manchester'),
(9, 'Ivy Thompson', 'ivy@example.com', 26, 1100.00, 'CA', 'Vancouver'),
(10, 'Jack Wilson', 'jack@example.com', 30, 2100.75, 'US', 'Seattle');

-- Insert sample products
INSERT INTO products (product_id, product_name, category, subcategory, brand, price, cost) VALUES
(1, 'MacBook Pro 16"', 'Electronics', 'Laptops', 'Apple', 2399.99, 1800.00),
(2, 'iPhone 15 Pro', 'Electronics', 'Smartphones', 'Apple', 999.99, 600.00),
(3, 'AirPods Pro', 'Electronics', 'Audio', 'Apple', 249.99, 120.00),
(4, 'Samsung Galaxy S24', 'Electronics', 'Smartphones', 'Samsung', 899.99, 500.00),
(5, 'Sony WH-1000XM4', 'Electronics', 'Audio', 'Sony', 349.99, 180.00),
(6, 'Dell XPS 13', 'Electronics', 'Laptops', 'Dell', 1299.99, 800.00),
(7, 'Nike Air Max 270', 'Fashion', 'Shoes', 'Nike', 159.99, 70.00),
(8, 'Levi\'s 501 Jeans', 'Fashion', 'Clothing', 'Levi\'s', 89.99, 40.00),
(9, 'Instant Pot Duo', 'Home & Kitchen', 'Appliances', 'Instant Pot', 99.99, 50.00),
(10, 'Dyson V11 Vacuum', 'Home & Kitchen', 'Appliances', 'Dyson', 599.99, 300.00);

-- =============================================================================
-- INDEXES FOR BETTER PERFORMANCE
-- =============================================================================

-- Add secondary indexes for common query patterns
-- (Note: ClickHouse uses different index types than traditional RDBMS)

-- Skipping index for user email lookups
ALTER TABLE users ADD INDEX idx_email_bloom email TYPE bloom_filter(0.01) GRANULARITY 1;

-- Skipping index for product categories
ALTER TABLE products ADD INDEX idx_category_set category TYPE set(100) GRANULARITY 1;

-- MinMax index for price ranges
ALTER TABLE products ADD INDEX idx_price_minmax price TYPE minmax GRANULARITY 1;

-- =============================================================================
-- USAGE NOTES
-- =============================================================================

/*
This schema provides:

1. User management tables for basic CRUD operations
2. E-commerce tables for order processing and inventory
3. Analytics tables for web tracking and campaign performance
4. Financial tables for transaction processing
5. Time series tables for metrics and monitoring
6. Materialized views for performance optimization

To use these tables in examples:
1. Run this file in your ClickHouse client
2. Modify connection parameters in example scripts to use 'examples' database
3. Use the sample data or generate more using the data generation functions

Performance considerations:
- Tables are partitioned by time for efficient querying
- Order keys are chosen based on common query patterns
- Materialized views pre-aggregate common metrics
- Indexes are added for frequent lookup patterns

For production use:
- Adjust data types based on your actual data ranges
- Consider compression settings for large tables
- Add more specific indexes based on your query patterns
- Implement proper data retention policies
*/