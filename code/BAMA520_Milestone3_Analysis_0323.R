# ==============================================================================
# BAMA 520 — Milestone 3: Customer Retention Analysis
# Olist Brazilian E-Commerce Dataset
# UBC Sauder School of Business | MBAN Program | March 2026
# ==============================================================================
# Structure (mirrors HW2 template):
#   STEP 1  — Data Loading & Cleaning
#   STEP 2  — Feature Engineering (7 dimensions, ~22 features)
#   STEP 3  — CLV Business Case  [PLOTS 1–3, 3b]
#   STEP 4  — Balanced Sample & Logistic Regression  [PLOTS 4–5]
#   STEP 5  — Oversampling Correction
#   STEP 6  — Economic Analysis & Strategy Comparison  [PLOTS 6–7]
#   STEP 7  — Causal Awareness & Experiment Design
# ==============================================================================

# --- 0. PACKAGES & PATHS ------------------------------------------------------
# install.packages(c("tidyverse", "pROC", "scales"))
library(tidyverse)
library(pROC)
library(scales)
library(ggrepel)

# Set paths relative to script location
# Change DATA_DIR if running from a different working directory
DATA_DIR <- "data/"
OUT_DIR  <- "output_plots/"
dir.create(OUT_DIR, showWarnings = FALSE)

cat("==============================================================\n")
cat(" BAMA 520 — Milestone 3: Olist Customer Retention Analysis\n")
cat("==============================================================\n\n")

# ==============================================================================
# STEP 1: LOAD & CLEAN DATA
# ==============================================================================
cat("--- STEP 1: Loading data ---\n")

orders    <- read_csv(paste0(DATA_DIR, "olist_orders_dataset.csv"),           show_col_types = FALSE)
customers <- read_csv(paste0(DATA_DIR, "olist_customers_dataset.csv"),        show_col_types = FALSE)
items     <- read_csv(paste0(DATA_DIR, "olist_order_items_dataset.csv"),      show_col_types = FALSE)
reviews   <- read_csv(paste0(DATA_DIR, "olist_order_reviews_dataset.csv"),    show_col_types = FALSE)
payments  <- read_csv(paste0(DATA_DIR, "olist_order_payments_dataset.csv"),   show_col_types = FALSE)
products  <- read_csv(paste0(DATA_DIR, "olist_products_dataset.csv"),         show_col_types = FALSE)
cat_trans <- read_csv(paste0(DATA_DIR, "product_category_name_translation.csv"), show_col_types = FALSE)

cat(sprintf("  Raw orders: %s\n", comma(nrow(orders))))
cat(sprintf("  Customers: %s\n\n", comma(nrow(customers))))

# --- 1A: Parse timestamps & attach unique customer ID ---
orders <- orders %>%
  mutate(
    order_purchase_timestamp      = as.POSIXct(order_purchase_timestamp,      tz = "UTC"),
    order_delivered_customer_date = as.POSIXct(order_delivered_customer_date, tz = "UTC"),
    order_estimated_delivery_date = as.POSIXct(order_estimated_delivery_date, tz = "UTC")
  ) %>%
  left_join(customers %>% select(customer_id, customer_unique_id), by = "customer_id")

# --- 1B: Delivered orders only ---
delivered <- orders %>% filter(order_status == "delivered")
cat(sprintf("  Delivered orders: %s (dropped %s non-delivered)\n",
            comma(nrow(delivered)), comma(nrow(orders) - nrow(delivered))))

# --- 1C: Order rank per customer ---
delivered <- delivered %>%
  arrange(customer_unique_id, order_purchase_timestamp) %>%
  group_by(customer_unique_id) %>%
  mutate(order_rank = row_number()) %>%
  ungroup()

# --- 1D: Repeat-purchase flag ---
order_counts <- delivered %>%
  group_by(customer_unique_id) %>%
  summarise(n_orders = n_distinct(order_id), .groups = "drop") %>%
  mutate(is_repeat = as.integer(n_orders > 1))

delivered <- delivered %>% left_join(order_counts, by = "customer_unique_id")

n_unique   <- n_distinct(delivered$customer_unique_id)
n_repeat   <- sum(order_counts$is_repeat)
repeat_pct <- mean(order_counts$is_repeat)
cat(sprintf("  Unique customers: %s\n", comma(n_unique)))
cat(sprintf("  Repeat buyers:    %s (%.1f%%)\n\n", comma(n_repeat), repeat_pct * 100))

# --- 1E: Enrich items with product info & category translation ---
products_enriched <- products %>%
  left_join(cat_trans, by = "product_category_name") %>%
  rename(category = product_category_name_english)

items_enriched <- items %>%
  left_join(
    products_enriched %>% select(
      product_id, category,
      product_name_lenght, product_description_lenght, product_photos_qty,
      product_weight_g, product_length_cm, product_height_cm, product_width_cm
    ),
    by = "product_id"
  )

# Aggregate items per order (use first item for product attributes)
order_items_agg <- items_enriched %>%
  group_by(order_id) %>%
  summarise(
    total_price                = sum(price,         na.rm = TRUE),
    total_freight              = sum(freight_value, na.rm = TRUE),
    n_items                    = n(),
    seller_id_first            = first(seller_id),
    category_first             = first(category),
    product_name_lenght        = first(product_name_lenght),
    product_description_lenght = first(product_description_lenght),
    product_photos_qty         = first(product_photos_qty),
    product_weight_g           = first(product_weight_g),
    product_length_cm          = first(product_length_cm),
    product_height_cm          = first(product_height_cm),
    product_width_cm           = first(product_width_cm),
    .groups = "drop"
  )

# --- 1F: Delivery features ---
delivered <- delivered %>%
  mutate(
    # Impute missing actual delivery with estimated delivery
    order_delivered_customer_date = coalesce(
      order_delivered_customer_date, order_estimated_delivery_date
    ),
    delivery_time_days  = as.numeric(difftime(
      order_delivered_customer_date, order_purchase_timestamp, units = "days"
    )),
    delivery_delay_days = as.numeric(difftime(
      order_delivered_customer_date, order_estimated_delivery_date, units = "days"
    )),
    is_late = as.integer(delivery_delay_days > 0)
  )

# --- 1G: Build full analytical table ---
df <- delivered %>%
  left_join(order_items_agg, by = "order_id") %>%
  mutate(
    order_value   = total_price + total_freight,
    freight_ratio = total_freight / (total_price + total_freight + 0.01)
  )

cat(sprintf("  Full analytical table: %s rows\n\n", comma(nrow(df))))

# ==============================================================================
# STEP 2: FEATURE ENGINEERING
# ==============================================================================
cat("--- STEP 2: Feature engineering ---\n")

# --- 2A: Payment features ---
payment_agg <- payments %>%
  group_by(order_id) %>%
  summarise(
    n_installments    = max(payment_installments, na.rm = TRUE),
    used_installments = as.integer(max(payment_installments, na.rm = TRUE) > 1),
    used_boleto       = as.integer(any(payment_type == "boleto", na.rm = TRUE)),
    .groups = "drop"
  )

df <- df %>%
  left_join(payment_agg, by = "order_id") %>%
  mutate(
    n_installments    = replace_na(n_installments, 1),
    used_installments = replace_na(used_installments, 0),
    used_boleto       = replace_na(used_boleto, 0)
  )

# --- 2B: Review features (per-order average score) ---
review_agg <- reviews %>%
  group_by(order_id) %>%
  summarise(
    review_score = mean(review_score, na.rm = TRUE),
    has_review   = 1L,
    .groups = "drop"
  )

df <- df %>%
  left_join(review_agg, by = "order_id") %>%
  mutate(
    review_score = replace_na(review_score, 3.0),   # neutral imputation
    has_review   = replace_na(has_review,   0L)
  )

# --- 2C: Seller features ---
seller_volume <- items %>%
  group_by(seller_id) %>%
  summarise(seller_order_count = n_distinct(order_id), .groups = "drop")

seller_reviews_avg <- items %>%
  left_join(reviews %>% select(order_id, review_score), by = "order_id") %>%
  group_by(seller_id) %>%
  summarise(seller_avg_review = mean(review_score, na.rm = TRUE), .groups = "drop")

seller_info <- seller_volume %>%
  left_join(seller_reviews_avg, by = "seller_id") %>%
  mutate(
    is_top_seller = as.integer(
      seller_order_count >= quantile(seller_order_count, 0.75, na.rm = TRUE) &
        !is.na(seller_avg_review) & seller_avg_review >= 4.0
    )
  )

df <- df %>%
  left_join(seller_info, by = c("seller_id_first" = "seller_id")) %>%
  mutate(
    seller_order_count = replace_na(seller_order_count, 0),
    seller_avg_review  = replace_na(seller_avg_review,  3.0),
    is_top_seller      = replace_na(is_top_seller,      0L)
  )

# --- 2D: Physical product features ---
df <- df %>%
  mutate(
    product_name_lenght        = replace_na(product_name_lenght,        0),
    product_description_lenght = replace_na(product_description_lenght, 0),
    product_photos_qty         = replace_na(product_photos_qty,         0),
    product_weight_g           = replace_na(product_weight_g,           0),
    product_volume_cm3         = product_length_cm * product_height_cm * product_width_cm,
    product_volume_cm3         = replace_na(product_volume_cm3,         0)
  )

# --- 2E: Product category features (using first-order data only) ---
first_orders <- df %>% filter(order_rank == 1)
overall_rate <- mean(first_orders$is_repeat, na.rm = TRUE)

cat_stats <- first_orders %>%
  filter(!is.na(category_first)) %>%
  group_by(category_first) %>%
  summarise(
    n_cust      = n(),
    n_repeat    = sum(is_repeat, na.rm = TRUE),
    repeat_rate = mean(is_repeat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_cust >= 50) %>%
  arrange(desc(repeat_rate))

high_cats <- cat_stats %>% filter(repeat_rate > overall_rate * 1.5) %>% pull(category_first)
low_cats  <- cat_stats %>% filter(repeat_rate < overall_rate * 0.5) %>% pull(category_first)
consumable_cats <- c("food", "food_drink", "drinks", "health_beauty", "perfumery",
                     "diapers_and_hygiene", "baby", "fashion_underwear_beach")

df <- df %>%
  mutate(
    is_high_repeat_category = as.integer(coalesce(category_first %in% high_cats, FALSE)),
    is_low_repeat_category  = as.integer(coalesce(category_first %in% low_cats,  FALSE)),
    is_consumable           = as.integer(coalesce(category_first %in% consumable_cats, FALSE))
  )

# --- 2F: Filter to FIRST orders for the model ---
FEATURES <- c(
  # Delivery (3)
  "delivery_time_days", "delivery_delay_days", "is_late",
  # Satisfaction (2)
  "review_score", "has_review",
  # Cost/Economics (4)
  "total_price", "total_freight", "freight_ratio", "n_items",
  # Payment (3)
  "n_installments", "used_installments", "used_boleto",
  # Product Category (3)
  "is_high_repeat_category", "is_low_repeat_category", "is_consumable",
  # Seller (3)
  "seller_order_count", "seller_avg_review", "is_top_seller",
  # Physical (4)
  "product_description_lenght", "product_photos_qty",
  "product_weight_g", "product_volume_cm3"
)

model_df <- df %>%
  filter(order_rank == 1) %>%
  select(customer_unique_id, is_repeat, all_of(FEATURES)) %>%
  filter(
    !is.na(delivery_time_days),
    !is.na(total_price),
    delivery_time_days > 0,
    delivery_time_days < 365
  ) %>%
  # Replace any remaining NAs with 0 for model features
  mutate(across(all_of(FEATURES), ~replace_na(., 0)))

cat(sprintf("  Model dataset: %s first-order customers\n", comma(nrow(model_df))))
cat(sprintf("  Repeat buyers: %s (%.2f%%)\n\n",
            comma(sum(model_df$is_repeat)), mean(model_df$is_repeat) * 100))

# ==============================================================================
# STEP 3: CLV ANALYSIS & BUSINESS CASE
# ==============================================================================
cat("--- STEP 3: CLV Analysis ---\n")

# --- 3A: Revenue by customer segment ---
cust_lifetime <- df %>%
  group_by(customer_unique_id) %>%
  summarise(
    n_orders      = n_distinct(order_id),
    total_revenue = sum(total_price, na.rm = TRUE),
    is_repeat     = as.integer(n_distinct(order_id) > 1),
    .groups = "drop"
  )

repeat_custs  <- cust_lifetime %>% filter(is_repeat == 1)
onetime_custs <- cust_lifetime %>% filter(is_repeat == 0)

avg_rev_repeat  <- mean(repeat_custs$total_revenue)
avg_rev_onetime <- mean(onetime_custs$total_revenue)
MARGIN_RATE     <- 0.22 #assume Olist margin is 22% of the revenue

cat(sprintf("  One-time customers: %s | avg lifetime revenue: BRL %.2f\n",
            comma(nrow(onetime_custs)), avg_rev_onetime))
cat(sprintf("  Repeat customers:   %s | avg lifetime revenue: BRL %.2f\n",
            comma(nrow(repeat_custs)), avg_rev_repeat))
cat(sprintf("  Revenue multiplier: %.1fx\n", avg_rev_repeat / avg_rev_onetime))
cat(sprintf("  One-time margin (22%%):  BRL %.2f | Repeat margin (22%%): BRL %.2f\n\n",
            avg_rev_onetime * MARGIN_RATE, avg_rev_repeat * MARGIN_RATE))

# --- 3B: CLV formula ---
m <- mean(model_df$total_price, na.rm = TRUE)
r <- mean(model_df$is_repeat)
d <- 0.10
clv_avg <- m * r / (1 + d - r)
cat(sprintf("  Avg order value (m):  BRL %.2f\n", m))
cat(sprintf("  Repeat rate (r):      %.2f%%\n", r * 100))
cat(sprintf("  Avg CLV:     BRL %.2f\n\n", clv_avg))

# --- 3C: Whale curve ---
whale <- cust_lifetime %>%
  arrange(desc(total_revenue)) %>%
  mutate(
    pct_cust    = row_number() / n() * 100,
    pct_revenue = cumsum(total_revenue) / sum(total_revenue) * 100
  )
for (pct in c(10, 20, 30, 50)) {
  idx <- max(1, round(pct / 100 * nrow(whale)))
  cat(sprintf("  Top %d%% customers → %.1f%% of revenue\n", pct, whale$pct_revenue[idx]))
}

# ─── PLOT 1: Repeat vs One-Time Customer Value ────────────────────────────────
margin_comp <- tibble(
  Segment  = factor(c("One-time Buyer", "Repeat Buyer"),
                    levels = c("One-time Buyer", "Repeat Buyer")),
  Revenue  = c(avg_rev_onetime, avg_rev_repeat),
  Margin   = c(avg_rev_onetime * MARGIN_RATE, avg_rev_repeat * MARGIN_RATE)
)

p1 <- ggplot(margin_comp, aes(x = Segment, y = Margin, fill = Segment)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = sprintf("BRL %.0f", Margin)),
            vjust = -0.5, size = 5.5, fontface = "bold") +
  scale_fill_manual(values = c("#BFBFBF", "#002060")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Repeat Buyers Generate Significantly More Margin",
    subtitle = sprintf("%.1fx higher lifetime margin | Assumption: 22%% gross margin on revenue",
                       avg_rev_repeat / avg_rev_onetime),
    x = NULL, y = "Estimated Lifetime Margin (BRL)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "gray40"),
        axis.text = element_text(size = 12))
ggsave(paste0(OUT_DIR, "01_customer_value.png"), p1, width = 7, height = 5.5, dpi = 150)
cat("\n  ✓ Plot 1 saved: 01_customer_value.png\n")

# ─── PLOT 2: Repeat Rate by Product Category ─────────────────────────────────
top_cats_plot <- cat_stats %>%
  arrange(desc(repeat_rate)) %>%
  mutate(
    cat_label = str_replace_all(category_first, "_", " ") %>% str_to_title(),
    cat_high_low = case_when(
      category_first %in% high_cats ~ "high",
      category_first %in% low_cats  ~ "low",
      TRUE                          ~ "other"
    ),
    bar_color = case_when(
      category_first %in% high_cats ~ "#002060",
      category_first %in% low_cats  ~ "#C00000",
      TRUE                          ~ "#9DC3E6"
    )
  ) %>% 
  # filter(cat_high_low != 'other') %>% 
  group_by(cat_high_low) %>% 
  slice_head(n = 4)

cats_count <- cat_stats %>%
  arrange(desc(repeat_rate)) %>%
  mutate(
    cat_label = str_replace_all(category_first, "_", " ") %>% str_to_title(),
    cat_high_low = case_when(
      category_first %in% high_cats ~ "high",
      category_first %in% low_cats  ~ "low",
      TRUE                          ~ "other"
    )
  ) %>% 
  group_by(cat_high_low) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(ratio = n / sum(n))

p2 <- ggplot(top_cats_plot,
             aes(x = reorder(cat_label, repeat_rate), y = repeat_rate * 100)) +
  geom_col(fill = top_cats_plot$bar_color, show.legend = FALSE) +
  geom_hline(yintercept = overall_rate * 100, linetype = "dashed",
             color = "gray40", linewidth = 0.8) +
  annotate("text", x = 0.7, y = overall_rate * 100 + 0.15,
           label = sprintf("Overall baseline: %.1f%%", overall_rate * 100),
           hjust = 0, size = 3.8, color = "gray30") +
  geom_text(aes(label = sprintf("%.1f%%", repeat_rate * 100)),
            hjust = -0.15, size = 3.5, fontface = "bold",
            color = top_cats_plot$bar_color) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Repeat Purchase Rate Varies Widely Across Product Categories",
    subtitle = sprintf("Dark blue = high-repeat (≥1.5× baseline): %g (%0.1 f %%)\nBlue         = other (0.5x - 1.5x baseline)  :  %g (%0.1 f %%)\nRed          = low-repeat  (<0.5x baseline): %g (%0.1 f %%)",
                       cats_count[cats_count$cat_high_low == 'high', ]$n, round(cats_count[cats_count$cat_high_low == 'high', ]$ratio * 100, 1),
                       cats_count[cats_count$cat_high_low == 'other', ]$n, round(cats_count[cats_count$cat_high_low == 'other', ]$ratio * 100, 1),
                       cats_count[cats_count$cat_high_low == 'low', ]$n, round(cats_count[cats_count$cat_high_low == 'low', ]$ratio * 100, 1)
                       ),
      
      # "Dark blue = high-repeat (≥1.5× baseline); \nRed = low-repeat (<0.5x baseline) ;\n",
    x = NULL, y = "Repeat Purchase Rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray40"),
        panel.grid.major.y = element_blank())
ggsave(paste0(OUT_DIR, "02_repeat_by_category.png"), p2, width = 8, height = 6.5, dpi = 150)
cat("  ✓ Plot 2 saved: 02_repeat_by_category.png\n")

# ─── PLOT 3: Whale Curve ─────────────────────────────────────────────────────
pct20_revenue <- whale$pct_revenue[round(0.20 * nrow(whale))]

p3 <- ggplot(whale %>% filter(pct_cust <= 60),
             aes(x = pct_cust, y = pct_revenue)) +
  geom_ribbon(aes(ymin = pct_cust, ymax = pct_revenue),
              fill = "#002060", alpha = 0.12) +
  geom_line(color = "#002060", linewidth = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 20, linetype = "dotted", color = "#C00000", linewidth = 0.9) +
  geom_hline(yintercept = pct20_revenue, linetype = "dotted",
             color = "#C00000", linewidth = 0.9) +
  annotate("text", x = 22, y = pct20_revenue - 4,
           label = sprintf("Top 20%% → %.0f%% of revenue", pct20_revenue),
           color = "#C00000", hjust = 0, size = 4.5, fontface = "bold") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Revenue Is Highly Concentrated (Whale Curve)",
    subtitle = "Shaded area = inequality vs. uniform distribution",
    x = "Cumulative % of Customers (by revenue rank)",
    y = "Cumulative % of Revenue"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 14))
ggsave(paste0(OUT_DIR, "03_whale_curve.png"), p3, width = 8, height = 5.5, dpi = 150)
cat("  ✓ Plot 3 saved: 03_whale_curve.png\n\n")

# ─── PLOT 4: CLV Sensitivity to Retention Rate ───────────────────────────────
# CLV formula:  CLV = m * r / (1 + d - r)
# m = avg margin per purchase (avg order value × gross margin rate)
# r = retention (repeat-purchase) rate  |  d = annual discount rate
m_margin     <- mean(model_df$total_price, na.rm = TRUE) * MARGIN_RATE
n_customers  <- nrow(cust_lifetime)

r_grid       <- seq(0.005, 0.200, by = 0.005)
clv_grid     <- m_margin * r_grid / (1 + d - r_grid)
fleet_grid   <- clv_grid * n_customers

# Scale factor: converts fleet (right axis) to same y-range as per-customer (left axis)
scale_factor <- max(clv_grid) / max(fleet_grid / 1000)   # fleet values shown in BRL '000

sens_df <- tibble(
  r_pct  = r_grid * 100,
  clv    = clv_grid,
  fleet  = fleet_grid / 1000    # BRL '000 for legibility
)

clv_base_val   <- m_margin * r / (1 + d - r)
fleet_base_val <- clv_base_val * n_customers / 1000
r_base_pct     <- r * 100

# Milestone data with labels
milestone_df <- tibble(
  x     = c(5, 10, 15),
  y     = m_margin * c(0.05, 0.10, 0.15) / (1 + d - c(0.05, 0.10, 0.15)),
  label = c(
    sprintf("r=5%%\nBRL %.2f",  m_margin * 0.05 / (1 + d - 0.05)),
    sprintf("r=10%%\nBRL %.2f", m_margin * 0.10 / (1 + d - 0.10)),
    sprintf("r=15%%\nBRL %.2f", m_margin * 0.15 / (1 + d - 0.15))
  )
)

# Baseline label data
baseline_df <- tibble(
  x     = r_base_pct,
  y     = clv_base_val,
  label = sprintf("Baseline\nr = %.1f%%\nCLV = BRL %.2f",
                  r_base_pct, clv_base_val)
)

# Legend label constants
lbl_clv   <- "Per-customer CLV (left axis)"
lbl_fleet <- "Fleet CLV \u00d793,358 customers (right axis)"
lbl_base  <- "Current baseline (r = 3.0%)"

# ── Plot ──────────────────────────────────────────────────────────────────────
p4 <- ggplot(sens_df, aes(x = r_pct)) +
  
  # 1. Shaded area
  geom_area(aes(y = clv), fill = "#002060", alpha = 0.08) +
  
  # 2. Lines
  geom_line(aes(y = fleet * scale_factor, colour = lbl_fleet),
            linewidth = 1.8, linetype = "dashed") +
  geom_line(aes(y = clv, colour = lbl_clv), linewidth = 2.4) +
  
  # 3. Baseline vertical dotted line
  geom_vline(aes(xintercept = r_base_pct, colour = lbl_base),
             linewidth = 1.5, linetype = "dotted") +
  
  # 4. CLV track points
  geom_point(aes(y = clv), colour = "#002060", size = 1.4) +
  
  # 5. Baseline red dot
  geom_point(data = baseline_df,
             aes(x = x, y = y),
             colour = "#C0392B", size = 4) +
  
  # 6. Milestone gold dots
  geom_point(data = milestone_df,
             aes(x = x, y = y),
             colour = "#B8952A", size = 5,
             shape = 21, fill = "#B8952A", stroke = 0.8) +
  
  # 7. Milestone labels — directly above their dots
  geom_label_repel(
    data               = milestone_df,
    aes(x = x, y = y, label = label),
    nudge_x            = 0,
    nudge_y            = 0.90,
    direction          = "y",
    size               = 3.0,
    colour             = "#3a3a3a",
    fill               = "white",
    label.size         = 0.35,
    lineheight         = 1.2,
    segment.colour     = "#999999",
    segment.size       = 0.4,
    min.segment.length = 0,
    box.padding        = 0.3,
    point.padding      = 0.4,
    force              = 0,
    seed               = 42
  ) +
  
  # 8. Baseline label — directly above the red dot
  geom_label_repel(
    data               = baseline_df,
    aes(x = x, y = y, label = label),
    nudge_x            = 0,
    nudge_y            = 1.20,
    direction          = "y",
    size               = 3.2,
    fontface           = "bold",
    colour             = "#C0392B",
    fill               = "white",
    label.size         = 0.5,
    lineheight         = 1.2,
    segment.colour     = "#C0392B",
    segment.size       = 0.4,
    min.segment.length = 0,
    box.padding        = 0.3,
    point.padding      = 0.5,
    force              = 0,
    seed               = 42
  ) +
  
  # Colour scale
  scale_colour_manual(
    name   = NULL,
    values = c(
      lbl_clv   = "#002060",
      lbl_fleet = "#B8952A",
      lbl_base  = "#C0392B"
    ),
    breaks = c(lbl_clv, lbl_fleet, lbl_base)
  ) +
  
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype  = c("solid", "dashed", "dotted"),
        linewidth = c(2.4, 1.8, 1.5),
        shape     = c(NA, NA, NA)
      )
    )
  ) +
  
  # Dual axes
  scale_y_continuous(
    name   = "Per-Customer CLV  (BRL)",
    labels = function(x) sprintf("BRL %.2f", x),
    sec.axis = sec_axis(
      transform = ~ . / scale_factor,
      name      = sprintf("Fleet CLV  (BRL '000, \u00d7 %s customers)",
                          comma(n_customers)),
      labels    = function(x) sprintf("BRL %s K", comma(round(x, 0)))
    )
  ) +
  scale_x_continuous(
    name   = "Customer Retention Rate (r)",
    labels = function(x) paste0(x, "%"),
    limits = c(0, 21),
    breaks = seq(0, 20, by = 2),
    expand = expansion(mult = 0)
  ) +
  
  labs(
    title    = "CLV Sensitivity to Retention Rate  (per +0.5 pp increment)",
    subtitle = sprintf(
      "CLV = m \u00d7 r / (1 + d \u2212 r)   |   m = BRL %.2f (22%% margin on BRL %.0f avg order)   |   d = %.0f%%",
      m_margin, m_margin / 0.22, d * 100)
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle      = element_text(size = 8.5, colour = "gray50", hjust = 0.5),
    axis.title.y.left  = element_text(colour = "#002060", face = "bold", size = 10),
    axis.text.y.left   = element_text(colour = "#002060", size = 9),
    axis.title.y.right = element_text(colour = "#8B6914", face = "bold", size = 10),
    axis.text.y.right  = element_text(colour = "#8B6914", size = 9),
    axis.title.x       = element_text(face = "bold", size = 10),
    axis.text.x        = element_text(size = 9),
    panel.grid.major   = element_line(colour = "#e0e0e0", linewidth = 0.6),
    panel.grid.minor   = element_blank(),
    legend.position    = c(0.22, 0.82),
    legend.background  = element_rect(fill = "white", colour = "#cccccc",
                                      linewidth = 0.4),
    legend.title       = element_blank(),
    legend.text        = element_text(size = 8.5),
    legend.key.width   = unit(1.4, "cm")
  )

ggsave(paste0(OUT_DIR, "04_clv_sensitivity.png"),
       p4, width = 10, height = 5.8, dpi = 150)
cat("  ✓ Plot 4 saved: 04_clv_sensitivity.png\n\n")

# ==============================================================================
# STEP 4: BALANCED SAMPLE & LOGISTIC REGRESSION
# ==============================================================================
cat("--- STEP 4: Logistic Regression ---\n")

# --- 4A: Create 50/50 balanced sample (random undersampling) ---
set.seed(42)
repeat_rows  <- model_df %>% filter(is_repeat == 1)
onetime_rows <- model_df %>% filter(is_repeat == 0) %>%
  sample_n(nrow(repeat_rows))

balanced_df <- bind_rows(repeat_rows, onetime_rows) %>%
  mutate(is_repeat_f = factor(is_repeat, levels = c(0, 1), labels = c("No", "Yes")))

cat(sprintf("  Balanced sample: %s repeat + %s one-time = %s total\n\n",
            comma(nrow(repeat_rows)), comma(nrow(onetime_rows)), comma(nrow(balanced_df))))

# --- 4B: Fit logistic regression (glm) ---
model_formula <- as.formula(paste("is_repeat_f ~", paste(FEATURES, collapse = " + ")))
model <- glm(model_formula, data = balanced_df, family = binomial(link = "logit"))

cat("--- Model Summary (full) ---\n")
print(summary(model))

# --- 4C: Significant predictors, odds ratios ---
coef_tbl <- as.data.frame(summary(model)$coefficients) %>%
  rownames_to_column("Variable") %>%
  rename(Estimate = Estimate, SE = `Std. Error`, z_val = `z value`, p_value = `Pr(>|z|)`) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    OddsRatio = exp(Estimate),
    OR_lower  = exp(Estimate - 1.96 * SE),
    OR_upper  = exp(Estimate + 1.96 * SE),
    Sig       = case_when(p_value < 0.001 ~ "***",
                          p_value < 0.01  ~ "**",
                          p_value < 0.05  ~ "*",
                          TRUE            ~ ""),
    Direction = ifelse(Estimate > 0, "Increases Repeat", "Decreases Repeat")
  ) %>%
  arrange(p_value)

sig_vars <- coef_tbl %>% filter(p_value < 0.05)

cat("\n--- Significant Predictors (p < 0.05) ---\n")
print(
  sig_vars %>% select(Variable, Estimate, OddsRatio, p_value, Sig),
  digits = 3, row.names = FALSE
)
cat(sprintf("\n  %d significant predictors out of %d\n", nrow(sig_vars), length(FEATURES)))

# --- 4D: ROC Curve & AUC (on balanced sample) ---
pred_prob_train <- predict(model, type = "response")
roc_obj   <- roc(balanced_df$is_repeat_f, pred_prob_train,
                 levels = c("No", "Yes"), direction = "<", quiet = TRUE)
auc_value <- round(auc(roc_obj), 4)
cat(sprintf("\n  Model AUC (balanced sample): %s\n\n", auc_value))

# ─── PLOT 4: ROC Curve ───────────────────────────────────────────────────────
roc_df <- tibble(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities)

p5 <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_area(fill = "#002060", alpha = 0.10) +
  geom_line(color = "#002060", linewidth = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray60", linewidth = 0.8) +
  annotate("label", x = 0.65, y = 0.25,
           label = sprintf("AUC = %s", auc_value),
           size = 6, fontface = "bold", color = "#002060",
           fill = "white", label.size = 0.5) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1),
                     expand = expansion(mult = 0)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    title    = paste0("ROC Curve — Olist Repeat Purchase Model  (AUC = ", auc_value, ")"),
    subtitle = "Model predicts repeat purchase significantly better than random chance",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 14))
ggsave(paste0(OUT_DIR, "05_roc_curve.png"), p5, width = 7.5, height = 6, dpi = 150)
cat("  ✓ Plot 5 saved: 05_roc_curve.png\n")

# ─── PLOT 6: Odds Ratio Forest Plot (significant predictors only) ─────────────
var_labels <- c(
  delivery_time_days         = "Delivery Time (days)",
  delivery_delay_days        = "Delivery Delay (days)",
  is_late                    = "Late Delivery",
  review_score               = "Review Score",
  has_review                 = "Left a Review",
  total_price                = "Order Value (BRL)",
  total_freight              = "Freight Cost (BRL)",
  freight_ratio              = "Freight Ratio",
  n_items                    = "# Items in Order",
  n_installments             = "# Payment Installments",
  used_installments          = "Used Installments",
  used_boleto                = "Used Boleto Payment",
  is_high_repeat_category    = "High-Repeat Category",
  is_low_repeat_category     = "Low-Repeat Category",
  is_consumable              = "Consumable Product",
  seller_order_count         = "Seller Order Volume",
  seller_avg_review          = "Seller Avg. Review Score",
  is_top_seller              = "Top-Tier Seller",
  product_description_lenght = "Product Description Length",
  product_photos_qty         = "# Product Photos",
  product_weight_g           = "Product Weight (g)",
  product_volume_cm3         = "Product Volume (cm³)"
)

sig_plot <- sig_vars %>%
  mutate(
    VarLabel  = coalesce(var_labels[Variable], Variable),
    VarLabel  = factor(VarLabel, levels = VarLabel[order(OddsRatio)]),
    # Label nudge: push right of CI upper bound for increases, left for decreases
    label_x   = if_else(Direction == "Increases Repeat",
                        OR_upper + 0.02,
                        OR_lower - 0.02),
    or_label  = sprintf("OR = %.2f%s", OddsRatio, Sig)
  )

p6 <- ggplot(sig_plot, aes(x = OddsRatio, y = VarLabel, colour = Direction)) +
  
  # Baseline reference line
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = "gray40", linewidth = 0.8) +
  
  # Confidence interval bars
  geom_errorbarh(aes(xmin = OR_lower, xmax = OR_upper),
                 height = 0.35, linewidth = 0.9) +
  
  # Point estimates
  geom_point(size = 4) +
  
  # OR labels repelled horizontally away from the CI whisker ends
  geom_label_repel(
    aes(x = label_x, label = or_label),
    direction          = "x",
    nudge_x            = if_else(sig_plot$Direction == "Increases Repeat",
                                 0.08, -0.08),
    hjust              = if_else(sig_plot$Direction == "Increases Repeat",
                                 0, 1),
    size               = 3.5,
    fontface           = "bold",
    fill               = "white",
    label.size         = 0,          # no box border — clean look
    label.padding      = unit(0.15, "lines"),
    segment.size       = 0,          # no connector line needed
    min.segment.length = Inf,
    box.padding        = 0.1,
    point.padding      = 0.3,
    force              = 0,
    seed               = 42
  ) +
  
  scale_colour_manual(
    values = c("Increases Repeat" = "#002060",
               "Decreases Repeat" = "#C00000"),
    guide  = guide_legend(
      override.aes = list(
        shape     = 16,
        size      = 5,
        linetype  = "solid",
        linewidth = 1.0,
        label     = ""        # removes the a/b letter
      ),
      title = NULL
    )
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0.25, 0.30))
  ) +
  
  labs(
    title    = "Key Drivers of Repeat Purchase Behaviour",
    subtitle = "Statistically significant predictors (p < 0.05) | Odds ratios with 95% CI\n*** p<0.001  ** p<0.01  * p<0.05",
    x        = "Odds Ratio (relative to baseline)",
    y        = NULL,
    colour   = NULL
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(colour = "gray40", size = 10),
    legend.position    = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 10),
    axis.text.x        = element_text(size = 9)
  )

ggsave(paste0(OUT_DIR, "06_odds_ratios.png"), p6,
       width  = 10,
       height = max(5, nrow(sig_plot) * 0.55 + 2.5),
       dpi    = 150)
cat("  ✓ Plot 6 saved: 06_odds_ratios.png\n\n")

# ==============================================================================
# STEP 5: OVERSAMPLING CORRECTION
# ==============================================================================
cat("--- STEP 5: Oversampling Correction ---\n")

true_rate   <- mean(model_df$is_repeat)   # true population repeat rate
sample_rate <- 0.50                        # 50/50 balanced sample

# Log-odds shift (only intercept affected; all other coefficients valid)
correction <- log(true_rate / (1 - true_rate)) - log(sample_rate / (1 - sample_rate))
cat(sprintf("  True repeat rate:  %.4f (%.2f%%)\n", true_rate, true_rate * 100))
cat(sprintf("  Sample rate:       %.2f (%.0f%%)\n", sample_rate, sample_rate * 100))
cat(sprintf("  Correction factor: %.4f\n\n", correction))

# Apply to FULL population (model_df — all first-order customers)
log_odds_raw       <- predict(model, newdata = model_df, type = "link")
log_odds_corrected <- log_odds_raw + correction
pred_prob_corrected <- plogis(log_odds_corrected)   # inverse logit

model_df$pred_prob_corrected <- pred_prob_corrected

cat(sprintf("  Mean corrected P:   %.4f (%.2f%%)\n",
            mean(pred_prob_corrected), mean(pred_prob_corrected) * 100))
cat(sprintf("  Median corrected P: %.4f (%.2f%%)\n",
            median(pred_prob_corrected), median(pred_prob_corrected) * 100))
cat(sprintf("  Max corrected P:    %.4f (%.2f%%)\n\n",
            max(pred_prob_corrected), max(pred_prob_corrected) * 100))

# ==============================================================================
# STEP 6: ECONOMIC ANALYSIS — THRESHOLD & STRATEGY COMPARISON
# ==============================================================================
cat("--- STEP 6: Economic Analysis ---\n")

# Economic assumptions
INTERVENTION_COST  <- 2.0    # BRL per email (low-cost digital outreach)
INCREMENTAL_MARGIN <- (avg_rev_repeat - avg_rev_onetime) * MARGIN_RATE   # BRL  (22% margin × ~BRL 121 avg repeat order value)

breakeven_threshold <- INTERVENTION_COST / INCREMENTAL_MARGIN
cat(sprintf("  Intervention cost:     BRL %.1f\n", INTERVENTION_COST))
cat(sprintf("  Incremental margin:    BRL %.1f (22%% of avg order)\n", INCREMENTAL_MARGIN))
cat(sprintf("  Break-even threshold:  BRL %.1f / BRL %.1f = %.1f%%\n\n",
            INTERVENTION_COST, INCREMENTAL_MARGIN, breakeven_threshold * 100))

# ─── PLOT 7: Corrected Probability Distribution ───────────────────────────────
p7 <- ggplot(model_df, aes(x = pred_prob_corrected * 100)) +
  
  # Histogram
  geom_histogram(bins = 70, fill = "#9DC3E6",
                 colour = "white", alpha = 0.85) +
  
  # Break-even vertical line
  geom_vline(xintercept = breakeven_threshold * 100,
             linetype = "dashed", colour = "#C00000", linewidth = 1.2) +
  
  # Base rate vertical line
  geom_vline(xintercept = true_rate * 100,
             linetype = "dotted", colour = "#70AD47", linewidth = 1.1) +
  
  # Break-even label — right of red line, top of plot
  annotate("text",
           x        = breakeven_threshold * 100 + 1.5,
           y        = Inf,
           label    = sprintf("Break-even: %.1f%%\n(right of this = profitable to target)",
                              breakeven_threshold * 100),
           colour   = "#C00000",
           hjust    = 0,
           vjust    = 1.4,
           size     = 3.8,
           fontface = "bold") +
  
  # Base rate label — placed in open space on the right, clearly separated
  annotate("text",
           x        = 35,
           y        = 40000,
           label    = sprintf("Base rate: %.1f%%", true_rate * 100),
           colour   = "#3A7D22",
           hjust    = 0,
           vjust    = 1.4,
           size     = 3.8,
           fontface = "bold") +
  
  # Arrow from base rate label back to the green line
  annotate("segment",
           x        = 34.5,
           xend     = true_rate * 100 + 0.3,
           y        = 40000,
           yend     = 40000,
           colour   = "#3A7D22",
           linewidth = 0.5,
           vjust    = 2.5,
           arrow    = arrow(length = unit(0.2, "cm"), type = "closed")) +
  
  scale_x_continuous(
    breaks = c(0, 25, 50, 75),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  labs(
    title    = "Model-Corrected Repeat Purchase Probabilities",
    subtitle = sprintf(
      "After log-odds correction from 50/50 sample to %.1f%% true base rate.  N = %s customers.",
      true_rate * 100, comma(nrow(model_df))
    ),
    x = "Corrected P(Repeat Purchase)  (%)",
    y = "Number of Customers"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "gray40", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#e0e0e0", linewidth = 0.6),
    axis.text        = element_text(size = 10),
    axis.title       = element_text(size = 11)
  )

ggsave(paste0(OUT_DIR, "07_probability_dist.png"),
       p7, width = 10, height = 5.5, dpi = 150)
cat("  ✓ Plot 7 saved: 07_probability_dist.png\n")

# --- Strategy 1: Random targeting (same contact volume as model-targeted) ---
targeted_df <- model_df %>% filter(pred_prob_corrected > breakeven_threshold)
n_targeted  <- nrow(targeted_df)
cat(sprintf("\n  Customers above break-even threshold (%.1f%%): %s (%.1f%% of all)\n",
            breakeven_threshold * 100, comma(n_targeted),
            n_targeted / nrow(model_df) * 100))

random_exp_conv_rate <- true_rate
random_exp_conv    <- n_targeted * true_rate                    # same budget
random_revenue     <- random_exp_conv * INCREMENTAL_MARGIN
random_cost        <- n_targeted * INTERVENTION_COST
random_net_profit  <- random_revenue - random_cost

# --- Strategy 2: Model-targeted ---
targeted_exp_conv_rate  <- sum(targeted_df$pred_prob_corrected) / nrow(targeted_df)      # expectation of individual probabilities
targeted_exp_conv  <- sum(targeted_df$pred_prob_corrected)      # sum of individual probabilities
targeted_revenue   <- targeted_exp_conv * INCREMENTAL_MARGIN
targeted_cost      <- n_targeted * INTERVENTION_COST
targeted_net_profit <- targeted_revenue - targeted_cost

cat("\n")
cat("=================================================================\n")
cat("STRATEGY 1: RANDOM OUTREACH\n")
cat("-----------------------------------------------------------------\n")
cat(sprintf("  Customers contacted:   %s\n", comma(n_targeted)))
cat(sprintf("  Expected conversions:  %.1f  (%.1f%% base rate)\n",
            random_exp_conv, true_rate * 100))
cat(sprintf("  Gross margin:         BRL %s\n", comma(round(random_revenue))))
cat(sprintf("  Campaign cost:        BRL %s\n", comma(round(random_cost))))
cat(sprintf("  Net Profit:           BRL %s\n", comma(round(random_net_profit))))
cat("=================================================================\n")

cat("\n")
cat("=================================================================\n")
cat(sprintf("STRATEGY 2: MODEL-TARGETED  (P > %.1f%%)\n", breakeven_threshold * 100))
cat("-----------------------------------------------------------------\n")
cat(sprintf("  Customers contacted:   %s\n", comma(n_targeted)))
cat(sprintf("  Expected conversions:  %.1f\n", targeted_exp_conv))
cat(sprintf("  Gross margin:         BRL %s\n", comma(round(targeted_revenue))))
cat(sprintf("  Campaign cost:        BRL %s\n", comma(round(targeted_cost))))
cat(sprintf("  Net Profit:           BRL %s\n", comma(round(targeted_net_profit))))
cat("=================================================================\n")

cat(sprintf("\n  >>> Model Value (S2 − S1): BRL %s\n", comma(round(targeted_net_profit - random_net_profit))))

# Summary table
summary_tbl <- tibble(
  Strategy             = c("Random Outreach", "Model-Targeted"),
  `Customers Contacted` = c(n_targeted, n_targeted),
  `Expected Conversion Rate`= c(round(random_exp_conv_rate*100, 1), round(targeted_exp_conv_rate*100, 1)),  
  `Expected Conversions`= c(round(random_exp_conv, 1), round(targeted_exp_conv, 1)),
  `Gross Margin (BRL)` = c(round(random_revenue), round(targeted_revenue)),
  `Campaign Cost (BRL)` = c(round(random_cost), round(targeted_cost)),
  `Net Profit (BRL)`   = c(round(random_net_profit), round(targeted_net_profit))
)
cat("\n--- Summary Table ---\n")
print(summary_tbl, n = Inf)

# ─── PLOT 8: Strategy P&L Comparison ─────────────────────────────────────────
pl_comp <- tibble(
  Strategy = factor(
    rep(c(paste0("Random\nOutreach\nCVR:", summary_tbl$`Expected Conversion Rate`[1],"%"),
          paste0("Model-\nTargeted\nCVR:", summary_tbl$`Expected Conversion Rate`[2], "%")), 3),
    levels = c(paste0("Random\nOutreach\nCVR:", summary_tbl$`Expected Conversion Rate`[1],"%"),
               paste0("Model-\nTargeted\nCVR:", summary_tbl$`Expected Conversion Rate`[2], "%"))
  ),
  Component = rep(c("Gross Margin", "Campaign Cost", "Net Profit"), each = 2),
  Value     = c(
    random_revenue,    targeted_revenue,
    random_cost,       targeted_cost,
    random_net_profit, targeted_net_profit
  )
)

# Net Profit comparison bar
pl_profit <- pl_comp %>% filter(Component == "Net Profit") %>% 
  mutate(
    Conversion = c(summary_tbl$`Expected Conversion Rate`)
    ) %>% 
  mutate(label_text = sprintf("Net: BRL %g\nConverasion Rate: %.1f%%", round(Value), Conversion))
  

p8 <- ggplot(pl_profit, aes(x = Strategy, y = Value, fill = Value > 0)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(
    aes(label  = paste0("BRL ", comma(round(Value))),
        vjust  = ifelse(Value >= 0, -0.4, 1.4)),
    size = 5.5, fontface = "bold"
  ) +
  geom_hline(yintercept = 0, linewidth = 0.9) +
  scale_fill_manual(values = c("TRUE" = "#002060", "FALSE" = "#C00000")) +
  scale_y_continuous(labels = dollar_format(prefix = "BRL ", big.mark = ","),
                     expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title    = "Model-Targeted Outreach Generates Positive Net Profit",
    subtitle = sprintf(
      "Same %s customers contacted | BRL %.0f intervention cost | BRL %.0f incremental margin",
      comma(n_targeted), INTERVENTION_COST, INCREMENTAL_MARGIN
    ),
    x = NULL, y = "Net Profit (BRL)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "gray40"),
        axis.text.x = element_text(size = 13))
ggsave(paste0(OUT_DIR, "08_strategy_comparison.png"), p8, width = 9, height = 5.5, dpi = 150)
cat("\n  ✓ Plot 8 saved: 08_strategy_comparison.png\n")

p9 <- ggplot(pl_comp, aes(x = Strategy, y = Value, fill = Component)) +
  geom_col(position = "dodge", width = 0.55) +
  geom_text(
    aes(
      label = dollar(round(Value), prefix = "", big.mark = ",", accuracy = 1),
      vjust = ifelse(Value >= 0, -0.5, 1.5)   # above positive bars, below negative
    ),
    position = position_dodge(width = 0.55),   # must match geom_col width
    size = 3.5,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(values = c("Gross Margin" = "#002060", "Campaign Cost" = "#BFBFBF", "Net Profit" = "#C00000")) +
  scale_y_continuous(labels = dollar_format(prefix = "", big.mark = ","),
                     expand = expansion(mult = c(0.15, 0.15))) +  # more room top & bottom
  labs(
    title    = "P&L Comparison: Random vs. Model-Targeted Outreach",
    subtitle = sprintf("Cost: BRL %.0f per contact | Incremental margin: BRL %.0f per conversion",
                       INTERVENTION_COST, INCREMENTAL_MARGIN),
    x = NULL, y = "BRL", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(paste0(OUT_DIR, "09_pl_breakdown.png"), p9, width = 8, height = 5.5, dpi = 150)
cat("  ✓ Plot 9 saved: 09_pl_breakdown.png\n\n")

# ==============================================================================
# STEP 7: SEGMENTED TARGETING STRATEGY (high-repeat vs low-repeat vs other)
# ==============================================================================
cat("\n--- STEP 7: Segmented Targeting Strategy ---\n")

# Rebuild a light segmentation table with category flags for first-order customers
# NOTE: Added the requested metrics to the select() statement here so they carry over
segment_df <- df %>%
  filter(order_rank == 1) %>%
  select(customer_unique_id,
         category_first,
         is_high_repeat_category,
         is_low_repeat_category,
         n_items,               # Added for stats comparison
         n_installments,        # Added for stats comparison
         product_photos_qty,    # Added for stats comparison
         total_freight,         # Added for stats comparison
         delivery_delay_days,   # Added for stats comparison
         total_price) %>%       # Added for stats comparison
  left_join(
    model_df %>% select(customer_unique_id, pred_prob_corrected),
    by = "customer_unique_id"
  ) %>%
  mutate(
    cat_label = str_replace_all(category_first, "_", " ") %>% str_to_title(),
    segment = case_when(
      is_high_repeat_category == 1 ~ "High-repeat category",
      is_low_repeat_category  == 1 ~ "Low-repeat category",
      TRUE                         ~ "Other category"
    ),
    category_label = ifelse(is.na(category_first), "Unknown", category_first)
  )

# Keep only customers above the break-even threshold
seg_targeted <- segment_df %>%
  filter(pred_prob_corrected > breakeven_threshold) %>% 
  mutate(
    bar_color = case_when(
      category_first %in% high_cats ~ "#002060",
      category_first %in% low_cats  ~ "#C00000",
      TRUE                          ~ "#9DC3E6"
    )
  )

cat(sprintf("  Customers above threshold: %s\n\n", comma(nrow(seg_targeted))))

# Unique category_first of the "Other category" (Sorted by Volume)
cat("\n--- Unique Categories in 'Other category' Segment (By Customer Volume) ---\n")

other_categories_list <- seg_targeted %>%
  filter(segment == "Other category") %>%
  filter(!is.na(category_first)) %>%                 # Drop NAs before counting
  count(category_first, name = "customer_count") %>% # Count occurrences 
  arrange(desc(customer_count))                      # Sort descending by count

# Print the resulting data frame (n = Inf ensures all rows are shown)
print(other_categories_list, n = Inf)

# Key statistics comparison (High-repeat vs Other)
cat("\n--- Key Statistics Comparison ---\n")
stats_comparison <- seg_targeted %>%
  filter(segment %in% c("High-repeat category", "Other category")) %>%
  group_by(segment) %>%
  summarise(
    number_of_customers     = n(),
    avg_n_items             = mean(n_items, na.rm = TRUE),
    avg_n_installments      = mean(n_installments, na.rm = TRUE),
    avg_product_photos_qty  = mean(product_photos_qty, na.rm = TRUE),
    avg_total_freight       = mean(total_freight, na.rm = TRUE),
    avg_delivery_delay_days = mean(delivery_delay_days, na.rm = TRUE),
    avg_total_price         = mean(total_price, na.rm = TRUE),
    .groups = "drop"
  )

# Transpose or print directly for easy reading
print(stats_comparison, width = Inf)

# ------------------------------------------------------------------------------
# Segment-level Category customer distribution
# ------------------------------------------------------------------------------

high_repeat_seg_count <- seg_targeted %>%
  filter(segment == 'High-repeat category') %>%  
  group_by(cat_label, bar_color) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(desc(n))

cats_count_seg <- seg_targeted %>%
  group_by(segment, bar_color) %>%
  summarise(n = n(), .groups = 'drop')

other_repeat_seg_count <- cats_count_seg %>%
  filter(segment == 'Other category')  %>%
  rename(cat_label=segment)

union_cat_seg <- bind_rows(high_repeat_seg_count, other_repeat_seg_count) %>% 
  mutate(ratio = n / sum(n), rank=-row_number()) 

p10 <- ggplot(union_cat_seg,
              aes(x = reorder(cat_label,rank), y = n)) +
  geom_col(fill = union_cat_seg$bar_color, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%g (%.1f%%)", n, ratio * 100)),
            hjust = -0.15, size = 3.5, fontface = "bold",
            color = union_cat_seg$bar_color) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Targeted Customer Distribution by Category",
    subtitle = sprintf("Dark blue = high-repeat : %g (%0.1 f %%)\nBlue         =       other      : %g (%0.1 f %%)",
                       cats_count_seg[cats_count_seg$segment == 'High-repeat category', ]$n, 
                       round(cats_count_seg[cats_count_seg$segment == 'High-repeat category', ]$n / sum(cats_count_seg$n) * 100, 1),
                       union_cat_seg[union_cat_seg$cat_label == 'Other category', ]$n, 
                       round(union_cat_seg[union_cat_seg$cat_label == 'Other category', ]$ratio * 100, 1)
    ),
    
    # "Dark blue = high-repeat (≥1.5× baseline); \nRed = low-repeat (<0.5x baseline) ;\n",
    x = NULL, y = "Targeted Customer Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "gray40"),
        panel.grid.major.y = element_blank())
ggsave(paste0(OUT_DIR, "10_targeted_customer_distribution_by_category.png"), p10, width = 8, height = 6.5, dpi = 150)
cat("  ✓ Plot 10 saved: 10_targeted_customer_distribution_by_category.png\n")

# ------------------------------------------------------------------------------
# Segment-level P&L under current economics
# ------------------------------------------------------------------------------
segment_summary <- seg_targeted %>%
  group_by(segment) %>%
  summarise(
    CustomersTargeted    = n(),
    AvgPredProb          = mean(pred_prob_corrected),
    ExpectedConversions  = sum(pred_prob_corrected),
    GrossMargin          = ExpectedConversions * INCREMENTAL_MARGIN,
    CampaignCost         = CustomersTargeted * INTERVENTION_COST,
    NetProfit            = GrossMargin - CampaignCost,
    .groups = "drop"
  ) %>%
  arrange(desc(NetProfit))

cat("--- Segment-Level Targeting P&L ---\n")
print(segment_summary, n = Inf)

# ------------------------------------------------------------------------------
# Add profit per contacted customer for easier recommendation language
# ------------------------------------------------------------------------------
segment_summary <- segment_summary %>%
  mutate(
    ProfitPerContact = NetProfit / CustomersTargeted,
    ConversionRateExpected = ExpectedConversions / CustomersTargeted
  )

cat("\n--- Segment-Level Targeting P&L (with unit economics) ---\n")
print(segment_summary, n = Inf)

# ------------------------------------------------------------------------------
# Recommended differentiated action text helper
# ------------------------------------------------------------------------------
recommended_actions <- tibble(
  segment = c("High-repeat category", "Low-repeat category", "Other category"),
  suggested_action = c(
    "Reminder/replenishment email; lighter incentive if needed",
    "Selective outreach only; stronger incentive or deprioritize",
    "Standard re-engagement email"
  )
)

segment_summary_action <- segment_summary %>%
  left_join(recommended_actions, by = "segment")

cat("\n--- Recommendation by Segment ---\n")
print(segment_summary_action, n = Inf)

# Targeted P&L breakdown
pl_segment_comp <- tibble(
  Strategy = factor(
    rep(c(paste0("High-repeat\nCateogry\nCVR:", round(segment_summary$AvgPredProb[1]*100,1), "%\nContact #:",segment_summary$CustomersTargeted[1]), 
          paste0("Other\nCategory\nCVR:", round(segment_summary$AvgPredProb[2]*100,1), "%\nContact #:",segment_summary$CustomersTargeted[2])), 3),
    levels = c(paste0("High-repeat\nCateogry\nCVR:", round(segment_summary$AvgPredProb[1]*100,1), "%\nContact #:",segment_summary$CustomersTargeted[1]), 
               paste0("Other\nCategory\nCVR:", round(segment_summary$AvgPredProb[2]*100,1), "%\nContact #:",segment_summary$CustomersTargeted[2]))
  ),
  Component = rep(c("Gross Margin", "Campaign Cost", "Net Profit"), each = 2),
  Value     = c(
    segment_summary$GrossMargin,
    segment_summary$CampaignCost,
    segment_summary$NetProfit
  )
)

pl_segment_profit <- pl_segment_comp %>% 
  filter(Component == "Net Profit") %>%
  mutate(
    Conversion = 100 * c(segment_summary$AvgPredProb)
  ) %>% 
  mutate(label_text = sprintf("Net: BRL %g\nConversion Rate: %.1f%%", round(Value), Conversion))

pl_segment_full <- pl_segment_comp %>% 
  filter(Component %in% c("Gross Margin", "Campaign Cost"))

p11 <- ggplot(pl_segment_comp, aes(x = Strategy, y = Value, fill = Component)) +
  geom_col(position = "dodge", width = 0.55) +
  # --- NEW GEOM_TEXT LAYER ---
  geom_text(
    aes(label = dollar(round(Value), prefix = "", big.mark = ",")), # Formats the label 
    position = position_dodge(width = 0.55),                     # Aligns text with dodged bars
    vjust = -0.5,                                                # Pushes text just above the bar
    size = 4, 
    fontface = "bold",
    color = "black"
  ) +
  # ---------------------------
scale_fill_manual(values = c("Gross Margin" = "#002060", "Campaign Cost" = "#BFBFBF", "Net Profit" = "#C00000")) +
  scale_y_continuous(
    labels = dollar_format(prefix = "", big.mark = ","),
    expand = expansion(mult = c(0.05, 0.15)) # The 0.15 gives headroom so labels don't get cut off
  ) +
  labs(
    title    = "P&L Comparison - Breakdown by Category Segment",
    subtitle = sprintf("Cost: BRL %.0f per contact | Incremental margin: BRL %.0f per conversion",
                       INTERVENTION_COST, INCREMENTAL_MARGIN),
    x = NULL, y = "BRL", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave(paste0(OUT_DIR, "11_pl_segment_cateogry_breakdown.png"), p11, width = 8, height = 5.5, dpi = 150)
cat("  ✓ Plot11 saved: 11_pl_segment_cateogry_breakdown.png\n\n")

# ==============================================================================
# STEP 8: VALIDATION EXPERIMENT DESIGN 
# ==============================================================================
cat("\n--- STEP 8: Validation Experiment Design ---\n")

# We validate only within the model-targeted segment
# Business proposal: contact 80% of targeted customers, withhold email from 20%
holdout_rate <- 0.20
treat_n      <- round(n_targeted * (1 - holdout_rate))
control_n    <- n_targeted - treat_n

cat(sprintf("  Targeted customers available for test: %s\n", comma(n_targeted)))
cat(sprintf("  Treatment group (80%%): %s\n", comma(treat_n)))
cat(sprintf("  Holdout group (20%%):   %s\n\n", comma(control_n)))

# ------------------------------------------------------------------------------
# Decision rule:
# Incremental profit per targeted customer = lift × incremental margin - cost
# Roll out if this is positive
# ------------------------------------------------------------------------------

break_even_lift <- INTERVENTION_COST / INCREMENTAL_MARGIN

cat(sprintf("  Intervention cost per contacted customer: BRL %.2f\n", INTERVENTION_COST))
cat(sprintf("  Incremental margin per causal conversion: BRL %.2f\n", INCREMENTAL_MARGIN))
cat(sprintf("  Break-even causal lift needed: %.4f (%.2f percentage points)\n\n",
            break_even_lift, break_even_lift * 100))

cat("  Rollout decision rule:\n")
cat("  If observed incremental repeat-rate lift × incremental margin > contact cost,\n")
cat("  then scale to the full target base.\n")
cat(sprintf("  Equivalently, roll out if lift > %.2f percentage points.\n\n",
            break_even_lift * 100))

# ------------------------------------------------------------------------------
# Example decision table 
# ------------------------------------------------------------------------------

validation_scenarios <- tibble(
  assumed_lift = c(0.01, 0.02, 0.03, 0.05)   # 1 pp, 2 pp, 3 pp, 5 pp
) %>%
  mutate(
    incr_profit_per_contact = assumed_lift * INCREMENTAL_MARGIN - INTERVENTION_COST,
    treat_contacts          = treat_n,
    expected_incremental_conversions = treat_contacts * assumed_lift,
    expected_incremental_profit = treat_contacts * incr_profit_per_contact,
    decision = ifelse(incr_profit_per_contact > 0, "ROLL OUT", "DO NOT SCALE")
  )

cat("--- Validation Scenario Table ---\n")
print(validation_scenarios, n = Inf)

# ------------------------------------------------------------------------------
# Power calculation for experiment among targeted customers only
# Baseline here should be the expected repeat rate of the targeted segment
# ------------------------------------------------------------------------------

targeted_base_rate <- mean(targeted_df$pred_prob_corrected)

power_n <- function(p0, delta, alpha = 0.05, power = 0.80) {
  p1    <- p0 + delta
  p_bar <- (p0 + p1) / 2
  za    <- qnorm(1 - alpha / 2)
  zb    <- qnorm(power)
  ceiling(
    (za * sqrt(2 * p_bar * (1 - p_bar)) +
       zb * sqrt(p0 * (1 - p0) + p1 * (1 - p1)))^2 / delta^2
  )
}

cat("\n--- Power Analysis Within Targeted Segment ---\n")
cat(sprintf("  Expected baseline repeat rate in targeted group: %.2f%%\n\n",
            targeted_base_rate * 100))
cat(sprintf("  %-18s %-15s %-15s\n", "Detectable Lift", "N per arm", "Feasible now?"))
cat(paste0(rep("-", 50), collapse = ""), "\n")

power_tbl <- tibble(
  detectable_lift = c(0.01, 0.015, 0.02, 0.03)
) %>%
  mutate(
    n_per_arm = map_int(detectable_lift, ~ power_n(targeted_base_rate, .x)),
    feasible_now = ifelse(n_per_arm <= min(treat_n, control_n), "Yes", "No")
  )

for (i in 1:nrow(power_tbl)) {
  cat(sprintf("  +%.1f pp           %-15s %-15s\n",
              power_tbl$detectable_lift[i] * 100,
              comma(power_tbl$n_per_arm[i]),
              power_tbl$feasible_now[i]))
}

# ------------------------------------------------------------------------------
# Recommended validation strategy
# ------------------------------------------------------------------------------

cat("\nRecommended validation strategy")
cat(sprintf("  • Randomize within the %s targeted customers.\n", comma(n_targeted)))
cat(sprintf("  • Send email to %s customers; hold out %s customers.\n",
            comma(treat_n), comma(control_n)))
cat("  • Measure repeat purchase rate within 90 days after delivery.\n")
cat(sprintf("  • Roll out if observed lift exceeds %.2f percentage points,\n",
            break_even_lift * 100))
cat(sprintf("    because %.2f pp is the break-even causal lift under current economics.\n\n",
            break_even_lift * 100))

