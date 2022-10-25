
library(tidyverse)
library(bigrquery)

project_id <- 'is-6812-363412'

qq <- 'SELECT 
  visitorId,
  visitNumber,
  visitId,
  visitStartTime,
  `date`,
  totals.visits,
  trafficSource.source,
  h.transaction.transactionId,
  h.transaction.transactionRevenue,
  hp.productSKU,
  hp.v2ProductName,
  hp.v2ProductCategory,
  hp.productPrice,
  h.hitNumber
FROM `bigquery-public-data.google_analytics_sample.ga_sessions_*`, 
  UNNEST(hits) as h,
  unnest(h.product) as hp
where _table_suffix between "20170101" and "20170401"'

all_info_qq <- bq_project_query(project_id,qq)
all_info <- bq_table_download(all_info_qq)

hits_per_purchase <- all_info |>
    mutate(purch=case_when(transactionRevenue > 0 ~ 1,TRUE ~ 0)) |>
    group_by(productSKU,v2ProductName,v2ProductCategory) |>
    summarise(hits=n(),purchases=sum(purch),.groups='drop') |> 
    filter(purchases > 0)

ggplot(hits_per_purchase) + 
    geom_point(aes(x=hits,y=purchases,color=v2ProductCategory),size=2) +
    theme_bw()

prod_lm <- lm(purchases ~ hits,data=hits_per_purchase)

# hits_per_purchase$fitted_values <- predict(prod_lm,hits_per_purchase)

reg_analysis <- hits_per_purchase |>
    modelr::add_predictions(prod_lm) |>
    mutate(residuals=purchases-pred,conversion_rate=purchases/hits)

lbls <- reg_analysis |>
    slice_min(residuals,n=10)

ggplot(reg_analysis,aes(x=hits,y=purchases)) + 
    geom_point(aes(color=v2ProductCategory),size=2) +
    ggrepel::geom_text_repel(aes(label=v2ProductName),data=lbls) +
    theme_bw()

keep_products <- reg_analysis |>
    slice_max(residuals,n=10)

traffic_patterns <- all_info |>
    filter(productSKU %in% keep_products$productSKU) |>
    mutate(dt=as.Date(date,format='%Y%m%d')) |>
    group_by(productSKU,v2ProductName,source,dt) |>
    summarise(hits=n(),purchases=sum(case_when(transactionRevenue > 0 ~ 1,TRUE ~ 0)),
              revenue=sum(transactionRevenue/1000000,na.rm=T),.groups='drop')

ggplot(traffic_patterns) +
    geom_line(aes(x=dt,y=revenue,color=source)) +
    facet_wrap(~v2ProductName) +
    theme_bw()


source_summary <- all_info |>
    filter(productSKU %in% keep_products$productSKU) |>
    group_by(productSKU,v2ProductName,source) |>
    summarise(hits=n(),purchases=sum(case_when(transactionRevenue > 0 ~ 1,TRUE ~ 0)),
              revenue=sum(transactionRevenue/1000000,na.rm=T),.groups='drop') |>
    mutate(conversion_rate=purchases/hits)

top_sources <- source_summary |>
    filter(purchases > 0 ) |>
    group_by(productSKU) |>
    slice_max(conversion_rate,n=5)

ggplot(top_sources) +
    geom_col(aes(y=source,x=conversion_rate)) +
    facet_wrap(~v2ProductName,scales='free') +
    theme_bw()
