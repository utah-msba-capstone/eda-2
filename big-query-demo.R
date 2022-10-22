
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
