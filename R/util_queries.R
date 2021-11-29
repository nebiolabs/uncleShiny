
##-------------------------------------------------------------------------
##  SQL queries                                                          --
##-------------------------------------------------------------------------

sql_queries <- list()


##-------------------------------------------------------
##  Table of available products                        --
##-------------------------------------------------------
# # INNER JOIN method will create replicate protein entries
# # if there are multiple matches in the experiment_sets table
# "SELECT p.name product_name, p.id product_id, p.catalog_number
#  FROM products p
#  INNER JOIN uncle_experiment_sets exp_sets
#   ON p.id = exp_sets.product_id"
# 
# # instead, use semi join (WHERE EXISTS) method to prevent the above issue,
# # also maybe faster, but negligible given table size
# "SELECT p.name AS product_name, p.id AS product_id, p.catalog_number
#                             FROM products p
#                             WHERE EXISTS (SELECT *
#                                           FROM uncle_experiment_sets exp_sets
#                                           WHERE exp_sets.product_id = p.id)"
# 
# # collapsing with DISTINCT also works, but is the slowest option
# "SELECT DISTINCT p.name AS product_name, p.id AS product_id,
#   p.catalog_number
# FROM products p
# INNER JOIN uncle_experiment_sets exp_sets
#   ON p.id = exp_sets.product_id)"
#
# Entries from products with matching Uncle experiment_sets
sql_queries$products <- {
  "SELECT p.name AS product_name, p.id AS product_id, p.catalog_number
  FROM products p
  WHERE EXISTS (SELECT *
                FROM uncle_experiment_sets exp_sets
                WHERE exp_sets.product_id = p.id)
  ORDER BY p.name"
}


##--------------------------------------------------------
##  Table of available experiment sets                  --
##--------------------------------------------------------
# Experiment sets for user-selected product {input}
sql_queries$experiment_sets <-  {
  "SELECT id AS exp_set_id, product_id,
    CONCAT(exp_type, '_', plate_generation) AS plate, well_set_id, 
    processing_status AS status, benchling_url, notes
  FROM uncle_experiment_sets
  WHERE product_id = {input}"
}


##--------------------------------------------------------
##  Table of available experiments                      --
##--------------------------------------------------------
# Individual experiments for available experiment sets for user-selected product
# Note that for glue::glue_sql, * does argument expansion for server
sql_queries$experiments <- {
  "SELECT id AS exp_id, uncle_experiment_set_id AS exp_set_id,
    uncle_instrument_id AS instrument, plate_side AS side, date,
    processing_errors AS errors
  FROM uncle_experiments
  WHERE uncle_experiment_set_id IN ({input*})"
}


##--------------------------------------------------------
##  Table of summary data for experiment selection      --
##--------------------------------------------------------
sql_queries$summary_data <- {
  "WITH cte_sum AS
    (SELECT wells.layout_address AS well, sum.* 
    FROM uncle_summaries sum
    INNER JOIN wells
      ON sum.well_id = wells.id
    WHERE EXISTS (SELECT *
                  FROM uncle_experiments exps
                  WHERE exps.uncle_experiment_set_id IN ({input*})
                    AND sum.uncle_experiment_id = exps.id)
    )
  SELECT p.name AS product_name, p.id AS product_id,
    exp_sets.id AS exp_set_id,
    CONCAT(exp_sets.exp_type, '_', exp_sets.plate_generation) AS plate,
    exps.id AS exp_id, exps.uncle_instrument_id AS instrument, exp_sets.notes,
    cte_sum.*
  FROM cte_sum
  INNER JOIN uncle_experiments AS exps
    ON cte_sum.uncle_experiment_id = exps.id
  INNER JOIN uncle_experiment_sets AS exp_sets
    ON exps.uncle_experiment_set_id = exp_sets.id
  INNER JOIN products AS p
    ON exp_sets.product_id = p.id"
}


##--------------------------------------------------------
##  Table of distinct condition groups                  --
##--------------------------------------------------------
sql_queries$condition_groups <- {
  "SELECT DISTINCT name
  FROM condition_groups"
}


##--------------------------------------------------------
##  Table of conditions/units for joining to summary    --
##--------------------------------------------------------
sql_queries$conditions_units <- {
  "WITH cte_sum AS
    (SELECT wells.layout_address AS well, sums.* 
    FROM uncle_summaries sums
    INNER JOIN wells
      ON sums.well_id = wells.id
    WHERE EXISTS (SELECT *
                  FROM uncle_experiments exps
                  WHERE exps.uncle_experiment_set_id IN ({input*})
                    AND sums.uncle_experiment_id = exps.id
                  )
    )
  SELECT exp_conds.well_id, exp_conds.raw_value AS unit_value,
    units.name AS unit_name,
    conds.name AS condition_name, conds.id AS condition_id,
    grps.name AS group_name, grps.id AS group_id,
    grpings.included, grpings.id AS grouping_id
  FROM experimental_conditions AS exp_conds
  INNER JOIN units
   ON exp_conds.unit_id = units.id
  INNER JOIN conditions AS conds
    ON exp_conds.condition_id = conds.id
  INNER JOIN condition_groupings AS grpings
    ON grpings.condition_id = conds.id
  INNER JOIN condition_groups AS grps
    ON grps.id = grpings.condition_group_id
  WHERE EXISTS (SELECT *
                FROM cte_sum
                WHERE cte_sum.well_id = exp_conds.well_id
                )"
}
