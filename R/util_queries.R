
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

# Entries from products with matching Uncle experiment_sets
sql_queries$products <- {
  "SELECT p.name AS product_name, p.id AS product_id, p.catalog_number
  FROM products p
  WHERE EXISTS (SELECT *
                FROM uncle_experiment_sets exp_sets
                WHERE exp_sets.product_id = p.id)"
}


##--------------------------------------------------------
##  Table of available experiment sets                  --
##--------------------------------------------------------

# Experiment sets for user-selected product {input}
sql_queries$experiment_sets <-  {
  "SELECT id AS set_id, product_id,
    exp_type, plate_generation AS gen, well_set_id
  FROM uncle_experiment_sets
  WHERE product_id = {input}"
}


##--------------------------------------------------------
##  Table of available experiments                      --
##--------------------------------------------------------

# Individual experiments for available experiment sets for user-selected product
# Note that for glue::glue_sql, * does argument expansion for server
sql_queries$experiments <- {
  "SELECT id AS exp_id, uncle_experiment_set_id AS set_id,
    uncle_instrument_id AS inst_id, plate_side AS side, date
  FROM uncle_experiments
  WHERE uncle_experiment_set_id IN ({input*})"
}




# return(sql_queries)