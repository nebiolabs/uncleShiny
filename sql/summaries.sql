-- !preview conn=ebase_test

WITH cte_sum AS
    (SELECT wells.layout_address AS well, sum.* 
    FROM uncle_summaries sum
    INNER JOIN wells
      ON sum.well_id = wells.id
    WHERE EXISTS (SELECT *
                  FROM uncle_experiments exps
                  WHERE exps.uncle_experiment_set_id IN (28)
                    AND sum.uncle_experiment_id = exps.id)
    )
  SELECT p.name AS product_name, p.id AS product_id,
    exp_sets.id AS exp_set_id, exp_sets.exp_type, exp_sets.plate_generation,
    exps.id AS exp_id, cte_sum.*
  FROM cte_sum
  INNER JOIN uncle_experiments AS exps
    ON cte_sum.uncle_experiment_id = exps.id
  INNER JOIN uncle_experiment_sets AS exp_sets
    ON exps.uncle_experiment_set_id = exp_sets.id
  INNER JOIN products AS p
    ON exp_sets.product_id = p.id