-- !preview conn=ebase_test

WITH cte_sum AS
(SELECT wells.layout_address AS well, sum.* 
FROM uncle_summaries sum
INNER JOIN wells
  ON sum.well_id = wells.id
WHERE EXISTS (SELECT *
              FROM uncle_experiments exps
              WHERE exps.uncle_experiment_set_id IN (29)
                AND sum.uncle_experiment_id = exps.id)
)
SELECT exp_conds.well_id, exp_conds.raw_value AS unit_value, units.name AS unit_name,
  conds.name AS condition_name, conds.type AS condition_type
FROM experimental_conditions AS exp_conds
INNER JOIN units
  ON exp_conds.unit_id = units.id
INNER JOIN conditions AS conds
  ON exp_conds.condition_id = conds.id
WHERE EXISTS (SELECT *
              FROM cte_sum
              WHERE cte_sum.well_id = exp_conds.well_id)