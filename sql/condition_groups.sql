-- !preview conn=ebase_dev

WITH cte_sum AS
    (SELECT wells.layout_address AS well, sums.* 
    FROM uncle_summaries sums
    INNER JOIN wells
      ON sums.well_id = wells.id
    WHERE EXISTS (SELECT *
                  FROM uncle_experiments exps
                  WHERE exps.uncle_experiment_set_id IN (33, 32)
                    AND sums.uncle_experiment_id = exps.id
                  )
    )
SELECT exp_conds.well_id, exp_conds.raw_value AS unit_value,
  units.name AS unit_name, conds.name AS condition_name,
  grps.name AS group_name, grpings.*
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
              )
ORDER BY exp_conds.well_id