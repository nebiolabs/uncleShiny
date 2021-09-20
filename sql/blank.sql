-- !preview conn=ebase_test
/*SELECT DISTINCT(sums.uncle_experiment_id) AS exp_id
FROM uncle_summaries sums
LEFT JOIN uncle_sls266s sls266s
  ON sums.id = sls266s.uncle_summary_id
WHERE sls266s.uncle_summary_id IS NULL;*/

/*WITH cte_sls266s AS (SELECT sums.uncle_experiment_id AS uncle_experiment_id,
                      sums.id AS uncle_summary_id,
                      COUNT(sls266s.id) AS n_sls266s
                      FROM uncle_summaries AS sums
                      INNER JOIN uncle_sls266s AS sls266s
                        ON sls266s.uncle_summary_id = sums.id
                      GROUP BY sums.uncle_experiment_id, sums.id
                    )
SELECT DISTINCT(uncle_experiment_id)
FROM cte_sls266s
WHERE n_sls266s > 140
ORDER BY uncle_experiment_id;*/
