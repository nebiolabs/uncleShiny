-- !preview conn=ebase_ro

SELECT sets.id AS set_id, prods.name, sets.product_id, sets.exp_type,
    sets.plate_generation AS gen, sets.well_set_id
FROM uncle_experiment_sets AS sets
INNER JOIN products AS prods
  ON sets.product_id = prods.id 
--WHERE product_id = {input}