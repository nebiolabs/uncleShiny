-- !preview conn=ebase_dev

/*SELECT sets.id AS set_id, prods.name, sets.product_id, sets.exp_type,
    sets.plate_generation AS gen, sets.well_set_id
FROM uncle_experiment_sets AS sets
INNER JOIN products AS prods
  ON sets.product_id = prods.id 
WHERE product_id = {input}*/

SELECT exp_sets.product_id,
    CONCAT(types.name, well_sets.uncle_plate_generation) AS plate,
    well_sets.uncle_plate_type_id AS plate_id,
    exp_sets.id AS exp_set_id, exp_sets.well_set_id,
    exp_sets.processing_status AS status, 
    exp_sets.benchling_url, exp_sets.notes
  FROM uncle_experiment_sets AS exp_sets
  INNER JOIN well_sets
    ON exp_sets.well_set_id = well_sets.id
  INNER JOIN uncle_plate_types AS types
    ON well_sets.uncle_plate_type_id = types.id
  WHERE product_id = 4732
  