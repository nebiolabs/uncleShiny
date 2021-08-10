-- !preview conn=ebase_ro

SELECT p.name AS product_name, p.id AS product_id, p.catalog_number
FROM products p
WHERE EXISTS (SELECT *
              FROM uncle_experiment_sets exp_sets
              WHERE exp_sets.product_id = p.id)
