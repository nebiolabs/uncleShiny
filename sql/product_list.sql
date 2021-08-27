-- !preview conn=ebase_test

SELECT p.name AS product_name, p.id AS product_id, p.catalog_number
FROM products p
WHERE EXISTS (SELECT *
              FROM uncle_experiment_sets exp_sets
              WHERE exp_sets.product_id = p.id)
ORDER BY p.name
