-- migrate:up
-- creates tmp column in order to migrate the old values
ALTER TABLE exercises ADD COLUMN weight_in_kg_new INT[] DEFAULT ARRAY[0];
-- assuming that when we could only specify one weight we did use that weight
-- for each rep of a set.
UPDATE exercises SET weight_in_kg_new = array_fill(weight_in_kg, ARRAY[array_length(reps,1)]);
ALTER TABLE exercises DROP COLUMN weight_in_kg;
ALTER TABLE exercises RENAME COLUMN weight_in_kg_new TO weight_in_kg;

-- migrate:down
-- creates tmp column in order to migrate the old values
ALTER TABLE exercises ADD COLUMN weight_in_kg_new INT;
-- expects every exercise to have at least one weight specified (ensured by our
  -- DEFAULT [0] above)
UPDATE exercises SET weight_in_kg_new = weight_in_kg[1];
ALTER TABLE exercises DROP COLUMN weight_in_kg;
ALTER TABLE exercises RENAME COLUMN weight_in_kg_new TO weight_in_kg;
