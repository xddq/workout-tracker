-- migrate:up
CREATE TABLE deleted_records (
  id SERIAL PRIMARY KEY,
  data JSONB NOT NULL,
  object_id SERIAL NOT NULL,
  table_name VARCHAR(200) NOT NULL,
  deleted_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp
);

CREATE FUNCTION deleted_records_insert() RETURNS trigger
  LANGUAGE plpgsql
AS $$
  BEGIN
    EXECUTE 'INSERT INTO deleted_records (data, object_id, table_name) VALUES ($1, $2, $3)'
    USING to_jsonb(OLD.*), OLD.id, TG_TABLE_NAME;

    RETURN OLD;
  END;
$$;

-- migrate:down
DROP TABLE IF EXISTS deleted_records;
DROP TRIGGER IF EXISTS deleted_records_insert ON purchases;
DROP FUNCTION IF EXISTS deleted_records_insert;

