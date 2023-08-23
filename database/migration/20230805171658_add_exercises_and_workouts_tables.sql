-- migrate:up
CREATE TABLE workouts (
  id SERIAL PRIMARY KEY,
  type TEXT NOT NULL,
  date DATE NOT NULL
);

CREATE TABLE exercises (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  reps INT[] NOT NULL,
  note TEXT NOT NULL,
  -- position is used for ordering the exercises for a given workout
  position INT NOT NULL,
  weight_in_kg INT NOT NULL,
  workout_id INT REFERENCES workouts(id)
);

-- soft-deletes
CREATE TRIGGER deleted_records_insert AFTER DELETE ON workouts
  FOR EACH ROW EXECUTE FUNCTION deleted_records_insert();

-- soft-deletes
CREATE TRIGGER deleted_records_insert AFTER DELETE ON exercises
  FOR EACH ROW EXECUTE FUNCTION deleted_records_insert();

-- migrate:down
DROP TRIGGER IF EXISTS deleted_records_insert ON workouts;
DROP TRIGGER IF EXISTS deleted_records_insert ON exercises;
DROP TABLE IF EXISTS exercises;
DROP TABLE IF EXISTS workouts;

