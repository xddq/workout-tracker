-- migrate:up
ALTER TABLE workouts ADD COLUMN note text NOT NULL DEFAULT '';

-- migrate:down
ALTER TABLE workouts DROP COLUMN note;

