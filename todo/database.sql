CREATE DATABASE todo;

USE todo;

CREATE TABLE IF NOT EXISTS todo (
  id INT PRIMARY KEY AUTO_INCREMENT,
  title VARCHAR(255) NOT NULL
);

INSERT INTO todo (title) VALUES ('test1'), ('test2'), ('test3');
