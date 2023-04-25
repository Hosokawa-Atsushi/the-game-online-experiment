CREATE USER IF NOT EXISTS the_game_user@localhost IDENTIFIED WITH mysql_native_password BY 'mysql_password';
CREATE DATABASE IF NOT EXISTS the_game;
GRANT ALL ON the_game.* TO the_game_user@localhost;
USE the_game;

-- 上書き用(子から先に消す)
DROP TABLE IF EXISTS pairs;
DROP TABLE IF EXISTS players;

-- プレイヤー情報の管理用テーブル
CREATE TABLE IF NOT EXISTS players(
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    kamei_id VARCHAR(50) NOT NULL UNIQUE,
    socket_id VARCHAR(30) DEFAULT NULL UNIQUE,
    is_host BOOLEAN DEFAULT NULL,
    is_connect BOOLEAN DEFAULT FALSE,
    is_male BOOLEAN DEFAULT NULL,
    age TINYINT UNSIGNED DEFAULT NULL,
    partner_id VARCHAR(20) DEFAULT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    -- updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY(partner_id) REFERENCES players(kamei_id) ON DELETE RESTRICT ON UPDATE CASCADE
) engine = innodb DEFAULT charset = utf8 COLLATE = utf8_unicode_ci AUTO_INCREMENT = 1;

INSERT INTO players SET kamei_id = 'model1', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model2', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model3', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model4', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model5', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model6', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model7', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model8', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model9', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model10', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model11', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'model12', is_host = FALSE, is_connect = NULL;
INSERT INTO players SET kamei_id = 'random', is_host = FALSE, is_connect = NULL;

-- ペア情報の管理用テーブル
CREATE TABLE IF NOT EXISTS pairs(
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    host_id VARCHAR(20) NOT NULL,
    guest_id VARCHAR(20) DEFAULT NULL,
    host_entered_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    guest_entered_at TIMESTAMP NULL DEFAULT NULL,
    status TINYINT UNSIGNED DEFAULT 0,
    FOREIGN KEY(host_id) REFERENCES players(kamei_id) ON DELETE RESTRICT ON UPDATE CASCADE,
    FOREIGN KEY(guest_id) REFERENCES players(kamei_id) ON DELETE RESTRICT ON UPDATE CASCADE
) engine = innodb DEFAULT charset = utf8 COLLATE = utf8_unicode_ci AUTO_INCREMENT = 1;
