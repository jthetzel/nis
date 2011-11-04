
## Delete rows from tables `dx` and `pr` with empty values
## This is optional, but will significantly reduce table size
## This is much faster if done prior to index creation
DELETE FROM `dx` WHERE `icd9` = "     ";
DELETE FROM `pr` WHERE `icd9` = "     ";

## Optimize tables `dx` and `pr` after delete;
OPTIMIZE TABLE `dx`;
OPTIMIZE TABLE `pr`;

## Create index on `icd9` columns in `dx` and `pr` tables
CREATE INDEX `icd9` ON `dx` (`icd9`);
CREATE INDEX `icd9` ON `pr` (`icd9`);

## Create primary key on `key` column in `core` table
ALTER TABLE `core` ADD PRIMARY KEY(`key`);

## Create index on `key` in `dx` and `pr` tables
CREATE INDEX `key` ON `dx` (`key`);
CREATE INDEX `key` ON `pr` (`key`);

## Create index on `variable` in `dx` and `pr` tables
CREATE INDEX `variable` ON `dx` (`variable`);
CREATE INDEX `variable` ON `pr` (`variable`);

## Create primary key on `hospid` in `hosptials` table
#! Is hospid uniqu?  Or is KEY(`hospid`, `year`) unique?
ALTER TABLE `hospitals` ADD PRIMARY KEY(`hospid`);

## Create index on `hospid` in `core` table
#! `hospid`  or  (`hospid`, `year`)?
CREATE INDEX `hospid` ON `core` (`hospid`);

## Create index on `age` in `core` table
CREATE INDEX `age` on `core` (`age`);





SELECT `icd9` FROM `dx` GROUP BY `icd9`;
select Field2
from tableName
group by Field2;



## To create database and tables directly from R, use a database connection
require(RMySQL)
drv <- dbDriver('MySQL')
conn <- dbConnect(drv, group='nis2')

query <- "SELECT `icd9` FROM `dx` GROUP BY `icd9`;"

res <- dbSendQuery(conn = conn, statement = query)
result <- fetch(res, n = -1)