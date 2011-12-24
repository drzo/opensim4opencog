/* Run on a fresh, UTF-8 encoded database */

/*   Table:  titleindex
 * Purpose:  Provides a quick index from words to page titles containing those words
 */
DROP TABLE IF EXISTS titleindex;
CREATE TABLE titleindex 
(
  word varchar(255) NOT NULL,
  page_namespace int NOT NULL,
  page_title varchar(255) NOT NULL,

  PRIMARY KEY word_title (word, page_namespace, page_title)
) TYPE=InnoDB;
