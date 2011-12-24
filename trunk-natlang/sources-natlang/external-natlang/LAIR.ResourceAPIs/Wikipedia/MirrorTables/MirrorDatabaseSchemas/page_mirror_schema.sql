/* Run on a fresh, UTF-8 encoded database */

/*   Table:  page
 * Purpose:  Provides a version of Wikipedia pages that is stripped of Wiki-markup
 */
DROP TABLE IF EXISTS page;
CREATE TABLE page
(
  page_id int(8) unsigned NOT NULL auto_increment,
  page_namespace int NOT NULL,
  page_title varchar (255) binary NOT NULL,
  page_text LONGBLOB NOT NULL,
  section_layout TEXT NOT NULL,
  redirects_to varchar(255),
  
  PRIMARY KEY page_id (page_id),
  INDEX ns_title (page_namespace, page_title)
) TYPE=InnoDB;

/*   Table:  pagelinks
 * Purpose:  Provides a quick lookup of internal wiki links between pages
 */
DROP TABLE IF EXISTS pagelinks;
CREATE TABLE pagelinks 
(
  list_id int(8) unsigned NOT NULL auto_increment,
  page_namespace int NOT NULL,
  page_title varchar(255) binary NOT NULL,
  link_list LONGBLOB NOT NULL,
  
  PRIMARY KEY list_id (list_id),
  INDEX ns_title (page_namespace, page_title)
) TYPE=InnoDB;

/*   Table:  termfreqs
 * Purpose:  Provides a quick lookup of term frequency counts within a page
 */
DROP TABLE IF EXISTS termfreqs;
CREATE TABLE termfreqs 
(
  list_id int(8) unsigned NOT NULL auto_increment,
  page_namespace int NOT NULL,
  page_title varchar(255) binary NOT NULL,
  freq_list LONGBLOB NOT NULL,

  PRIMARY KEY list_id (list_id),
  INDEX ns_title (page_namespace, page_title)
) TYPE=InnoDB;

