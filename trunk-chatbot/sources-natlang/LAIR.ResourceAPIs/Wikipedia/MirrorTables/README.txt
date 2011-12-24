********************************************
*                                          *
*  Wikipedia API in C#                     *
*  Date:  September 25, 2007               *
*  Author: Matt Gerber (gerberm2@msu.edu)  *
*                                          *
********************************************

LICENSE

Free. Do whatever you want with it.


COMPILING

1) Compile the Wikipedia project as described in README.txt in the
   Wikipedia project.

2) Right-click on the MirrorTables project, select "Properties", and go to
   "Reference Paths". Add the Public folder that was checked out when building
   the Wikipedia project.

3) Build this project.


USE

This application reads pages from the database that holds the Wikipedia dump. It processes pages into sections, trims off 
all the markup, saves the internal Wiki links, computes page term frequencies, and writes the cleaned pages and other 
information to the mirror database. This mirrored database is significantly faster than the standard MediaWiki for retrieving 
Page objects (class Page within the Wikipedia project). Page objects can be retrieved from the standard MediaWiki database, 
but there is some overhead involved in processing the page into sections, removing markup, computing term frequencies, etc. The 
mirror database explicitly encodes this information.

You should get a decent feel about the API by looking at the test application for this project.


BUGS

None that I know of, but if you find any please contact me at gerberm2@msu.edu