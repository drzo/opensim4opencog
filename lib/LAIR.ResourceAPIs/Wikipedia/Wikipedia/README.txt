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

1) The Wikipedia API depends on some libraries that are distributed
   separately. To get them, perform a Subversion checkout from the following
   URL:

   http://links.cse.msu.edu:8000/svn/NLP/Libraries/Public

2) Right-click on the Wikipedia project, select "Properties", and go to
   "Build". Change "Output path" to the Public directory that was checked out
   in the previous step. Check the box next to "XML documentation file".

3) Right-click on the Wikipedia project and select "Build".


USE

This library does all the work in retrieving data from the MediaWiki database
and processing it into instances of the Page class and associated classes.


BUGS

None that I know of, but if you find any please contact me at gerberm2@msu.edu