*********************************************
*                                           *
*  PropBank/TreeBank API in C#              *
*  Date:  February 15, 2009                 *
*  Author:  Matt Gerber (gerberm2@msu.edu)  *
*                                           *
*********************************************


LICENSE

Free. Do whatever you want with it.


CORPUS MODIFICATIONS

If using the PropBank API, make the changes described in the 
"my_modifications.txt" file, located in PropBank/Docs.


COMPILATION

1) The PennBank API depends on some libraries that are distributed
   separately. To get them, perform a Subversion checkout from the following
   URL:

   http://links.cse.msu.edu:8000/svn/NLP/Libraries/Public

2) Right-click on the PennBank project, select "Properties", and go to
   "Build". Change "Output path" to the Public directory that was checked out
   in the previous step. Check the box next to "XML documentation file".

3) Right-click on the PennBank project and select "Build".


INDEX BUILDING

The first time a TreeBankEngine or PropBankEngine is constructed, some
associated indexes are built that allow efficient retrieval of attestations
while consuming minimal memory. This process might take a while,
but only needs to be done the first time. Subsequent constructions with the
same parameters will be much quicker.


BUGS REPORTS

Send to gerber.matthew@gmail.com
