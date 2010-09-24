*********************************************
*                                           *
*  NomBank API in C#                        *
*  Date:  February 15, 2009                 *
*  Author:  Matt Gerber (gerberm2@msu.edu)  *
*                                           *
*********************************************


LICENSE

Free. Do whatever you want with it.


CORPUS MODIFICATIONS

Make the changes described in the "my_modifications.txt" file of the NomBank 
project.


COMPILATION

1) Build the PennBank API according to its README.txt. 

2) Right-click on the NomBank project, select "Properties", and go to
   "Build". Change "Output path" to the Public directory that was configured
   when building the PennBank API. Check the box next to "XML documentation
   file".

3) Right-click on the NomBank project and select "Build".


INDEX BUILDING

The first time a NomBankEngine is constructed, some associated indexes are
built that allow efficient retrieval of attestations while consuming minimal
memory. This process might take a while, but only needs to be done once. 
Subsequent constructions with the same parameters will be much quicker.


BUG REPORTS

Send to gerber.matthew@gmail.com
