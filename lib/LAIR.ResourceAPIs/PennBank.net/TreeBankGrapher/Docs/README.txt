********************************************
*                                          *
*  TreeBank grapher in C#                  *
*  Date:  April 25, 2009                   *
*  Author: Matt Gerber (gerberm2@msu.edu)  *
*                                          *
********************************************

LICENSE

Free. Do whatever you want with it.


COMPILATION

1) Compile the PennBank project as described in README.txt in the PennBank
   project.

2) Right-click on the TreeBankGrapher project, select "Properties", and go to
   "Reference Paths". Add the Public folder that was checked out when building
   the PennBank project.

3) Right-click on the TreeBankGrapher project and select "Build".


USE

You can either view a TreeBank tree (assuming you have the TreeBank data) or 
input a tree definition directly. Then, simply click the "Create graph" button.
The application will prompt you for an output path. The first time the 
application is run, you will also be prompted for the path to the GraphViz
Dot executable. You'll need to install the latest version of GraphViz 
from http://www.graphviz.org.


BUGS REPORTS

Send to gerber.matthew@gmail.com