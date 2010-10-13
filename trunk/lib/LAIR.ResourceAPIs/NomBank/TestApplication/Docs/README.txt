*********************************************
*                                           *
*  NomBank API in C#                        *
*  Date:  February 15, 2009                 *
*  Author:  Matt Gerber (gerberm2@msu.edu)  *
*                                           *
*********************************************


LICENSE

Free. Do whatever you want with it.


COMPILATION

1) Compile the NomBank project as described in README.txt in the NomBank
   project.

2) Right-click on the TestApplication project, select "Properties", and go to
   "Reference Paths". Add the Public folder that was configured when building
   the PennBank API.

3) Right-click on the TestApplication project and select "Build".


USE

Here are a few examples using the TestApplication:

1) Enter a noun in the "Noun" box. Click "Get attestations" to retrieve all
   labeled argument structures for the given noun.

2) Select a node type to retrieve all nodes with the given type from argument
   structures for the noun in the "Noun" box.

3) Likewise with the feature type.

4) The "Test retrieval of all indexed attestations..." button attempts to
   retrieve all attestations for all nominalizations in the database.


LIMITATIONS

This test application leaves out a lot of functionality present in the
API. Browse the API documentation for a better idea about what's in there.


BUG REPORTS

Send to gerber.matthew@gmail.com
