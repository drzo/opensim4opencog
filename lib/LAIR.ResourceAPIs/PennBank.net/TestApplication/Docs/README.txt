********************************************
*                                          *
*  PropBank/TreeBank API in C#             *
*  Date:  February 15, 2009                *
*  Author: Matt Gerber (gerberm2@msu.edu)  *
*                                          *
********************************************


LICENSE

Free. Do whatever you want with it.


COMPILATION

1) Compile the PennBank project as described in README.txt in the PennBank
   project.

2) Right-click on the TestApplication project, select "Properties", and go to
   "Reference Paths". Add the Public folder that was checked out when building
   the PennBank project.

3) Right-click on the TestApplication project and select "Build".


MACHINE-SPECIFIC PATHS

The test application must know where to find the TreeBank and/or PropBank
distribution directories. You don't need both resources to run the test
application. For each resource you _do_ have, locate the corresponding load
event handler in the main form's class - for TreeBank, this is
"loadTbBtn_Click" and for PropBank this is "loadPbBtn_Click". Replace all
relevant paths with paths that match your machine.


USE

Here are a few examples using the TestApplication:

TreeBank examples
-----------------
1) Browse to a .mrg file from the TreeBank 3 distribution. Click "Get
   attestations" to retrieve all indexed sentences from that file.

2) Enter a .mrg file and a grammatical function. Click "Get nodes by function"
   to retrieve all parse nodes with the given grammatical function.

3) Enter a search pattern (can be a C# regular expression) and click
   "Search". The surface text of all TreeBank parse trees are searched for
   matches with the pattern.

4) Click "Test retrieval of all indexed sentences" to do...well...exactly that.

PropBank examples
-----------------
1) Enter a verb in the verb box and click "Get attestations" to retrieve
   predicate-bracketed text for each annotated sentence.

2) Enter a verb in the verb box, select a node type, and click "Get nodes by
   type". All nodes of the given type are displayed.

3) Enter a verb in the verb box, select a feature type and click "Get nodes by
   feature". All nodes with the given feature are displayed.

4) Click "Test retrieval of all...". This will retrieve all attestations for
   all verbs.


LIMITATIONS

This test application leaves out a lot of functionality present in the
API. Browse the API documentation for a better idea about what's in there.


BUGS REPORTS

Send to gerber.matthew@gmail.com
