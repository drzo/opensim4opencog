********************************************
*                                          *
*  FrameNet 1.3 API in C#                  *
*  Date:  September 16, 2007               *
*  Author: Matt Gerber (gerberm2@msu.edu)  *
*                                          *
********************************************

LICENSE

Free. Do whatever you want with it.


COMPILING

1) The FrameNet API depends on some libraries that are distributed
   separately. To get them, perform a Subversion checkout from the following
   URL:

   http://links.cse.msu.edu:8000/svn/NLP/Libraries/Public

2) Right-click on the FrameNet project, select "Properties", and go to
   "Build". Change "Output path" to the Public directory that was checked out
   in the previous step. Check the box next to "XML documentation file".

3) Right-click on the FrameNet project and select "Build".


USE

See the test application.


LIMITATIONS

The API has been tested only on FrameNet version 1.3. I have no idea
if it will work with other versions of the data. Furthermore, it isn't
terribly complete (e.g., POS information is not represented) or efficient (it's
something of a memory hog right now).

BUGS

None that I know of, but if you find any please contact me at gerberm2@msu.edu
