# The intial checkout of the project (**BIG**) #
<pre>
svn checkout http://opensim4opencog.googlecode.com/svn/trunk/ opensim4opencog-read-only<br>
</pre>
# Change Directory to the SVN Checkout dir #
<pre>
cd opensim4opencog-read-only<br>
</pre>
# Run GNU Make (it uses NAnt) #
<pre>
make<br>
</pre>
# Change to the run directory #
<pre>
cd bin<br>
</pre>
# Make sure X windows is running #
<pre>
mono ABuildStartup.exe<br>
</pre>


# Each time there is a SVN update #
cd to "opensim4opencog-read-only" Dir
svn update
make