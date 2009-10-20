NANT	= $(shell if test "$$EMACS" = "t" ; then echo "nant"; else echo "./nant-color"; fi)

all: prebuild
	# @export PATH=/usr/local/bin:$(PATH)
	${NANT}
	find . -name \*.mdb -exec cp {} bin \; 

release: prebuild
	${NANT} -D:project.config=Release
	find . -name \*.mdb -exec cp {} bin \;

prebuild:
	./runprebuild.sh

distclean:
	# @export PATH=/usr/local/bin:$(PATH)
	#  ${NANT} clean
	find . -iname "*.build" -exec rm -v  {} \;
	find . -iname "*.csproj" -exec rm -v  {} \; 
        
clean:
	# @export PATH=/usr/local/bin:$(PATH)
	#${NANT} clean
	rm -vf bin/Cogbot*.exe bin/OpenM*.*

test: prebuild
	${NANT} test

test-xml: prebuild
	${NANT} test-xml

tags:
	find . -name \*\.cs | xargs etags 

