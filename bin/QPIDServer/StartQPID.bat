
if exist "QPIDServer" cd QPIDServer
..\ikvm -Damqj.logging.level=info "-DQPID_HOME=..\QPIDServer\\" "-DQPID_WORK=" -cp lib/**;lib/qpid-all.jar org.apache.qpid.server.Main
exit 0
ikvmc lib\*.jar -out:..\LIBQPIDBROKER.DLL  -target:library
ikvmc -reference:..\LIBQPIDBROKER.DLL  -main:org.apache.qpid.server.Main -out:..\QPIDBROKER.EXE -target:exe
