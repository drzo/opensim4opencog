using cogbot;
using Radegast;

namespace CogbotRadegastPluginModule
{
    public class CogbotRadegastInterpreter: Radegast.Commands.ICommandInterpreter
    {
        private ClientManager clientManager;

        public CogbotRadegastInterpreter(ClientManager manager)
        {
            clientManager = manager;
        }
        public RadegastInstance RadegastInstance;
        public bool IsValidCommand(string cmdline)
        {
            if (cmdline.StartsWith("/")) return true;
            return false;
        }

        public void ExecuteCommand(ConsoleWriteLine WriteLine, string cmdline)
        {
            while (cmdline.StartsWith("/"))
            {
                cmdline = cmdline.Substring(1);
            }
            WriteLine(clientManager.ExecuteCommand(cmdline, new OutputDelegate(WriteLine)));
        }

        public void Help(string helpArgs, ConsoleWriteLine WriteLine)
        {
            WriteLine(clientManager.ExecuteCommand("help " + helpArgs, new OutputDelegate(WriteLine)));         
        }

        public void Dispose()
        {
           clientManager.Dispose();
        }

        public void StartInterpreter(RadegastInstance inst)
        {
            RadegastInstance = inst;
        }

        public void StopInterpreter(RadegastInstance inst)
        {
            Dispose();
        }
    }
}