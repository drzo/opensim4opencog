using cogbot;
using MushDLR223.ScriptEngines;
using Radegast;

namespace CogbotRadegastPluginModule
{
    public class CogbotRadegastInterpreter: Radegast.Commands.ICommandInterpreter
    {
        private ClientManager clientManager
        {
           get
           {
               return BotClient.ClientManager;
           }
        }
       // private BotClient botClient;
        public BotClient BotClient
        {
            get
            {
                return cogbotRadegastPlugin.TheBot;
            }
          /*  set
            {
                if (botClient == value)
                    return;
                botClient = value;
            }*/
        }


        public CogbotRadegastInterpreter(CogbotRadegastPlugin manager)
        {
            cogbotRadegastPlugin = manager;//
            //clientManager = manager;
            //botClient = manager.LastBotClient;
        }
        public RadegastInstance RadegastInstance;
        private CogbotRadegastPlugin cogbotRadegastPlugin;

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
            OutputDelegate newOutputDelegate = new OutputDelegate(WriteLine);
            CmdResult result;
            var botClient = BotClient;
            if (botClient == null)
            {
                result = clientManager.ExecuteCommand(cmdline,newOutputDelegate);
            }
            else
            {
                result = botClient.ExecuteCommand(cmdline,newOutputDelegate);
            }

            if (result != null)
                WriteLine(result.ToString());
            else WriteLine("No result returned: {0}", cmdline);
        }

        public void Help(string helpArgs, ConsoleWriteLine WriteLine)
        {
            WriteLine(clientManager.ExecuteCommand("help " + helpArgs, new OutputDelegate(WriteLine)).ToString());         
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