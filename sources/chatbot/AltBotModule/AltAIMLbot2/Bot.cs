using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Aima.Core.Logic.Propositional.Parsing.AST;
using AltAIMLbot;
using AltAIMLbot.Utils;
using LogicalParticleFilter1;

namespace AltAIMLbot
{
    public partial class AltBot
    {
               internal Result Chat(Request r, string graphName)
        {
            throw new NotImplementedException();
        }

        public Servitor servitor
        {
            get { throw new NotImplementedException(); }
        }

        public AltBot sm
        {
            get { throw new NotImplementedException(); }
        }

        public string rapStoreDirectoryStem
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }
        internal Result Chat(Request r)
        {
            throw new NotImplementedException();
        }

        internal void loadAIMLFromXML(XmlDocument chemsymDoc, string p)
        {
            throw new NotImplementedException();
        }

        public bool noSerialzation
        {
            get { throw new NotImplementedException(); }
        }

        public int SizeC
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public static AimlResult tl_aimlResult
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public EasyLogger Logger
        {
            get { throw new NotImplementedException(); }
        }

        public GraphMaster Graphmaster
        {
            get { throw new NotImplementedException(); }
        }

        public void updateRTP2Sevitor(User curUser)
        {
            throw new NotImplementedException();
        }

        public void loadAIMLFromFiles(string curAimlClass)
        {
            throw new NotImplementedException();
        }
        public void defineBehavior(string myName, string templateNodeTotalValue)
        {
            throw new NotImplementedException();
        }

        public void defineFSM(string myName, string templateNodeTotalValue)
        {
            throw new NotImplementedException();
        }

        public void startServitor()
        {
            throw new NotImplementedException();
        }

        public void LoadPersonality()
        {
            throw new NotImplementedException();
        }

        private void RegisterObject(string robot, Servitor value)
        {
            throw new NotImplementedException();
        }

        public void evalTemplateNode(XmlNode templateNode, RequestKind stateMachineProcess)
        {
            throw new NotImplementedException();
        }

        public void StampRaptstoreValid(bool b)
        {
            throw new NotImplementedException();
        }

        public void RunOnBotCreatedHooks()
        {
            throw new NotImplementedException();
        }

        public class AimlResult
        {
        }

        public void performBehaviors()
        {
            throw new NotImplementedException();
        }

        internal void loadAIMLFromFiles()
        {
            throw new NotImplementedException();
        }

        internal GraphMaster GetGraph(string gn)
        {
            throw new NotImplementedException();
        }

        public string PersonalizePath(string s)
        {
            throw new NotImplementedException();
        }

        public void AcceptInput(Action<string, object[]> writeLine, string input, User curUser, bool isToplevel, RequestKind requestType)
        {
            throw new NotImplementedException();
        }
    }

    public class EasyLogger
    {
        public void Warn(string s)
        {
            throw new NotImplementedException();
        }
    }

    /*
    public class MyActiveModelFacade
    {
        public bool IsTrue(Sentence sen)
        {
            throw new NotImplementedException();
        }
    }

    public class MyCronFacade
    {
        public void addLine(string myTimeLine)
        {
            throw new NotImplementedException();
        }
    }

    public class RealChemFacade
    {
        public void addChemical(string queueName, double parse, string aiml)
        {
            throw new NotImplementedException();
        }

        public void tick_chemistry(bool b)
        {
            throw new NotImplementedException();
        }

        public void interepretCmdList(string templateNodeInnerValue)
        {
            throw new NotImplementedException();
        }
    }

    public class MyServFacade
    {
        public SIProlog prologEngine;
    }

    public class MyBehFacade
    {
        public EventQueueFacade eventQueue;

        public void queueEvent(string onFail)
        {
            throw new NotImplementedException();
        }

        public void runBTXML(XmlNode templateNode)
        {
            throw new NotImplementedException();
        }

        public void defineSubAIML(XmlNode templateNode)
        {
            throw new NotImplementedException();
        }

        public static string FixXmlEnitites(string message)
        {
            throw new NotImplementedException();
        }

        public void satisfyDrive(string driveName)
        {
            throw new NotImplementedException();
        }

        public void keepTime(string lastchatoutput, object success)
        {
            throw new NotImplementedException();
        }

        public void activationTime(string lastchatoutput, object success)
        {
            throw new NotImplementedException();
        }
    }

    /// <summary>
    /// Values that can be returned from composites and the like.
    /// </summary> 
    public enum RunStatusLeg
    {
        Non,
        Success,
        Failure,
        Running,
    }
    public enum ThresholdLeg
    {
        Positive,
        Negative,
    }


    public class EventQueueFacade
    {
        public bool Contains(string myBehavior)
        {
            throw new NotImplementedException();
        }
    }
    */
    [Flags]
    public enum RequestKind
    {
        NaturalLang = 1,
        EventLang = 2,
        CommentLang = 4,
        Realtime = 8,
        ForString = 16,
        Process = 32,
        TagHandler = 64,
        InnerDialog = 128,
        TemplateExpander = 256,
        ForLoader = 512,
        SubProcess = 1024,
        BackgroundThread = 2048,
        MTalk = 4096,
        FSM = 8192,
        BTX = 16384,

        AIMLLoader = Process | ForLoader,
        ChatRealTime = NaturalLang | Realtime,
        ChatForString = NaturalLang | ForString,
        InnerSelfTalk = ChatForString | InnerDialog,
        EventProcessor = Process | EventLang,
        BotPropertyEval = CommentLang | Process,
        PushPopTag = NaturalLang | TagHandler,
        SraiTag = NaturalLang | TagHandler | SubProcess,
        BehaviourChat = ChatRealTime | BackgroundThread | BTX,
        MTalkThread = ChatRealTime | BackgroundThread | MTalk,
        StateMachineProcess = TemplateExpander | FSM | Process,
        BehaviourProcess = TemplateExpander | BTX | Realtime | Process,
        EvalAIMLHandler = ForString | EventLang | Process | SubProcess,
        CommandAndChatProcessor = EvalAIMLHandler | ChatRealTime | BackgroundThread | Process | ForString | EventLang | Process | SubProcess,
    }
    /// <summary>
    /// Encapsulates a bot. If no settings.xml file is found or referenced the bot will try to
    /// default to safe settings.
    /// </summary>
    /// 
    public delegate void sayProcessorDelegate(string message);
    public delegate void systemProcessorDelegate(string message);
    public delegate void systemPersonaDelegate(string message);

    public class myConst
    {
        public static string MEMHOST = "127.0.0.1";
        //public static string MEMHOST = "192.168.2.141";
    }
}
