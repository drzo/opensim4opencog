using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Brainiac.Design;

namespace DaxPlugin
{
    public class DaxPlugin:Brainiac .Design .Plugin 
    {
        public DaxPlugin()
        {
            // register all the file managers
            _fileManagers.Add(new FileManagerInfo(typeof(Brainiac.Design.FileManagers.FileManagerXML),
                "Behaviour XML (*.xml)|*.xml", ".xml"));


            NodeGroup conditions = new NodeGroup("Conditions", NodeIcon.Condition, null);
            _nodeGroups.Add(conditions);
            NodeGroup selectors = new NodeGroup("Selectors", NodeIcon.Selector, null);
            _nodeGroups.Add(selectors);
            NodeGroup parallel = new NodeGroup("Parallel", NodeIcon.Parallel, null);
            _nodeGroups.Add(parallel);
            NodeGroup decorators = new NodeGroup("Decorators", NodeIcon.Decorator , null);
            _nodeGroups.Add(decorators);
            NodeGroup actions = new NodeGroup("Actions", NodeIcon.Action, null);
            _nodeGroups.Add(actions);

            NodeGroup aiml = new NodeGroup("AIML", NodeIcon.Action, null);
            _nodeGroups.Add(aiml);
            NodeGroup valence = new NodeGroup("Valence", NodeIcon.Impulse, null);
            _nodeGroups.Add(valence);
            NodeGroup sapi = new NodeGroup("Sapi", NodeIcon.Decorator, null);
            _nodeGroups.Add(sapi);

            actions.Items.Add(typeof(Nodes.DaxNode));
            actions.Items.Add(typeof(Nodes.DaxFlushqueue));
            actions.Items.Add(typeof(Nodes.DaxEnqueue));
            actions.Items.Add(typeof(Nodes.DaxChat));
            actions.Items.Add(typeof(Nodes.DaxSubbehavior));
            actions.Items.Add(typeof(Nodes.DaxStarttimer));
            actions.Items.Add(typeof(Nodes.DaxStoptimer));

            actions.Items.Add(typeof(Nodes.DaxTellkb));
            actions.Items.Add(typeof(Nodes.DaxTellbasekb));
            actions.Items.Add(typeof(Nodes.DaxTellkbocc));
            actions.Items.Add(typeof(Nodes.DaxClearkb));
            actions.Items.Add(typeof(Nodes.DaxClearbasekb));
            actions.Items.Add(typeof(Nodes.DaxProcesskb));
            actions.Items.Add(typeof(Nodes.DaxTaskguest));
            actions.Items.Add(typeof(Nodes.DaxBreaker));

            selectors.Items.Add(typeof(Nodes.DaxSelector));
            selectors.Items.Add(typeof(Nodes.DaxWeighted));
            selectors.Items.Add(typeof(Nodes.DaxRandom ));

            parallel.Items.Add(typeof(Nodes.DaxBehavior));
            parallel.Items.Add(typeof(Nodes.DaxRbehavior));
            parallel.Items.Add(typeof(Nodes.DaxParallel));
            parallel.Items.Add(typeof(Nodes.DaxSequence));
 
            decorators.Items.Add(typeof(Nodes.DaxLoop));
            decorators.Items.Add(typeof(Nodes.DaxLoopuntil));
            decorators.Items.Add(typeof(Nodes.DaxSubaiml));
            

            conditions.Items.Add(typeof(Nodes.DaxAssert));
            conditions.Items.Add(typeof(Nodes.DaxAsserttimer));
            conditions.Items.Add(typeof(Nodes.DaxAssertguest));

            valence.Items.Add(typeof(Nodes.DaxDrive));
            valence.Items.Add(typeof(Nodes.DaxMotive));
            valence.Items.Add(typeof(Nodes.DaxInhibit));
            valence.Items.Add(typeof(Nodes.DaxRelease));

            aiml.Items.Add(typeof(Nodes.DaxCategory));
            aiml.Items.Add(typeof(Nodes.DaxPattern));
            aiml.Items.Add(typeof(Nodes.DaxThat));
            aiml.Items.Add(typeof(Nodes.DaxTemplate));
            aiml.Items.Add(typeof(Nodes.DaxLi));
            aiml.Items.Add(typeof(Nodes.DaxTask));
            aiml.Items.Add(typeof(Nodes.DaxSay));
            aiml.Items.Add(typeof(Nodes.DaxRemoteserver));
            aiml.Items.Add(typeof(Nodes.DaxState));
            aiml.Items.Add(typeof(Nodes.DaxTopic));

            sapi.Items.Add(typeof(Nodes.DaxSapi));
            sapi.Items.Add(typeof(Nodes.DaxBookmark));
            sapi.Items.Add(typeof(Nodes.DaxSilence));

            // register all the file managers
            _fileManagers.Add(new FileManagerInfo(typeof(DaxImporter), "Dax BTX (*.btx)|*.btx", ".btx",true));
            // register all the exporters
            _exporters.Add(new ExporterInfo(typeof(DaxExporter), "Dax Behavior XML", true, "BTX",true));
        }
    }
}
