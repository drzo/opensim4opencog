using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Brainiac.Design.Attributes;
using DaxPlugin;
using Brainiac.Design;

namespace DaxPlugin.Nodes
{

    public enum ServerType {wolframserver,trueknowledgeserver,refserver,pannouserver }
    #region prototypes
    public class DaxAction : Brainiac.Design.Nodes.Action
    {
        protected string _id = "";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }
        protected float _weight = 0.5f;

        [DesignerFloat("weight",
                    "weight for weighted random selection (0 to 1)",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 1000000, (float)0.1, 3,
                    "UnitsMilliseconds")]

        public float weight
        {
            get { return _weight; }
            set { _weight = value; }
        }
        public DaxAction(string name, string description)
            : base(name, description)
        {
        }

    }
    public class DaxSelect : Brainiac.Design.Nodes.Selector
    {
        protected string _id = "";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }
        protected float _weight = 0.5f;

        [DesignerFloat("weight",
                    "weight for weighted random selection (0 to 1)",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 1000000, (float)0.1, 3,
                    "UnitsMilliseconds")]

        public float weight
        {
            get { return _weight; }
            set { _weight = value; }
        }
        public DaxSelect(string name, string description)
            : base(name, description)
        {
        }

    }
    public class DaxValence : Brainiac.Design.Nodes.Action
    {
        protected string _name = "*";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _name; }
            set { _name = value; }
        }
        protected int _halflife = 60000;

        [DesignerInteger("halflife",
                    "halflife of valence in Milliseconds",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 1000000, 1,
                    "UnitsMilliseconds")]

        public int halflife
        {
            get { return _halflife; }
            set { _halflife = value; }
        }

        protected float _threshold = 0.5f;

        [DesignerFloat("threshold",
                    "threshold of valence usually (0 to 1)",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 1000000, (float)0.1,3,
                    "UnitsMilliseconds")]

        public float threshold
        {
            get { return _threshold; }
            set { _threshold = value; }
        }

        protected float _weight = 0.5f;

        [DesignerFloat("weight",
                    "weight for weighted random selection (0 to 1)",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 1000000, (float)0.1, 3,
                    "UnitsMilliseconds")]

        public float weight
        {
            get { return _weight; }
            set { _weight = value; }
        }

        public DaxValence(string name, string description)
            : base(name, description)
        {
        }

    }

    public class DaxParNode : Brainiac.Design.Nodes.Parallel
    {
        protected string _id = "";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }
        public DaxParNode(string name, string description)
            : base(name, description)
        {
        }

    }

    public class DaxBehave : Brainiac.Design.Nodes.Parallel
    {
        protected string _id = "*";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }

        protected int _pace = 1000;

        [DesignerInteger("pace",
                    "delay between steps",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 60000, 1,
                    "UnitsMilliseconds")]

        public int pace
        {
            get { return _pace; }
            set { _pace = value; }
        }
        
        protected string _onchat = "*";

        [DesignerString("onchat",
                    "onchat behavior",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string onchat
        {
            get { return _onchat; }
            set { _onchat = value; }
        }


        protected string _onrestore = "*";

        [DesignerString("onrestore",
                    "onrestore behavior",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string onrestore
        {
            get { return _onrestore; }
            set { _onrestore = value; }
        }

        protected string _onabort = "*";

        [DesignerString("onabort",
                    "onabort behavior",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string onabort
        {
            get { return _onabort; }
            set { _onabort = value; }
        }

        protected string _onsuccess = "*";

        [DesignerString("onsuccess",
                    "onsuccess behavior",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string onsuccess
        {
            get { return _onsuccess; }
            set { _onsuccess = value; }
        }

        protected string _onfail = "*";

        [DesignerString("onfail",
                    "onfail behavior",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string onfail
        {
            get { return _onfail; }
            set { _onfail = value; }
        }

        public DaxBehave(string name,string description)
            : base(name, description)
        {
        }

    }
    #endregion

    #region Actions
    public class DaxNode:Brainiac.Design.Nodes.Action 
    {
		protected int _exampleProperty= 5;

		[DesignerInteger("Example Property", 
                    "Example property description", 
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 1, 10, 1, 
                    "UnitsMeters")]
		public int ExampleProperty
		{
			get { return _exampleProperty; }
			set { _exampleProperty= value; }
		}

		protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
		{
			base.CloneProperties(newnode);

			DaxNode node= (DaxNode)newnode;
			node._exampleProperty= _exampleProperty;
		}

        public DaxNode()
            : base("Dax Test Node", "Dax initial test node.")
		{
		}
    }

    public class DaxSubbehavior : Brainiac.Design.Nodes.Action 
    {
        protected string _id = "*";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxSubbehavior node = (DaxSubbehavior)newnode;
        }

        public DaxSubbehavior()
            : base("subbehavior", "Call another behavior")
        {
        }
    }

    public class DaxDrive : DaxValence
    {


        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxDrive node = (DaxDrive)newnode;
        }

        public DaxDrive()
            : base("drive", "trigger when valence BELOW threshold")
        {
        }
    }
    public class DaxMotive : DaxValence
    {


        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxMotive node = (DaxMotive)newnode;
        }

        public DaxMotive()
            : base("motive", "trigger when valence ABOVE threshold")
        {
        }
    }
    public class DaxInhibit : DaxAction
    {


        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxInhibit node = (DaxInhibit)newnode;
        }

        public DaxInhibit()
            : base("inhibit", "set valence to zero")
        {
        }
    }
    public class DaxRelease : DaxAction
    {


        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxRelease node = (DaxRelease)newnode;
        }

        public DaxRelease()
            : base("release", "set valence to 1")
        {
        }
    }
    public class DaxStarttimer : DaxAction
    {


        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxStarttimer node = (DaxStarttimer)newnode;
        }

        public DaxStarttimer()
            : base("starttimer", "starts a timer")
        {
        }
    }
    public class DaxStoptimer : DaxAction
    {


        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxStoptimer node = (DaxStoptimer)newnode;
        }

        public DaxStoptimer()
            : base("stoptimer", "stops a timer")
        {
        }
    }
    public class DaxTaskguest : DaxAction
    {
        protected string _content = "*";

        [DesignerString("content",
                    "the paremeters to send to the call ",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }
        protected string _call = "*";

        [DesignerString("call",
                    "the function to call",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string call
        {
            get { return _call; }
            set { _call = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxTaskguest node = (DaxTaskguest)newnode;
        }

        public DaxTaskguest()
            : base("taskguest", "call the guest evaluator")
        {
        }
    }
    public class DaxBreaker : Brainiac.Design.Nodes.Action
    {

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxBreaker node = (DaxBreaker)newnode;
        }

        public DaxBreaker()
            : base("breaker", "self interrupt a dialog chain")
        {
        }
    }

    #endregion

    #region Parallel
    public class DaxBehavior : DaxBehave 
    {
        protected string _id = "*";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxBehavior node = (DaxBehavior)newnode;
        }

        public DaxBehavior()
            : base("Behavior", "Named processor of children")
        {
        }
    }
    public class DaxRbehavior : DaxBehave
    {
        protected string _id = "*";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxRbehavior node = (DaxRbehavior)newnode;
        }

        public DaxRbehavior()
            : base("rbehavior", "Named random processor of children")
        {
        }
    }

    public class DaxParallel : DaxParNode
    {
        public DaxParallel()
            : base("parallel", "Try ALL children.")
        {
        }
         protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxParallel node = (DaxParallel)newnode;
        }
    }
    public class DaxSequence : Brainiac.Design.Nodes.Sequence 
    {
        protected string _id = "";

        [DesignerString("id",
                    "Behavior identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }
        protected float _weight = 0.5f;

        [DesignerFloat("weight",
                    "weight for weighted random selection (0 to 1)",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 1000000, (float)0.1, 3,
                    "UnitsMilliseconds")]

        public float weight
        {
            get { return _weight; }
            set { _weight = value; }
        }
        public DaxSequence()
            : base("sequence", "Try ALL children until one fails.")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxSequence node = (DaxSequence)newnode;
        }
    }
    public class DaxSubaiml : DaxParNode
    {
        protected string _graph = "*";

        [DesignerString("graph",
                    "AIML graph Chat should occur in",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string graph
        {
            get { return _graph; }
            set { _graph = value; }
        }
        public DaxSubaiml()
            : base("subaiml", "context specific aiml.")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxParallel node = (DaxParallel)newnode;
        }
    }
    public class DaxState : DaxParNode
    {
        protected string _name = "*";

        [DesignerString("name",
                    "name of state",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string name
        {
            get { return _name; }
            set { _name = value; }
        }
        public DaxState()
            : base("state", "wrapping state")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxState node = (DaxState)newnode;
        }
    }
    public class DaxTopic : DaxParNode
    {
        protected string _name = "*";

        [DesignerString("name",
                    "name of state",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string graph
        {
            get { return _name; }
            set { _name = value; }
        }
        public DaxTopic()
            : base("topic", "wrapping topic")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxTopic node = (DaxTopic)newnode;
        }
    }

    #endregion 

    #region Selector
    public class DaxSelector : DaxSelect
    {
        public DaxSelector()
            : base("selector", "Try children until true.")
        {
        }
         protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxSelector node = (DaxSelector)newnode;
        }
   }
    public class DaxRandom : DaxSelect
    {
        public DaxRandom()
            : base("random", "Pick a child at random.")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxRandom node = (DaxRandom)newnode;
        }
    }
    public class DaxWeighted : DaxSelect
    {
        public DaxWeighted()
            : base("weighted", "Pick a child using weighted random selection.")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxWeighted node = (DaxWeighted)newnode;
        }
    }

    #endregion

    #region Condition
    public class DaxAssert : Brainiac.Design.Nodes.Condition
    {
        protected string _condition = "*";

        [DesignerString("cond",
                    "Condition that must be true to continue",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string cond
        {
            get { return _condition; }
            set { _condition = value; }
        }
        public DaxAssert()
            : base("assert", "Precondition Check.")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxAssert node = (DaxAssert)newnode;
        }
    }
    public class DaxAsserttimer : Brainiac.Design.Nodes.Condition
    {
        protected string _id = "";

        [DesignerString("id",
                    "timer identifier",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string id
        {
            get { return _id; }
            set { _id = value; }
        }
        protected string _condition = "*";

        [DesignerString("cond",
                    "Condition that must be true to continue",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string cond
        {
            get { return _condition; }
            set { _condition = value; }
        }
        public DaxAsserttimer()
            : base("asserttimer", "starts timer(id), and checks the condition")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxAsserttimer node = (DaxAsserttimer)newnode;
        }
    }
    public class DaxAssertguest : Brainiac.Design.Nodes.Condition
    {
        protected string _condition = "*";

        [DesignerString("cond",
                    "Guest function to call",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string cond
        {
            get { return _condition; }
            set { _condition = value; }
        }
        protected string _content = "*";

        [DesignerString("content",
                    "parameters to call guest function with",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxAssertguest node = (DaxAssertguest)newnode;
        }
         public DaxAssertguest()
            : base("assertguest", "checks truth of 'guest.(condition)(content)'")
        {
        }
   }
    #endregion

    #region Decorator
    public class DaxTask : Brainiac.Design.Nodes.Decorator
    {
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxTask node = (DaxTask)newnode;
        }
        public DaxTask()
            : base("task", "AIML template code")
        {
        }
    }
    public class DaxLoop : Brainiac.Design.Nodes.Decorator
    {
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxLoop node = (DaxLoop)newnode;
        }
        public DaxLoop()
            : base("loop", "loop")
        {
        }
    }
    public class DaxLoopuntil : Brainiac.Design.Nodes.Decorator
    {
        protected int _maxloop = 10;

        [DesignerInteger("pace",
                    "delay between steps",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 60000, 1,
                    "UnitsLoops")]

        public int maxloop
        {
            get { return _maxloop; }
            set { _maxloop = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxLoopuntil node = (DaxLoopuntil)newnode;
        }
        public DaxLoopuntil()
            : base("loopuntil", "loop maxloop times")
        {
        }
    }


    public class DaxSapi : Brainiac.Design.Nodes.Sequence
    {
        protected string _content = "";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxSapi node = (DaxSapi)newnode;
        }
        public DaxSapi()
            : base("sapi", "AIML template code for SAPI code")
        {
        }
    }
    public class DaxBookmark : Brainiac.Design.Nodes.Decorator
    {
        protected string _mark = "";

        [DesignerString("mark",
                    "mark",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string mark
        {
            get { return _mark; }
            set { _mark = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxBookmark node = (DaxBookmark)newnode;
        }
        public DaxBookmark()
            : base("bookmark", "SAPI code for animation")
        {
        }
    }
    public class DaxSilence : Brainiac.Design.Nodes.Decorator
    {
        protected int _msec = 10;

        [DesignerInteger("msec",
                    "delay in milliseconds",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags, 0, 60000, 1,
                    "UnitsMilliseconds")]

        public int msec
        {
            get { return _msec; }
            set { _msec = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxSilence node = (DaxSilence)newnode;
        }
        public DaxSilence()
            : base("silence", "SAPI code for silence")
        {
        }
    }

    #endregion

    #region SATKB
    public class DaxTellkb : DaxAction
    {
        protected string _content = "*";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxTellkb node = (DaxTellkb)newnode;
        }

        public DaxTellkb()
            : base("tellkb", "assert sentence to KB")
        {
        }
    }
    public class DaxTellbasekb : DaxAction
    {
        protected string _content = "*";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxTellbasekb node = (DaxTellbasekb)newnode;
        }

        public DaxTellbasekb()
            : base("tellbasekb", "assert sentence to BaseKB")
        {
        }
    }
    public class DaxTellkbocc : DaxAction
    {
        protected string _content = "*";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxTellkbocc node = (DaxTellkbocc)newnode;
        }

        public DaxTellkbocc()
            : base("tellkbocc", "assert OCC emotion about content logic to KB")
        {
        }
    }
    public class DaxClearkb : DaxAction
    {
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxClearkb node = (DaxClearkb)newnode;
        }

        public DaxClearkb()
            : base("clearkb", "clear the KB")
        {
        }
    }
    public class DaxClearbasekb : DaxAction
    {
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxClearbasekb node = (DaxClearbasekb)newnode;
        }

        public DaxClearbasekb()
            : base("clearbasekb", "clear the BaseKB")
        {
        }
    }
    public class DaxProcesskb : DaxAction
    {
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxProcesskb node = (DaxProcesskb)newnode;
        }

        public DaxProcesskb()
            : base("processkb", "process the KB")
        {
        }
    }
    #endregion

    #region AIML


    public class DaxCategory : Brainiac.Design.Nodes.Selector
    {
        public DaxCategory()
            : base("category", "AIML category.")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxCategory node = (DaxCategory)newnode;
        }
    }
    public class DaxThat : Brainiac.Design.Nodes.Condition
    {
        protected string _content = "*";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxThat node = (DaxThat)newnode;
        }

        public DaxThat()
            : base("that", "AIML that")
        {
        }
    }
    public class DaxPattern : Brainiac.Design.Nodes.Condition
    {
        protected string _content = "*";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxPattern node = (DaxPattern)newnode;
        }

        public DaxPattern()
            : base("pattern", "AIML pattern")
        {
        }
    }
    public class DaxTemplate : Brainiac.Design.Nodes.Selector
    {
        public DaxTemplate()
            : base("template", "AIML template.")
        {
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxTemplate node = (DaxTemplate)newnode;
        }
    }
    public class DaxSay : Brainiac.Design.Nodes.Sequence
    {
        protected string _content = "";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }
        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxSay node = (DaxSay)newnode;
        }
        public DaxSay()
            : base("say", "AIML template side: say")
        {
        }
    }
    public class DaxRemoteserver : DaxAction
    {
        protected ServerType _type;

        [DesignerEnum("ServerType", "RemoteServerType", "CategoryBasic", DesignerProperty.DisplayMode.Parameter, 0, DesignerProperty.DesignerFlags.NoFlags, null)]
        public ServerType servertype
        {
            get { return _type; }
            set { _type = value; }
        }
        protected string _content = "";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }
        protected string _url = "*";

        [DesignerString("url",
                    "server url",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string url
        {
            get { return _url; }
            set { _url = value; }
        }
        protected string _onfail = "";

        [DesignerString("onfail",
                    "behavior if no response",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string onfail
        {
            get { return _onfail; }
            set { _onfail = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxRemoteserver node = (DaxRemoteserver)newnode;
        }

        public DaxRemoteserver()
            : base("remoteserver", "Access Remote Answer Server")
        {
        }
    }
    public class DaxFlushqueue : DaxAction
    {

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxFlushqueue node = (DaxFlushqueue)newnode;
        }

        public DaxFlushqueue()
            : base("flushqueue", "Clear Event Stack and Queue.")
        {
        }
    }
    public class DaxEnqueue : DaxAction
    {

        protected string _content = "";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxEnqueue node = (DaxEnqueue)newnode;
        }

        public DaxEnqueue()
            : base("enqueue", "Add term to queue")
        {
        }
    }
    public class DaxChat : Brainiac.Design.Nodes.Action
    {
        protected string _graph = "*";

        [DesignerString("graph",
                    "AIML graph Chat should occur in",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string graph
        {
            get { return _graph; }
            set { _graph = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxChat node = (DaxChat)newnode;
        }

        public DaxChat()
            : base("chat", "Process verbal input using AIML")
        {
        }
    }
    public class DaxLi : Brainiac.Design.Nodes.Action
    {
        protected string _content = "*";

        [DesignerString("content",
                    "content",
                    "Basic",
                    DesignerProperty.DisplayMode.List, 0, DesignerProperty.DesignerFlags.NoFlags)]

        public string content
        {
            get { return _content; }
            set { _content = value; }
        }

        protected override void CloneProperties(Brainiac.Design.Nodes.Node newnode)
        {
            base.CloneProperties(newnode);

            DaxLi node = (DaxLi)newnode;
        }

        public DaxLi()
            : base("li", "AIML template side: li (random choice)")
        {
        }
    }


    #endregion

}
