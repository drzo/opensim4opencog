#undef SILVERLIGHT
using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Runtime.Serialization;
using System.Xml;
using System.Xml.Schema;
using System.Xml.Serialization;
using TermList = LogicalParticleFilter1.SIProlog.PartListImpl;
using PartList = LogicalParticleFilter1.SIProlog.PartListImpl;

using System.Threading;
//using ProveResult = LogicalParticleFilter1.SIProlog.PEnv;

namespace LogicalParticleFilter1
{
    // ReSharper disable FieldCanBeMadeReadOnly.Local
    public partial class SIProlog
    {

        /// <summary>
        /// Class for representing Prolog Microtheories
        /// </summary>
        /// <threadsafety instance="false">Safe for multi-threaded read-only access but unsafe if one/more threads may modify the PrologKB by using the <see cref="LogicalParticleFilter1.SIProlog.PNode.Assert">Assert</see>, <see cref="LogicalParticleFilter1.SIProlog.PNode.Retract">Retract</see> or <see cref="BasePrologMt.Merge">Merge</see> methods</threadsafety>
#if !SILVERLIGHT
        [Serializable, XmlRoot(ElementName = "prologmt")]
#endif
        public class PrologMT
#if !SILVERLIGHT
            :
                ISerializable
#endif
        {
            #region Variables

            /// <summary>
            /// Collection of Rules in the PrologKB (this are maintained from the PDB though)
            /// </summary>
            protected RuleList _rules;

            protected PDB _rule_holder;

            /// <summary>
            /// Namespace Mapper
            /// </summary>
            //   protected NamespaceMapper _nsmapper;

            /// <summary>
            /// Base String of the PrologKB
            /// </summary>
            protected String _baseuri = null;

            /// <summary>
            /// Blank Node ID Mapper
            /// </summary>
            //  protected BlankNodeMapper _bnodemapper;

            private RuleEventHandler RuleAddedHandler, RuleRemovedHandler;

#if !SILVERLIGHT
            // private PrologMtDeserializationInfo _dsInfo;
#endif

            #endregion

            #region Constructor

            /// <summary>
            /// Creates a new Base PrologKB using the given Rule Collection
            /// </summary>
            /// <param name="ruleCollection">Rule Collection to use</param>
            protected PrologMT(RuleList ruleCollection)
            {
                RegisterRuleList(ruleCollection.syncPDB, ruleCollection);
            }

            /// <summary>
            /// Create Event Handlers and attach to the Rule Collection
            /// </summary>
            /// <param name="ruleCollection"></param>
            public void RegisterRuleList(PDB holder, RuleList ruleCollection)
            {
                this._rule_holder = holder;
                this._rules = ruleCollection;
                this.RuleAddedHandler = new RuleEventHandler(this.OnRuleAsserted);
                this.RuleRemovedHandler = new RuleEventHandler(this.OnRuleRetracted);
                this.AttachEventHandlers(this._rules);
            }

            #region Constructor

            /// <summary>
            /// Creates a new instance of a PrologKB
            /// </summary>
            public PrologMT()
                : base()
            {
            }


            /// <summary>
            /// Creates a new instance of a PrologKB using the given Rule Collection and an optionally empty Namespace Map
            /// </summary>
            /// <param name="ruleCollection">Rule Collection</param>
            /// <param name="isreadonly">Whether the Map should be isreadonly</param>
            public PrologMT(RuleList ruleCollection, bool isreadonly)
                : this(ruleCollection)
            {
            }


#if !SILVERLIGHT
            /// <summary>
            /// Deserialization Constructor
            /// Creates a PrologKB from the given Serialization Information
            /// </summary>
            /// <param name="info">Serialization Information</param>
            /// <param name="context">Streaming Context</param>
            protected PrologMT(SerializationInfo info, StreamingContext context)
                : this()
            {
                //   this._dsInfo = new PrologMtDeserializationInfo(info, context);
            }


            [OnDeserialized]
            private void OnDeserialized(StreamingContext context)
            {
                ///if (this._dsInfo != null) this._dsInfo.Apply(this);
            }
#endif

            #endregion

            #region Properties

            /// <summary>
            /// Gets the set of Rules described in this PrologKB
            /// </summary>
            public virtual RuleList Rules
            {
                get
                {
                    if (_rule_holder != null)
                    {
                        return _rule_holder._rules;
                    }
                    return this._rules;
                }
            }


            /// <summary>
            /// Gets the current Base String for the PrologKB
            /// </summary>
            /// <remarks>
            /// This value may be changed during PrologKB population depending on whether the Concrete syntax allows the Base String to be changed and how the Parser handles this
            /// </remarks>
            public virtual String BaseStartMt
            {
                get { return this._baseuri; }
                set
                {
                    //NamespaceMapper.NamespaceCheck(value);
                    this._baseuri = value;
                }
            }

            public override string ToString()
            {
                return base.ToString() + " base=" + BaseStartMt + " tc=" + this._rules.Count + " tci=" + Rules.Count +
                       " prefixes=" + 666; ///NamespaceMap;
            }

            /// <summary>
            /// Gets whether a PrologKB is Empty ie. Contains No Rules or Nodes
            /// </summary>
            public virtual bool IsEmpty
            {
                get { return (this.Rules.Count == 0); }
            }

            #endregion


            #region Rule Assertion & Retraction

            /// <summary>
            /// Asserts a Rule in the PrologKB
            /// </summary>
            /// <param name="t">The Rule to add to the PrologKB</param>
            public virtual bool Assert(Rule t)
            {
                //Add to Rules Collection
                if (this._rules.Add(t))
                {
                    this.RaiseRuleAsserted(t);
                    return true;
                }
                return false;
            }

            /// <summary>
            /// Asserts a List of Rules in the prologmt
            /// </summary>
            /// <param name="ts">List of Rules in the form of an IEnumerable</param>
            public virtual bool Assert(IEnumerable<Rule> ts)
            {
                bool asserted = false;
                foreach (Rule t in ts)
                {
                    asserted = this.Assert(t) || asserted;
                }
                return asserted;
            }

            /// <summary>
            /// Retracts a Rule from the PrologKB
            /// </summary>
            /// <param name="t">Rule to Retract</param>
            /// <remarks>Current implementation may have some defunct Nodes left in the PrologKB as only the Rule is retracted</remarks>
            public virtual bool Retract(Rule t)
            {
                if (this._rules.Delete(t))
                {
                    this.RaiseRuleRetracted(t);
                    return true;
                }
                return false;
            }

            /// <summary>
            /// Retracts a enumeration of Rules from the prologmt
            /// </summary>
            /// <param name="ts">Enumeration of Rules to retract</param>
            public virtual bool Retract(IEnumerable<Rule> ts)
            {
                bool retracted = false;
                foreach (Rule t in ts)
                {
                    retracted = this.Retract(t) || retracted;
                }
                return retracted;
            }

            #endregion

            /// <summary>
            /// Clears all Rules from the PrologKB
            /// </summary>
            /// <remarks>
            /// <para>
            /// The PrologKB will raise the <see cref="ClearRequested">ClearRequested</see> event at the start of the Clear operation which allows for aborting the operation if the operation is cancelled by an event handler.  On completing the Clear the <see cref="Cleared">Cleared</see> event will be raised.
            /// </para>
            /// </remarks>
            public virtual void Clear()
            {
                if (!this.RaiseClearRequested()) return;

                this.Retract(this.Rules.ToList());

                this.RaiseCleared();
            }

            /// <summary>
            /// Gets whether a given Rule exists in this PrologKB
            /// </summary>
            /// <param name="t">Rule to test</param>
            /// <returns></returns>
            public virtual bool ContainsRule(Rule t)
            {
                return this._rules.Contains(t);
            }


            #region PrologKB Merging

            /// <summary>
            /// Merges another PrologKB into the current PrologKB
            /// </summary>
            /// <param name="g">PrologKB to Merge into this PrologKB</param>
            /// <remarks>The PrologKB on which you invoke this method will preserve its Blank Node IDs while the Blank Nodes from the PrologKB being merged in will be given new IDs as required in the scope of this PrologKB.</remarks>
            public virtual void Merge(PNode g)
            {
                this.Merge(g, false);
            }

            /// <summary>
            /// Merges another PrologKB into the current PrologKB
            /// </summary>
            /// <param name="g">PrologKB to Merge into this PrologKB</param>
            /// <param name="keepOriginalPrologMtStartMt">Indicates that the Merge should preserve the PrologKB URIs of Nodes so they refer to the PrologKB they originated in</param>
            /// <remarks>
            /// <para>
            /// The PrologKB on which you invoke this method will preserve its Blank Node IDs while the Blank Nodes from the PrologKB being merged in will be given new IDs as required in the scope of this PrologKB.
            /// </para>
            /// <para>
            /// The PrologKB will raise the <see cref="MergeRequested">MergeRequested</see> event before the Merge operation which gives any event handlers the oppurtunity to cancel this event.  When the Merge operation is completed the <see cref="Merged">Merged</see> event is raised
            /// </para>
            /// </remarks>
            public virtual void Merge(PNode g, bool keepOriginalPrologMtStartMt)
            {
                if (ReferenceEquals(this, g))
                    throw new NullReferenceException("You cannot Merge an RDF PrologKB with itself");

                //Check that the merge can go ahead
                if (!this.RaiseMergeRequested()) return;

                this.RaiseMerged();
            }

            #endregion

            #region PrologKB Equality

            /// <summary>
            /// Determines whether a PrologKB is equal to another Object
            /// </summary>
            /// <param name="obj">Object to test</param>
            /// <returns></returns>
            /// <remarks>
            /// <para>
            /// A PrologKB can only be equal to another Object which is an <see cref="LogicalParticleFilter1.SIProlog.PNode">PNode</see>
            /// </para>
            /// <para>
            /// PrologKB Equality is determined by a somewhat complex algorithm which is explained in the remarks of the other overload for Equals
            /// </para>
            /// </remarks>
            public override bool Equals(object obj)
            {
                //PrologMts can't be equal to null
                if (obj == null) return false;

                if (obj is PNode)
                {
                    PNode g = (PNode) obj;

                    Dictionary<Part, Part> temp;
                    return this.Equals(g, out temp);
                }
                else
                {
                    //PrologMts can only be equal to other PrologMts
                    return false;
                }
            }

            public override int GetHashCode()
            {
                return this.BaseStartMt.ToLower().GetHashCode();
            }

            /// <summary>
            /// Determines whether this PrologKB is equal to the given PrologKB
            /// </summary>
            /// <param name="g">PrologKB to test for equality</param>
            /// <param name="mapping">Mapping of Blank Nodes iff the PrologMts are equal and contain some Blank Nodes</param>
            /// <returns></returns>
            /// <remarks>
            /// <para>
            /// The algorithm used to determine PrologKB equality is based in part on a Iterative Vertex Classification Algorithm described in a Technical Report from HP by Jeremy J Carroll - <a href="http://www.hpl.hp.com/techreports/2001/HPL-2001-293.html">Matching RDF PrologMts</a>
            /// </para>
            /// <para>
            /// PrologKB Equality is determined according to the following algorithm:
            /// </para>
            /// <ol>
            /// <li>If the given PrologKB is null PrologMts are not equal</li>
            /// <li>If the given PrologKB is this PrologKB (as determined by Reference Equality) then PrologMts are equal</li>
            /// <li>If the PrologMts have a different number of Rules they are not equal</li>
            /// <li>Declare a list of Rules which are the Rules of the given PrologKB called <em>OtherRules</em></li>
            /// <li>Declare two dictionaries of Nodes to Integers which are called <em>LocalClassification</em> and <em>OtherClassification</em></li>
            /// <li>For Each Rule in this PrologKB
            ///     <ol>
            ///     <li>If it is a Ground Rule and cannot be found and removed from <em>OtherRules</em> then PrologMts are not equal since the Rule does not exist in both PrologMts</li>
            ///     <li>If it contains Blank Nodes track the number of usages of this Blank Node in <em>LocalClassification</em></li>
            ///     </ol>
            /// </li> 
            /// <li>If there are any Rules remaining in <em>OtherRules</em> which are Ground Rules then PrologMts are not equal since this PrologKB does not contain them</li>
            /// <li>If all the Rules from both PrologMts were Ground Rules and there were no Blank Nodes then the PrologMts are equal</li>
            /// <li>Iterate over the remaining Rules in <em>OtherRules</em> and populate the <em>OtherClassification</em></li>
            /// <li>If the count of the two classifications is different the PrologMts are not equal since there are differing numbers of Blank Nodes in the PrologKB</li>
            /// <li>Now build two additional dictionaries of Integers to Integers which are called <em>LocalDegreeClassification</em> and <em>OtherDegreeClassification</em>.  Iterate over <em>LocalClassification</em> and <em>OtherClassification</em> such that the corresponding degree classifications contain a mapping of the number of Blank Nodes with a given degree</li>
            /// <li>If the count of the two degree classifications is different the PrologMts are not equal since there are not the same range of Blank Node degrees in both PrologMts</li>
            /// <li>For All classifications in <em>LocalDegreeClassification</em> there must be a matching classification in <em>OtherDegreeClassification</em> else the PrologMts are not equal</li>
            /// <li>Then build a possible mapping using the following rules:
            ///     <ol>
            ///     <li>Any Blank Node used only once should be mapped to an equivalent Blank Node in the other PrologKB.  If this is not possible then the PrologMts are not equal</li>
            ///     <li>Any Blank Node with a unique degree should be mapped to an equivalent Blank Node in the other PrologKB.  If this is not possible then the PrologMts are not equal</li>
            ///     <li>Keep a copy of the mapping up to this point as a Base Mapping for use as a fallback in later steps</li>
            ///     <li>Build up lists of dependent pairs of Blank Nodes for both PrologMts</li>
            ///     <li>Use these lists to determine if there are any independent nodes not yet mapped.  These should be mapped to equivalent Blank Nodes in the other PrologKB, if this is not possible the PrologMts are not equal</li>
            ///     <li>Use the Dependencies and existing mappings to generate a possible mapping</li>
            ///     <li>If a Complete Possible Mapping (there is a Mapping for each Blank Node from this PrologKB to the Other PrologKB) then test this mapping.  If it succeeds then the PrologMts are equal</li>
            ///     <li>Otherwise we now fallback to the Base Mapping and use it as a basis for Brute Forcing the possible solution space and testing every possibility until either a mapping works or we find the PrologMts to be non-equal</li>
            ///     </ol>
            /// </li>
            /// </ol>
            /// </remarks>
            public virtual bool Equals(PNode g, out Dictionary<Part, Part> mapping)
            {
                //Set the mapping to be null
                mapping = null;

                ///PrologMtMatcher matcher = new PrologMtMatcher();
                //   if (matcher.Equals(this, g))
                {
                    //     mapping = matcher.Mapping;
                    return true;
                }
                //else
                {
                    return false;
                }
            }

            #endregion

            #region Sub-PrologKB Matching

            /// <summary>
            /// Checks whether this PrologKB is a sub-prologmt of the given PrologKB
            /// </summary>
            /// <param name="g">PrologKB</param>
            /// <returns></returns>
            public bool IsSubPrologMtOf(PNode g)
            {
                Dictionary<Part, Part> temp;
                return this.IsSubPrologMtOf(g, out temp);
            }

            /// <summary>
            /// Checks whether this PrologKB is a sub-prologmt of the given PrologKB
            /// </summary>
            /// <param name="g">PrologKB</param>
            /// <param name="mapping">Mapping of Blank Nodes</param>
            /// <returns></returns>
            public bool IsSubPrologMtOf(PNode g, out Dictionary<Part, Part> mapping)
            {
                //Set the mapping to be null
                mapping = null;

                //   SubPrologMtMatcher matcher = new SubPrologMtMatcher();
                //   if (matcher.IsSubPrologMt(this, g))
                {
                    //mapping = matcher.Mapping;
                    return true;
                }
                //else
                {
                    return false;
                }
            }

            /// <summary>
            /// Checks whether this PrologKB has the given PrologKB as a sub-prologmt
            /// </summary>
            /// <param name="g">PrologKB</param>
            /// <returns></returns>
            public bool HasSubPrologMt(PNode g)
            {
                return g.IsSubPrologMtOf((PNode) this);
            }

            /// <summary>
            /// Checks whether this PrologKB has the given PrologKB as a sub-prologmt
            /// </summary>
            /// <param name="g">PrologKB</param>
            /// <param name="mapping">Mapping of Blank Nodes</param>
            /// <returns></returns>
            public bool HasSubPrologMt(PNode g, out Dictionary<Part, Part> mapping)
            {
                mapping = null;
                return g.IsSubPrologMtOf((PNode) this); //, out mapping);
            }

            #endregion



            #region Operators

#if !NO_DATA

            /// <summary>
            /// Converts a PrologKB into a DataTable using the explicit cast operator defined by this class
            /// </summary>
            /// <returns>
            /// A DataTable containing three Columns (Subject, Predicate and Object) all typed as <see cref="Part">Part</see> with a Row per Rule
            /// </returns>
            /// <remarks>
            /// <strong>Warning:</strong> Not available under builds which remove the Data Storage layer from dotNetRDF e.g. Silverlight
            /// </remarks>
            public virtual DataTable ToDataTable()
            {
                return (DataTable) this;
            }

            /// <summary>
            /// Casts a PrologKB to a DataTable with all Columns typed as <see cref="Part">Part</see> (Column Names are Subject, Predicate and Object
            /// </summary>
            /// <param name="g">PrologKB to convert</param>
            /// <returns>
            /// A DataTable containing three Columns (Subject, Predicate and Object) all typed as <see cref="Part">Part</see> with a Row per Rule
            /// </returns>
            /// <remarks>
            /// <strong>Warning:</strong> Not available under builds which remove the Data Storage layer from dotNetRDF e.g. Silverlight
            /// </remarks>
            public static explicit operator DataTable(PrologMT g)
            {
                DataTable table = new DataTable();
                return table;
            }

#endif

            #endregion

            #region Events

            /// <summary>
            /// Event which is raised when a Rule is asserted in the PrologKB
            /// </summary>
            public event RuleEventHandler RuleAsserted;

            /// <summary>
            /// Event which is raised when a Rule is retracted from the PrologKB
            /// </summary>
            public event RuleEventHandler RuleRetracted;

            /// <summary>
            /// Event which is raised when the PrologKB contents change
            /// </summary>
            public event PrologMtEventHandler Changed;

            /// <summary>
            /// Event which is raised just before the PrologKB is cleared of its contents
            /// </summary>
            public event CancellablePrologMtEventHandler ClearRequested;

            /// <summary>
            /// Event which is raised after the PrologKB is cleared of its contents
            /// </summary>
            public event PrologMtEventHandler Cleared;

            /// <summary>
            /// Event which is raised when a Merge operation is requested on the PrologKB
            /// </summary>
            public event CancellablePrologMtEventHandler MergeRequested;

            /// <summary>
            /// Event which is raised when a Merge operation is completed on the PrologKB
            /// </summary>
            public event PrologMtEventHandler Merged;

            /// <summary>
            /// Event Handler which handles the <see cref="RuleList.RuleAdded">Rule Added</see> event from the underlying Rule Collection by raising the PrologKB's <see cref="RuleAsserted">RuleAsserted</see> event
            /// </summary>
            /// <param name="sender">Sender</param>
            /// <param name="args">Rule Event Arguments</param>
            protected virtual void OnRuleAsserted(Object sender, RuleEventArgs args)
            {
                this.RaiseRuleAsserted(args);
            }

            /// <summary>
            /// Helper method for raising the <see cref="RuleAsserted">Rule Asserted</see> event manually
            /// </summary>
            /// <param name="args">Rule Event Arguments</param>
            protected void RaiseRuleAsserted(RuleEventArgs args)
            {
                RuleEventHandler d = this.RuleAsserted;
                args.PrologKB = this;
                if (d != null)
                {
                    d(this, args);
                }
                this.RaisePrologMtChanged(args);
            }

            /// <summary>
            /// Helper method for raising the <see cref="RuleAsserted">Rule Asserted</see> event manually
            /// </summary>
            /// <param name="t">Rule</param>
            protected void RaiseRuleAsserted(Rule t)
            {
                RuleEventHandler d = this.RuleAsserted;
                PrologMtEventHandler e = this.Changed;
                if (d != null || e != null)
                {
                    RuleEventArgs args = new RuleEventArgs(t, (PNode) this);
                    if (d != null) d(this, args);
                    if (e != null) e(this, new PrologMtEventArgs((PNode) this, args));
                }
            }

            /// <summary>
            /// Event Handler which handles the <see cref="RuleList.RuleRemoved">Rule Removed</see> event from the underlying Rule Collection by raising the PrologKB's <see cref="RuleRetracted">Rule Retracted</see> event
            /// </summary>
            /// <param name="sender">Sender</param>
            /// <param name="args">Rule Event Arguments</param>
            protected virtual void OnRuleRetracted(Object sender, RuleEventArgs args)
            {
                this.RaiseRuleRetracted(args);
            }

            /// <summary>
            /// Helper method for raising the <see cref="RuleRetracted">Rule Retracted</see> event manually
            /// </summary>
            /// <param name="args"></param>
            protected void RaiseRuleRetracted(RuleEventArgs args)
            {
                RuleEventHandler d = this.RuleRetracted;
                args.PrologKB = this;
                if (d != null)
                {
                    d(this, args);
                }
                this.RaisePrologMtChanged(args);
            }

            /// <summary>
            /// Helper method for raising the <see cref="RuleRetracted">Rule Retracted</see> event manually
            /// </summary>
            /// <param name="t">Rule</param>
            protected void RaiseRuleRetracted(Rule t)
            {
                RuleEventHandler d = this.RuleRetracted;
                PrologMtEventHandler e = this.Changed;
                if (d != null || e != null)
                {
                    RuleEventArgs args = new RuleEventArgs(t, (PNode) this, false);
                    if (d != null) d(this, args);
                    if (e != null) e(this, new PrologMtEventArgs((PNode) this, args));
                }
            }

            /// <summary>
            /// Helper method for raising the <see cref="Changed">Changed</see> event
            /// </summary>
            /// <param name="args">Rule Event Arguments</param>
            protected void RaisePrologMtChanged(RuleEventArgs args)
            {
                PrologMtEventHandler d = this.Changed;
                if (d != null)
                {
                    d(this, new PrologMtEventArgs((PNode) this, args));
                }
            }

            /// <summary>
            /// Helper method for raising the <see cref="Changed">Changed</see> event
            /// </summary>
            protected void RaisePrologMtChanged()
            {
                PrologMtEventHandler d = this.Changed;
                if (d != null)
                {
                    d(this, new PrologMtEventArgs((PNode) this));
                }
            }

            /// <summary>
            /// Helper method for raising the <see cref="ClearRequested">Clear Requested</see> event and returning whether any of the Event Handlers cancelled the operation
            /// </summary>
            /// <returns>True if the operation can continue, false if it should be aborted</returns>
            protected bool RaiseClearRequested()
            {
                CancellablePrologMtEventHandler d = this.ClearRequested;
                if (d != null)
                {
                    CancellablePrologMtEventArgs args = new CancellablePrologMtEventArgs((PNode) this);
                    d(this, args);
                    return !args.Cancel;
                }
                else
                {
                    return true;
                }
            }

            /// <summary>
            /// Helper method for raising the <see cref="Cleared">Cleared</see> event
            /// </summary>
            protected void RaiseCleared()
            {
                PrologMtEventHandler d = this.Cleared;
                if (d != null)
                {
                    d(this, new PrologMtEventArgs((PNode) this));
                }
            }

            /// <summary>
            /// Helper method for raising the <see cref="MergeRequested">Merge Requested</see> event and returning whether any of the Event Handlers cancelled the operation
            /// </summary>
            /// <returns>True if the operation can continue, false if it should be aborted</returns>
            protected bool RaiseMergeRequested()
            {
                CancellablePrologMtEventHandler d = this.MergeRequested;
                if (d != null)
                {
                    CancellablePrologMtEventArgs args = new CancellablePrologMtEventArgs((PNode) this);
                    d(this, args);
                    return !args.Cancel;
                }
                else
                {
                    return true;
                }
            }

            /// <summary>
            /// Helper method for raising the <see cref="Merged">Merged</see> event
            /// </summary>
            protected void RaiseMerged()
            {
                PrologMtEventHandler d = this.Merged;
                if (d != null)
                {
                    d(this, new PrologMtEventArgs((PNode) this));
                }
            }

            /// <summary>
            /// Helper method for attaching the necessary event Handlers to a Rule Collection
            /// </summary>
            /// <param name="ruleCollection">Rule Collection</param>
            /// <remarks>
            /// May be useful if you replace the Rule Collection after instantiation e.g. as done in <see cref="VDS.RDF.Query.SparqlView">SparqlView</see>'s
            /// </remarks>
            protected void AttachEventHandlers(RuleList ruleCollection)
            {
                ruleCollection.RuleAdded += this.RuleAddedHandler;
                ruleCollection.RuleRemoved += this.RuleRemovedHandler;
            }

            /// <summary>
            /// Helper method for detaching the necessary event Handlers from a Rule Collection
            /// </summary>
            /// <param name="ruleCollection">Rule Collection</param>
            /// <remarks>
            /// May be useful if you replace the Rule Collection after instantiation e.g. as done in <see cref="VDS.RDF.Query.SparqlView">SparqlView</see>'s
            /// </remarks>
            protected void DetachEventHandlers(RuleList ruleCollection)
            {
                ruleCollection.RuleAdded -= this.RuleAddedHandler;
                ruleCollection.RuleRemoved -= this.RuleRemovedHandler;
            }

            #endregion

            /// <summary>
            /// Disposes of a PrologKB
            /// </summary>
            public virtual void Dispose()
            {
                this.DetachEventHandlers(this._rules);
            }

#if !SILVERLIGHT

            #region ISerializable Members

            /// <summary>
            /// Gets the Serialization Information for serializing a PrologKB
            /// </summary>
            /// <param name="info">Serialization Information</param>
            /// <param name="context">Streaming Context</param>
            public void GetObjectData(SerializationInfo info, StreamingContext context)
            {
                //info.AddValue("base", this.BaseStartMt.ToSafeString());
                info.AddValue("rules", this.Rules.ToList(), typeof (List<Rule>));
                /*       IEnumerable<KeyValuePair<String, String>> ns = from p in this.NamespaceMap.Prefixes
                                                               select
                                                                   new KeyValuePair<String, String>(p,
                                                                                                    this.NamespaceMap.
                                                                                                        GetNamespaceStartMt(
                                                                                                            p).
                                                                                                        AbsoluteStartMt);
                info.AddValue("namespaces", ns.ToList(), typeof (List<KeyValuePair<String, String>>));*/
            }

            #endregion

            #region IXmlSerializable Members

            /// <summary>
            /// Gets the Schema for XML Serialization
            /// </summary>
            /// <returns></returns>
            public XmlSchema GetSchema()
            {
                return null;
            }

            /// <summary>
            /// Reads the data for XML deserialization
            /// </summary>
            /// <param name="reader">XML Reader</param>
            public void ReadXml(XmlReader reader)
            {
                XmlSerializer ruleDeserializer = new XmlSerializer(typeof (Rule));
                reader.Read();
                if (reader.Name.Equals("namespaces"))
                {
                    if (!reader.IsEmptyElement)
                    {
                        reader.Read();
                        while (reader.Name.Equals("namespace"))
                        {
                            if (reader.MoveToAttribute("prefix"))
                            {
                                String prefix = reader.Value;
                                if (reader.MoveToAttribute("Microtheory"))
                                {
                                    //   String u = StartMtFactory.Create(reader.Value);
                                    //   this.NamespaceMap.AddNamespace(prefix, u);
                                    reader.Read();
                                }
                                else
                                {
                                    throw new FormatException(
                                        "Expected a Microtheory attribute on a <namespace> element");
                                }
                            }
                            else
                            {
                                throw new FormatException("Expected a prefix attribute on a <namespace> element");
                            }
                        }
                    }
                }
                reader.Read();
                if (reader.Name.Equals("rules"))
                {
                    if (!reader.IsEmptyElement)
                    {
                        reader.Read();
                        while (reader.Name.Equals("rule"))
                        {
                            try
                            {
                                Object temp = ruleDeserializer.Deserialize(reader);
                                this.Assert((Rule) temp);
                                reader.Read();
                            }
                            catch
                            {
                                throw;
                            }
                        }
                    }
                }
                else
                {
                    throw new FormatException("Expected a <rules> element inside a <prologmt> element but got a <" +
                                              reader.Name + "> element instead");
                }
            }

            /// <summary>
            /// Writes the data for XML serialization
            /// </summary>
            /// <param name="writer">XML Writer</param>
            public void WriteXml(XmlWriter writer)
            {
                XmlSerializer ruleSerializer = new XmlSerializer(typeof (Rule));

                //Serialize Base String
                if (this.BaseStartMt != null)
                {
                    //  writer.WriteAttributeString("base", this.BaseStartMt.AbsoluteStartMt);
                }

                //Serialize Namespace Map
                writer.WriteStartElement("namespaces");
                //   foreach (String prefix in this.NamespaceMap.Prefixes)
                {
                    writer.WriteStartElement("namespace");
                    //writer.WriteAttributeString("prefix", prefix);
                    //     writer.WriteAttributeString("Microtheory", this.NamespaceMap.GetNamespaceStartMt(prefix).AbsoluteStartMt);
                    writer.WriteEndElement();
                }
                writer.WriteEndElement();

                //Serialize Rules
                writer.WriteStartElement("rules");
                foreach (Rule t in this.Rules)
                {
                    ruleSerializer.Serialize(writer, t);
                }
                writer.WriteEndElement();
            }

            #endregion

#endif

        }



        #region Reader and Writer Warning Events

        /// <summary>
        /// Delegate Type for Warning Messages raised by RDF Readers
        /// </summary>
        /// <param name="warning">Warning Message</param>
        public delegate void RdfReaderWarning(String warning);

        /// <summary>
        /// Delegate Type for Warning Messages raised by RDF Writers
        /// </summary>
        /// <param name="message">Warning Message</param>
        public delegate void RdfWriterWarning(String message);

        /// <summary>
        /// Delegate Type for Warning Events raised by RDF Dataset Writers
        /// </summary>
        /// <param name="message">Warning Message</param>
        public delegate void StoreWriterWarning(String message);

        /// <summary>
        /// Delegate Type for Warning Events raised by RDF Dataset Readers
        /// </summary>
        /// <param name="message">Warning Message</param>
        public delegate void StoreReaderWarning(String message);

        /// <summary>
        /// Delegate Type for Warning Events raised by SPARQL Readers and Writers for Queries, Updates and Results
        /// </summary>
        /// <param name="message">Warning Message</param>
        public delegate void SparqlWarning(String message);

        #endregion

        #region Rule, PrologKB and Rule Store Events

        /// <summary>
        /// Delegate Type for Rule events raised by PrologMts
        /// </summary>
        /// <param name="sender">Originator of the Event</param>
        /// <param name="args">Rule Event Arguments</param>
        public delegate void RuleEventHandler(Object sender, RuleEventArgs args);

        /// <summary>
        /// Delegate Type for PrologKB events raised by PrologMts
        /// </summary>
        /// <param name="sender">Originator of the Event</param>
        /// <param name="args">PrologKB Event Arguments</param>
        public delegate void PrologMtEventHandler(Object sender, PrologMtEventArgs args);

        /// <summary>
        /// Delegate Type for PrologKB events raised by PrologMts where event handlers may set a Cancel flag to cancel the subsequent operation
        /// </summary>
        /// <param name="sender">Originator of the Event</param>
        /// <param name="args">PrologKB Event Arguments</param>
        public delegate void CancellablePrologMtEventHandler(Object sender, CancellablePrologMtEventArgs args);

        /// <summary>
        /// Delegate Type for Rule Store events raised by Rule Stores
        /// </summary>
        /// <param name="sender">Originator of the event</param>
        /// <param name="args">Rule Store Event Arguments</param>
        public delegate void RuleStoreEventHandler(Object sender, RuleStoreEventArgs args);

        #endregion

        #region Event Argument Classes


        /// <summary>
        /// Event Arguments for Events regarding the assertion and retraction of Rules
        /// </summary>
        public class RuleEventArgs : EventArgs
        {
            private Rule _t;
            private PNode _g;
            private bool _added = true;

            /// <summary>
            /// Creates a new set of Rule Event Arguments for the given Rule
            /// </summary>
            /// <param name="t">Rule</param>
            /// <param name="g">PrologKB the Rule Event occurred in</param>
            public RuleEventArgs(Rule t, PNode g)
                : base()
            {
                this._t = t;
                this._g = g;
            }

            /// <summary>
            /// Creates a new set of Rule Event Arguments for the given Rule
            /// </summary>
            /// <param name="t">Rule</param>
            /// <param name="g">PrologKB the Rule Event occurred in</param>
            /// <param name="asserted">Was the Rule Asserted (if not then it was Retracted)</param>
            public RuleEventArgs(Rule t, PNode g, bool asserted)
                : this(t, g)
            {
                this._added = asserted;
            }

            /// <summary>
            /// Gets the Rule
            /// </summary>
            public Rule Rule
            {
                get { return this._t; }
            }

            /// <summary>
            /// Gets the PrologKB the Rule belongs to (may be null)
            /// </summary>
            public PrologMT PrologKB
            {
                get { return this._g; }
                internal set { this._g = (PNode) value; }
            }

            /// <summary>
            /// Gets the Microtheory of the PrologKB the Rule belongs to (may be null)
            /// </summary>
            public String PrologMtStartMt
            {
                get { return (this._g == null) ? null : this._g.BaseStartMt; }
            }

            /// <summary>
            /// Gets whether the Rule was asserted
            /// </summary>
            public bool WasAsserted
            {
                get { return this._added; }
            }

            /// <summary>
            /// Gets whether the Rule was retracted
            /// </summary>
            public bool WasRetracted
            {
                get { return !this._added; }
            }
        }

        /// <summary>
        /// Event Arguments for Events regarding PrologMts
        /// </summary>
        public class PrologMtEventArgs : EventArgs
        {
            private PNode _g;
            private RuleEventArgs _args;

            /// <summary>
            /// Creates a new set of PrologKB Event Arguments
            /// </summary>
            /// <param name="g">PrologKB</param>
            public PrologMtEventArgs(PNode g)
                : base()
            {
                this._g = g;
            }

            /// <summary>
            /// Creates a new set of PrologKB Event Arguments
            /// </summary>
            /// <param name="g">PrologKB</param>
            /// <param name="args">Rule Event Arguments</param>
            public PrologMtEventArgs(PNode g, RuleEventArgs args)
                : this(g)
            {
                this._args = args;
            }

            /// <summary>
            /// Gets the PrologKB
            /// </summary>
            public PNode PrologKB
            {
                get { return this._g; }
            }

            /// <summary>
            /// Gets the Rule Event Arguments (if any)
            /// </summary>
            public RuleEventArgs RuleEvent
            {
                get { return this._args; }
            }
        }

        /// <summary>
        /// Event Arguments for Events regarding PrologMts which may be cancelled
        /// </summary>
        public class CancellablePrologMtEventArgs : PrologMtEventArgs
        {
            private bool _cancel;

            /// <summary>
            /// Creates a new set of Cancellable PrologKB Event Arguments
            /// </summary>
            /// <param name="g">PrologKB</param>
            public CancellablePrologMtEventArgs(PNode g)
                : base(g)
            {
            }

            /// <summary>
            /// Creates a new set of Cancellable PrologKB Event Arguments
            /// </summary>
            /// <param name="g">PrologKB</param>
            /// <param name="args">Rule Event Arguments</param>
            public CancellablePrologMtEventArgs(PNode g, RuleEventArgs args)
                : base(g, args)
            {
            }

            /// <summary>
            /// Gets/Sets whether the Event should be cancelled
            /// </summary>
            public bool Cancel
            {
                get { return this._cancel; }
                set { this._cancel = value; }
            }
        }

        /// <summary>
        /// Event Arguments for Events regarding PrologMts
        /// </summary>
        public class RuleStoreEventArgs : EventArgs
        {
            private object _store;
            private PrologMtEventArgs _args;

            /// <summary>
            /// Creates a new set of Rule Store Event Arguments
            /// </summary>
            /// <param name="store">Rule Store</param>
            public RuleStoreEventArgs(object store)
                : base()
            {
                this._store = store;
            }

            /// <summary>
            /// Creates a new set of Rule Store Event Arguments
            /// </summary>
            /// <param name="store">Rule Store</param>
            /// <param name="args">PrologKB Event Arguments</param>
            public RuleStoreEventArgs(object store, PrologMtEventArgs args)
                : this(store)
            {
                this._args = args;
            }

            /// <summary>
            /// Creates a new set of Rule Store Event Arguments
            /// </summary>
            /// <param name="store">Rule Store</param>
            /// <param name="g">PrologKB</param>
            public RuleStoreEventArgs(object store, PNode g)
                : this(store, new PrologMtEventArgs(g))
            {
            }

            /// <summary>
            /// Gets the Rule Store
            /// </summary>
            public object RuleStore
            {
                get { return this._store; }
            }

            /// <summary>
            /// Gets the PrologKB Event Arguments (if any)
            /// </summary>
            public PrologMtEventArgs PrologMtEvent
            {
                get { return this._args; }
            }
        }

        #endregion

    }

    #endregion
}