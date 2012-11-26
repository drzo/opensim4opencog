using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;
using System.Data;
using System.IO;

namespace LogicalParticleFilter1
{
    // Original from: Roosevelt Junior dos Santos 
    // http://www.codeproject.com/Articles/5276/ID3-Decision-Tree-Algorithm-in-C
    // http://oopstruggles.blogspot.com/2011/02/id3-decision-tree-in-c.html

    /// <summary>
    /// Class that implements a decision tree using the ID3 algorithm
    /// </summary>
    public class DecisionTree
    {
        private DataTable _sampleData;
        private int mTotalPositives = 0;
        private int mTotal = 0;
        private string mTargetAttribute = "result";
        private double mEntropySet = 0.0;

        /// <summary>
        /// Returns the total number of positive samples in a table of samples 
        /// </summary>
        /// <param name="samples"> DataTable with samples </param>
        /// <returns>number of positive samples</returns>
        private int countTotalPositives0(DataTable samples)
        {
            int result = 0;

            foreach (DataRow aRow in samples.Rows)
            {
                if (aRow[mTargetAttribute].ToString().ToUpper().Trim() == "TRUE")
                    result++;
            }

            return result;
        }
        private int countTotalPositives(DataTable samples)
        {
            int result = 0;
            string positveRef = getUniformRefValue(samples, mTargetAttribute);

            foreach (DataRow aRow in samples.Rows)
            {
                if (aRow[mTargetAttribute].ToString().ToLower().Trim() == positveRef)
                    result++;
            }

            return result;
        }

        /// <summary>
        /// Calculate the entropy given the following formula 
        /// -p+log2p+ - p-log2p-
        /// Where  p+ is the proportion of positive values 
        ///        P- is the ratio of negative 
        ///		  
        /// </summary>
        /// <param name="positives">Number of positive values ​​</param>
        /// <param name="negatives"> quantity negative values </param>
        /// <returns>Returns the value of Entropy</returns>
        private double getCalculatedEntropy(int positives, int negatives)
        {
            int total = positives + negatives;
            double ratioPositive = (double)positives / total;
            double ratioNegative = (double)negatives / total;

            if (ratioPositive != 0)
                ratioPositive = -(ratioPositive) * System.Math.Log(ratioPositive, 2);
            if (ratioNegative != 0)
                ratioNegative = -(ratioNegative) * System.Math.Log(ratioNegative, 2);

            double result = ratioPositive + ratioNegative;

            return result;
        }

        /// <summary>
        /// Sample table scans checking an attribute and if the result is positive or negative
        /// </summary>
        /// <param name="samples">DataTable with samples</param>
        /// <param name="attribute"> attribute to be searched </param>
        /// <param name="value"> value allowed for the attribute </param>
        /// <param name="positives"> nro will contain all the attributes with the value determined with positive results </param>
        /// <param name="negatives">nro will contain all the attributes with the value determined with negative</param>
        private void getValuesToAttribute0(DataTable samples, TreeAttribute attribute, string value, out int positives, out int negatives)
        {
            positives = 0;
            negatives = 0;

            foreach (DataRow aRow in samples.Rows)
            {
                ///To do:   Figure out if this is correct - it looks bad
                if (((string)aRow[attribute.AttributeName] == value))
                    if (aRow[mTargetAttribute].ToString().Trim().ToUpper() == "TRUE")
                        positives++;
                    else
                        negatives++;

            }
        }
        private void getValuesToAttribute(DataTable samples, TreeAttribute attribute, string value, out int positives, out int negatives)
        {
            positives = 0;
            negatives = 0;
            string positveRef = getUniformRefValue(samples, mTargetAttribute);
            foreach (DataRow aRow in samples.Rows)
            {
                ///To do:   Figure out if this is correct - it looks bad
                if (((string)aRow[attribute.AttributeName] == value))
                    if (aRow[mTargetAttribute].ToString().Trim().ToLower() == positveRef)
                        positives++;
                    else
                        negatives++;

            }
        }
        private void getValuesToAttributeContinous(DataTable samples, TreeAttribute attribute, string value, out int positives, out int negatives)
        {
            positives = 0;
            negatives = 0;
            double refval = double.Parse(value);

            string positveRef = getUniformRefValue(samples, mTargetAttribute);

            foreach (DataRow aRow in samples.Rows)
            {
                ///To do:   Figure out if this is correct - it looks bad
                double rowV = double.Parse((string)aRow[attribute.AttributeName]);
                if ((rowV <= refval))
                    if (aRow[mTargetAttribute].ToString().Trim().ToLower() == positveRef)
                        positives++;
                    else
                        negatives++;

            }
        }

        bool isContinousSet(PossibleValueCollection values)
        {
            if (values.Count == 0) return false;
            for (int i = 0; i < values.Count; i++)
            {
                double result;
                if (Double.TryParse(values[i],out result) ==false)
                {
                    return false;
                }
            }
            return true;
        }
        /// <summary>
        /// Calculate the gain of an attribute
        /// </summary>
        /// <param name="attribute">Attribute to be calculated </param>
        /// <returns> Gain attribute </returns>
        private double gain(DataTable samples, TreeAttribute attribute)
        {
            PossibleValueCollection values = attribute.PossibleValues;
            if (isContinousSet(values))
            {
                double sum = 0.0;
                double bsum = -9999999.0;

                // return the value for the best possible split
                for (int i = 0; i < values.Count; i++)
                {
                    int positives, negatives;

                    positives = negatives = 0;

                    getValuesToAttributeContinous(samples, attribute, values[i], out positives, out negatives);

                    double entropy = getCalculatedEntropy(positives, negatives);
                    sum = -(double)(positives + negatives) / mTotal * entropy;
                    if (sum > bsum) { bsum = sum; }
                }
                return mEntropySet + bsum;

            }
            else
            {
                double sum = 0.0;

                for (int i = 0; i < values.Count; i++)
                {
                    int positives, negatives;

                    positives = negatives = 0;

                    getValuesToAttribute(samples, attribute, values[i], out positives, out negatives);
                    // does it really split?
                    int remainder = mTotal - (positives + negatives);
                    if (remainder > 0)
                    {
                        double entropy = getCalculatedEntropy(positives, negatives);
                        sum += -(double)(positives + negatives) / mTotal * entropy;
                    }
                }
                return mEntropySet + sum;
            }
        }

        string bestSplitValue(DataTable samples, TreeAttribute attribute)
        {
            PossibleValueCollection values = attribute.PossibleValues;
            if (isContinousSet(values))
            {
                double sum = 0.0;
                double bsum = -9999999.0;
                string bval = values[0];

                // return the value for the best possible split
                for (int i = 0; i < values.Count; i++)
                {
                    int positives, negatives;

                    positives = negatives = 0;

                    getValuesToAttributeContinous(samples, attribute, values[i], out positives, out negatives);
                    // does it really split?
                    int remainder = mTotal - (positives + negatives);
                    if (remainder > 0)
                    {

                        double entropy = getCalculatedEntropy(positives, negatives);
                        sum = -(double)(positives + negatives) / mTotal * entropy;
                        if (sum > bsum)
                        {
                            bval = values[i];
                            bsum = sum;
                        }
                    }
                }
                return bval;

            }
            else
            {
                return values[0];
            }
        }
        /// <summary>
        ///Returns the best attribute.
        /// </summary>
        /// <param name="attributes"> A vector with attributes </param>
        /// <returns>Returns which has higher gain </returns>
        private TreeAttribute getBestAttribute(DataTable samples, TreeAttributeCollection attributes)
        {
            double maxGain = -9999999.0;
            TreeAttribute result = null;

            foreach (TreeAttribute attribute in attributes)
            {
                double aux = gain(samples, attribute);
                if (aux > maxGain)
                {
                    maxGain = aux;
                    result = attribute;
                }
            }
            return result;
        }

        /// <summary>
        /// Returns true if all are positive examples of sampling 
        /// </summary>
        /// <param name="samples"> DataTable with samples </param>
        /// <param name="targetAttribute"> attribute (column) of the table which will be checked </param>
        /// <returns> all examples are positive sampling </returns>
        private bool allSamplesArePositive(DataTable samples, string targetAttribute)
        {
            foreach (DataRow row in samples.Rows)
            {
                if (row[targetAttribute].ToString().ToUpper().Trim() == "FALSE")
                    return false;
            }

            return true;
        }

        /// <summary>
        /// Returns true if all are negative examples sampling 
        /// </summary>
        /// <param name="samples"> DataTable with samples </param>
        /// <param name="targetAttribute"> attribute (column) of the table which will be checked </param>
        /// <returns>> all examples of sampling are negative </returns>
        private bool allSamplesAreNegative(DataTable samples, string targetAttribute)
        {
            foreach (DataRow row in samples.Rows)
            {
                if (row[targetAttribute].ToString().ToUpper().Trim() == "TRUE")
                    return false;
            }

            return true;
        }

        private bool allSamplesAreUniform(DataTable samples, string targetAttribute)
        {

            if (samples.Rows.Count == 0) return true;

            string refval = getUniformRefValue(samples, targetAttribute);
            foreach (DataRow row in samples.Rows)
            {
                string rv = row[targetAttribute].ToString().ToLower().Trim();
                if (rv != refval)
                    return false;
            }

            return true;
        }

        string getUniformRefValue(DataTable samples, string targetAttribute)
        {
            int p = samples.Rows.Count - 1;
            if (p < 0) return null;
            DataRow refrow = samples.Rows[p];
            string refval = refrow[targetAttribute].ToString().ToLower().Trim();
            return refval;
        }

        /// <summary>
        ///  Returns a list of all the distinct values ​​from a table sampling
        /// </summary>
        /// <param name="samples"> DataTable with samples </param>
        /// <param name="targetAttribute">attribute (column) of the table which will be checked </param>
        /// <returns>An ArrayList with distinct values </returns>
        private ArrayList getDistinctValues(DataTable samples, string targetAttribute)
        {
            ArrayList distinctValues = new ArrayList(samples.Rows.Count);

            foreach (DataRow row in samples.Rows)
            {
                if (distinctValues.IndexOf(row[targetAttribute]) == -1)
                    distinctValues.Add(row[targetAttribute]);
            }
            distinctValues.Sort();
            return distinctValues;
        }

        /// <summary>
        /// Returns the most common value within a sampling 
        /// </summary>
        /// <param name="samples"> DataTable with samples </param>
        /// <param name="targetAttribute"> attribute (column) of the table which will be checked </param>
        /// <returns>Returns the object with highest incidence within the table of samples </returns>
        private object getMostCommonValue(DataTable samples, string targetAttribute)
        {
            ArrayList distinctValues = getDistinctValues(samples, targetAttribute);
            int[] count = new int[distinctValues.Count];

            foreach (DataRow row in samples.Rows)
            {
                int index = distinctValues.IndexOf(row[targetAttribute]);
                count[index]++;
            }

            int MaxIndex = 0;
            int MaxCount = 0;

            for (int i = 0; i < count.Length; i++)
            {
                if (count[i] > MaxCount)
                {
                    MaxCount = count[i];
                    MaxIndex = i;
                }
            }

            return distinctValues[MaxIndex];
        }

        /// <summary>
        /// Sets up a decision tree based on samples submitted 
        /// </summary>
        /// <param name="samples">Table with samples that will be provided for mounting the tree </param>
        /// <param name="targetAttribute"> Name column of the table that otherwise has the value true or false to 
        /// Validate or not a sample</param>
        /// <returns>The root of the decision tree mounted </returns></returns?>
        private TreeNode internalMountTree(DataTable samples, string targetAttribute, TreeAttributeCollection attributes)
        {
            if (allSamplesAreUniform(samples, targetAttribute) == true)
            {
                return new TreeNode(new OutcomeTreeAttribute(getUniformRefValue(samples, targetAttribute)));
            }
            //if (allSamplesArePositive(samples, targetAttribute) == true)
            //    return new TreeNode(new OutcomeTreeAttribute(true));

            //if (allSamplesAreNegative(samples, targetAttribute) == true)
            //    return new TreeNode(new OutcomeTreeAttribute(false));


            if (attributes.Count == 0)
            {
                return new TreeNode(new OutcomeTreeAttribute(getMostCommonValue(samples, targetAttribute)));
            }
            mTotal = samples.Rows.Count;
            mTargetAttribute = targetAttribute;
            mTotalPositives = countTotalPositives(samples);

            mEntropySet = getCalculatedEntropy(mTotalPositives, mTotal - mTotalPositives);

            TreeAttribute bestAttribute = getBestAttribute(samples, attributes);

            TreeNode root = new TreeNode(bestAttribute);

            if (bestAttribute == null)
            {
                return root;
            }
            PossibleValueCollection bestAttrValues = bestAttribute.PossibleValues;
            bool continousSet = isContinousSet(bestAttrValues);
            //DataTable aSample = samples.Clone();
            if (continousSet)
            {
                string value = bestSplitValue(samples, bestAttribute);
                {
                    DataTable aSample = samples.Clone();
                    //First Below then Above
                    DataRow[] rows;
                    string cond = bestAttribute.AttributeName + " <= " + "" + value + "";
                    rows = samples.Select(cond);

                    aSample.Rows.Clear();
                    foreach (DataRow row in rows)
                    {
                        aSample.Rows.Add(row.ItemArray);
                        Console.WriteLine(" SPLIT {0} ROW:", cond);
                        foreach (DataColumn myCol in samples.Columns)
                        {
                            Console.WriteLine("  " + row[myCol]);
                        }
                    }
                    // Create a new attribute list unless the attribute which is the current best attribute		
                    TreeAttributeCollection aAttributes = new TreeAttributeCollection();
                    //ArrayList aAttributes = new ArrayList(attributes.Count - 1);
                    for (int i = 0; i < attributes.Count; i++)
                    {
                        if (attributes[i].AttributeName != bestAttribute.AttributeName)
                            aAttributes.Add(attributes[i]);
                    }
                    //Recycle the best continous attribute if there are others
                    if (aAttributes.Count > 0) aAttributes.Add(bestAttribute);

                    // Create a new attribute list unless the attribute which is the current best attribute 

                    if (rows.Length == 0)
                    {
                        //return new TreeNode(new OutcomeTreeAttribute(getMostCommonValue(aSample, targetAttribute)));
                        return new TreeNode(new OutcomeTreeAttribute(getMostCommonValue(samples, targetAttribute)));
                    }
                    else
                    {
                        DecisionTree dc3 = new DecisionTree();
                        TreeNode ChildNode = dc3.mountTree(aSample, targetAttribute, aAttributes);
                        root.AddTreeNode(ChildNode, value, "leq");
                    }
                }
                {
                    DataTable aSample = samples.Clone();
                    DataRow[] rows2;
                    string cond = bestAttribute.AttributeName + " > " + "" + value + "";
                    rows2 = samples.Select(cond);

                    aSample.Rows.Clear();
                    foreach (DataRow row in rows2)
                    {
                        aSample.Rows.Add(row.ItemArray);
                        Console.WriteLine(" SPLIT {0} ROW:", cond);
                        foreach (DataColumn myCol in samples.Columns)
                        {
                            Console.WriteLine("  "+row[myCol]);
                        }
                    }
                    // Create a new attribute list unless the attribute which is the current best attribute		
                    TreeAttributeCollection aAttributes2 = new TreeAttributeCollection();
                    //ArrayList aAttributes = new ArrayList(attributes.Count - 1);
                    for (int i = 0; i < attributes.Count; i++)
                    {
                        if (attributes[i].AttributeName != bestAttribute.AttributeName)
                            aAttributes2.Add(attributes[i]);
                    }
                    //Recycle the best continous attribute if there are others
                    if (aAttributes2.Count > 0) aAttributes2.Add(bestAttribute);

                    // Create a new attribute list unless the attribute which is the current best attribute 

                    if (rows2.Length == 0)
                    {
                        //return new TreeNode(new OutcomeTreeAttribute(getMostCommonValue(aSample, targetAttribute)));
                        return new TreeNode(new OutcomeTreeAttribute(getMostCommonValue(samples, targetAttribute)));
                    }
                    else
                    {
                        DecisionTree dc3 = new DecisionTree();
                        TreeNode ChildNode = dc3.mountTree(aSample, targetAttribute, aAttributes2);
                        root.AddTreeNode(ChildNode, value, "gt");
                    }
                }

            }
            else
            {
                DataTable aSample = samples.Clone();
                foreach (string value in bestAttribute.PossibleValues)
                {
                    // Select all elements with the value of this attribute				
                    aSample.Rows.Clear();

                    DataRow[] rows;

                    rows = samples.Select(bestAttribute.AttributeName + " = " + "'" + value + "'");

                    foreach (DataRow row in rows)
                    {
                        aSample.Rows.Add(row.ItemArray);
                    }
                    // Select all elements with the value of this attribute				

                    // Create a new attribute list unless the attribute which is the current best attribute		
                    TreeAttributeCollection aAttributes = new TreeAttributeCollection();
                    //ArrayList aAttributes = new ArrayList(attributes.Count - 1);
                    for (int i = 0; i < attributes.Count; i++)
                    {
                        if (attributes[i].AttributeName != bestAttribute.AttributeName)
                            aAttributes.Add(attributes[i]);
                    }
                    // Create a new attribute list unless the attribute which is the current best attribute 

                    if (aSample.Rows.Count == 0)
                    {
                        //return new TreeNode(new OutcomeTreeAttribute(getMostCommonValue(aSample, targetAttribute)));
                        return new TreeNode(new OutcomeTreeAttribute(getMostCommonValue(samples, targetAttribute)));
                    }
                    else
                    {
                        DecisionTree dc3 = new DecisionTree();
                        TreeNode ChildNode = dc3.mountTree(aSample, targetAttribute, aAttributes);
                        root.AddTreeNode(ChildNode, value,"eq");
                    }
                }
            }


            return root;
        }


        /// <summary>
        /// Sets up a decision tree based on samples submitted 
        /// </summary>
        /// <param name="samples">Table with samples that will be provided for mounting the tree </param>
        /// <param name="targetAttribute">Name column of the table that otherwise has the value true or false to
        ///  Validate or not a sample </param>
        /// <returns>The root of the decision tree mounted</returns></returns?>
        public TreeNode mountTree(DataTable samples, string targetAttribute, TreeAttributeCollection attributes)
        {
            _sampleData = samples;
            return internalMountTree(_sampleData, targetAttribute, attributes);
        }
    }

    /// <summary>
    /// Class that exemplifies the use of ID3 
    /// </summary>
    public class DecisionTreeImplementation
    {

        string _sourceFile;

        public string GetTree(string sourceFile)
        {
            _sourceFile = sourceFile;
            RawDataSource samples = new RawDataSource(_sourceFile);

            TreeAttributeCollection attributes = samples.GetValidAttributeCollection();

            DecisionTree id3 = new DecisionTree();
            TreeNode root = id3.mountTree(samples, "result", attributes);

            return PrintNode(root, "") + Environment.NewLine + PrologPrintNode(root, "", "result");

        }

        public void GenRulesFromMt(SIProlog pEngine, string sourceMt, string destMt)
        {
            string targetAttr = "result";
           
            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            //Get collection of instances ID's and Attribute Names
            string query = "targetAttribute(ATTRIBUTE)";
            pEngine.askQuery(query, sourceMt, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    if (k == "ATTRIBUTE") targetAttr = bindings[k];
                }
            }

            GenRulesFromMt(pEngine, sourceMt, destMt, targetAttr);
        }
        public void GenRulesFromMt(SIProlog pEngine, string sourceMt, string destMt,string targetAttribute)
        {
            
            MtDataSource samples = new MtDataSource(pEngine,sourceMt);

            TreeAttributeCollection attributes = samples.GetValidAttributeCollection(targetAttribute);

            DecisionTree id3 = new DecisionTree();
            TreeNode root = id3.mountTree(samples, targetAttribute, attributes);

            string prologCode = PrologPrintNode(root, "", targetAttribute);
            pEngine.insertKB(prologCode, destMt);
            string codeSummary=PrintNode(root, "") + Environment.NewLine + PrologPrintNode(root, "", "result");
            Console.WriteLine(codeSummary);
        }

        public string PrintNode(TreeNode root, string tabs)
        {
            if (root == null) return "";

            string returnString = String.Empty;
            string prefix = "Best Attribute: ";

            if (tabs != String.Empty)
                prefix = " -> Likely Outcome: ";
            returnString += (tabs + prefix + root.Attribute) + Environment.NewLine;

            if (root != null && root.Attribute != null && root.Attribute.PossibleValues != null)
            {
                for (int i = 0; i < root.Attribute.PossibleValues.Count; i++)
                {
                    TreeNode childNode = root.GetChildByBranchName(root.Attribute.PossibleValues[i]);
                    string childNodeRelation = root.GetChildRelationByBranchName(root.Attribute.PossibleValues[i]);

                    returnString += (Environment.NewLine + tabs + "\t" + "Input:  " + childNodeRelation + " " + root.Attribute.PossibleValues[i]) + Environment.NewLine;
                    returnString += PrintNode(childNode, "\t" + tabs);

                    if (childNodeRelation != "eq")
                    {
                        TreeNode childNode2 = root.GetChildByBranchName2(root.Attribute.PossibleValues[i]);
                        string childNodeRelation2 = root.GetChildRelationByBranchName2(root.Attribute.PossibleValues[i]);

                        returnString += (Environment.NewLine + tabs + "\t" + "Input:  " + childNodeRelation2 + " " + root.Attribute.PossibleValues[i]) + Environment.NewLine;
                        returnString += PrintNode(childNode2, "\t" + tabs);
                    }
                }
            }

            return returnString;
        }

        public string PrologPrintNode(TreeNode root, string precond, string targetAttribute)
        {
            string returnString = String.Empty;
            if (root == null) return "." + Environment.NewLine;
            if (root != null && root.Attribute != null && root.Attribute.PossibleValues == null)
            {

                returnString += "dataset(X," + targetAttribute + "," + root.Attribute + "):-" + precond.Substring(1) + "." + Environment.NewLine;

                return returnString;
            }
            if (root != null && root.Attribute != null && root.Attribute.PossibleValues != null)
            {
                for (int i = 0; i < root.Attribute.PossibleValues.Count; i++)
                {
                    //returnString += (Environment.NewLine + tabs + "\t" + "Input:  " + root.Attribute.PossibleValues[i]) + Environment.NewLine;
                    TreeNode childNode = root.GetChildByBranchName(root.Attribute.PossibleValues[i]);
                    string childNodeRelation = root.GetChildRelationByBranchName(root.Attribute.PossibleValues[i]);
                    string ourPrecond = "";
                    string attrVar = "VAL_" + root.Attribute.ToString().ToUpper();

                    switch (childNodeRelation)
                    {
                        case "eq":
                                ourPrecond = precond + ",dataset(X," + root.Attribute + "," + root.Attribute.PossibleValues[i] + ")";
                            break;
                        case "gt":
                            if (precond.Contains(attrVar))
                            {
                                ourPrecond = precond + ",dgtr(" + attrVar + "," + root.Attribute.PossibleValues[i] + ")";
                            }
                            else
                            {
                                ourPrecond = precond + ",dataset(X," + root.Attribute + "," + attrVar + "),dgtr(" + attrVar + "," + root.Attribute.PossibleValues[i] + ")";
                            }
                            break;
                        case "leq":
                                if (precond.Contains(attrVar))
                                {
                                    ourPrecond = precond + ",dleq(" + attrVar + "," + root.Attribute.PossibleValues[i] + ")";
                                }
                                else
                                {
                                    ourPrecond = precond + ",dataset(X," + root.Attribute + "," + attrVar + "),dleq(" + attrVar + "," + root.Attribute.PossibleValues[i] + ")";
                                }
                                break;
                        default :
                            ourPrecond = precond + ",dataset(X," + root.Attribute + "," + root.Attribute.PossibleValues[i] + ")";
                           break;
                    }
                    string subcode = PrologPrintNode(childNode, ourPrecond, targetAttribute);
                    if (subcode.Length >3) returnString += subcode;
                    if (childNodeRelation!="eq")
                    {
                        TreeNode childNode2 = root.GetChildByBranchName2(root.Attribute.PossibleValues[i]);

                        string childNodeRelation2 = root.GetChildRelationByBranchName2(root.Attribute.PossibleValues[i]);
                        string ourPrecond2 = "";
                        switch (childNodeRelation2)
                        {
                            case "eq":
                                ourPrecond2 = precond + ",dataset(X," + root.Attribute + "," + root.Attribute.PossibleValues[i] + ")";
                                break;
                            case "gt":
                                if (precond.Contains(attrVar))
                                {
                                    ourPrecond2 = precond + ",dgtr(" + attrVar + "," + root.Attribute.PossibleValues[i] + ")";
                                }
                                else
                                {
                                ourPrecond2 = precond + ",dataset(X," + root.Attribute + "," + attrVar + "),dgtr(" + attrVar + "," + root.Attribute.PossibleValues[i] + ")";
                                }
                                break;
                            case "leq":
                                if (precond.Contains(attrVar))
                                {
                                    ourPrecond2 = precond + ",dleq(" + attrVar + "," + root.Attribute.PossibleValues[i] + ")";
                                }
                                else
                                {
                                ourPrecond2 = precond + ",dataset(X," + root.Attribute + "," + attrVar + "),dleq(" + attrVar + "," + root.Attribute.PossibleValues[i] + ")";
                                }
                                break;
                            default:
                                ourPrecond2 = precond + ",dataset(X," + root.Attribute + "," + root.Attribute.PossibleValues[i] + ")";
                                break;
                        }
                        subcode = PrologPrintNode(childNode2, ourPrecond2, targetAttribute);
                        if (subcode.Length > 3) returnString += subcode;
                    }
                
                }
            }

            return returnString;
        }
    }
    /// <summary>
    /// Class that represent the decision tree mounted; 
    /// </summary>
    public class TreeNode
    {
        TreeNodeCollection _children;
        TreeNodeCollection _children2;
        private TreeAttribute _attribute;
        PossibleValueRelationCollection _childRelations;
        PossibleValueRelationCollection _childRelations2;
        //public string relation = "eq";

        /// <summary>
        /// Initializes a new instance of the TreeNode 
        /// </summary>
        /// <param name="attribute">attribute to which the node is connected </param>
        public TreeNode(TreeAttribute attribute)
        {
            if (attribute != null && attribute.PossibleValues != null)
            {
                _children = new TreeNodeCollection();
                _children2 = new TreeNodeCollection();
                _childRelations = new PossibleValueRelationCollection();
                _childRelations2 = new PossibleValueRelationCollection();
                for (int i = 0; i < attribute.PossibleValues.Count; i++)
                {
                    _children.Add(null);
                    _children2.Add(null);
                    _childRelations.Add("eq");
                    _childRelations2.Add("eq");
                }
            }
            else
            {
                _children = new TreeNodeCollection();
                _children.Add(null);
                _children2 = new TreeNodeCollection();
                _children2.Add(null);
                _childRelations = new PossibleValueRelationCollection();
                _childRelations2 = new PossibleValueRelationCollection();
                _childRelations.Add("eq");
                _childRelations2.Add("eq");
            }
            _attribute = attribute;
        }

        /// <summary>
        /// Adds a child TreeNode TreeNode in this branch by name indicated ValueName
        /// </summary>
        /// <param name="treeNode">TreeNode child to be added </param>
        /// <param name="ValueName">name branch where the TreeNode is created </param>
        public void AddTreeNode(TreeNode treeNode, string ValueName,string rel)
        {
            int index = _attribute.indexValue(ValueName);
            if ((rel =="eq") ||(rel=="leq"))
            {
            _children[index] = treeNode;
            _childRelations[index] = rel;
            }
            else
            {
                _children2[index] = treeNode;
                _childRelations2[index] = rel;
            }
        }

        /// <summary>
        /// Returns the total number of child nodes 
        /// </summary>
        public int ChildrenCount
        {
            get
            {
                return _children.Count;
            }
        }

        /// <summary>
        /// Returns the child node of a node 
        /// </summary>
        /// <param name="index">Index child node </param>
        /// <returns> An object of class representing TreeNode node</returns>
        public TreeNode GetChildAt(int index)
        {
            return _children[index];
        }

        /// <summary>
        /// Attribute that is connected to Node
        /// </summary>
        public TreeAttribute Attribute
        {
            get
            {
                return _attribute;
            }
        }

        /// <summary>
        /// Returns the child of a node with the name of the branch that leads to him 
        /// </summary>
        /// <param name="branchName">branch name </param>
        /// <returns> The node </returns>
        public TreeNode GetChildByBranchName(string branchName)
        {
            int index = _attribute.indexValue(branchName);
            return _children[index];
        }
        public string GetChildRelationByBranchName(string branchName)
        {
            int index = _attribute.indexValue(branchName);
            return _childRelations[index];
        }
        public TreeNode GetChildByBranchName2(string branchName)
        {
            int index = _attribute.indexValue(branchName);
            return _children2[index];
        }
        public string GetChildRelationByBranchName2(string branchName)
        {
            int index = _attribute.indexValue(branchName);
            return _childRelations2[index];
        }
    }
    /// <summary>
    /// Class that represents an attribute used in the class of decision
    /// </summary>
    public class TreeAttribute
    {
        protected PossibleValueCollection _possibleValues;
        protected string _name;
        protected object _label;

        /// <summary>
        /// Initializes a new instance of a class Attribute 
        /// </summary>
        /// <param name="name">Indicates the attribute name</param>
        /// <param name="values">indicates the possible values ​​for the attribute</param>
        public TreeAttribute(string name, PossibleValueCollection possibleValues)
        {
            _name = name;

            if (possibleValues == null)
                _possibleValues = null;
            else
            {
                _possibleValues = possibleValues;
                _possibleValues.Sort();
            }
        }



        /// <summary>
        /// Indicates the attribute name
        /// </summary>
        public string AttributeName
        {
            get
            {
                return _name;
            }
        }

        /// <summary>
        /// Returns an array of attribute values 
        /// </summary>
        public PossibleValueCollection PossibleValues
        {
            get
            {
                return _possibleValues;
            }
        }

        /// <summary>
        /// Indicates whether a value is allowed for this attribute
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool isValidValue(string value)
        {
            return indexValue(value) >= 0;
        }

        /// <summary>
        /// Returns the index of a value
        /// </summary>
        /// <param name="value">Value to be returned</param>
        /// <returns>The index value on which the value position is </returns>
        public int indexValue(string value)
        {
            if (_possibleValues != null)
                return _possibleValues.BinarySearch(value);
            else
                return -1;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            if (_name != string.Empty)
            {
                return _name;
            }
            else
            {
                return _label.ToString();
            }
        }
    }
    public class TreeAttributeCollection : List<TreeAttribute>
    {
        public bool ContainsAttribute(TreeAttribute treeAttribute)
        {
            foreach (TreeAttribute a in this)
            {
                if (a.AttributeName.Trim().ToLower() == treeAttribute.AttributeName.Trim().ToLower())
                    return true;
            }
            return false;
        }


    }
    public class TreeNodeCollection : List<TreeNode>
    {
    }
    public class PossibleValueCollection : List<string>
    {
    }
    public class PossibleValueRelationCollection : List<string>
    {
    }
    public class OutcomeTreeAttribute : TreeAttribute
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="Label"></param>
        public OutcomeTreeAttribute(object Label)
            : base(String.Empty, null)
        {
            _label = Label;
            _name = string.Empty;
            _possibleValues = null;
        }
    }
    public class MtDataSource : DataTable
    {
        // Assumes that the MT will contain a data access predicate
        //  dataset(ID,attribute,value)
        

        string _sourceMt;
        SIProlog prologEngine = null;

        public MtDataSource(SIProlog pengine,string sourceMt)
        {
            _sourceMt = sourceMt;
            prologEngine = pengine;
            load();
        }

        public MtDataSource()
        {
            _sourceMt = String.Empty;
        }

        private void load()
        {
            this.TableName = "samples";
            string[] columns;
            string query = "";
            Dictionary <string,int> IDTable = new Dictionary<string,int> ();
            Dictionary<string, int> AttributeTable = new Dictionary<string, int>();
            Dictionary<string, string> IDV = new Dictionary<string, string>();

            List<Dictionary<string, string>> bingingsList = new List<Dictionary<string, string>>();
            //Get collection of instances ID's and Attribute Names
            query = "dataset(ID,ATTRIBUTE,VALUE)";
            prologEngine.askQuery(query, _sourceMt, out bingingsList);
            foreach (Dictionary<string, string> bindings in bingingsList)
            {
                foreach (string k in bindings.Keys)
                {
                    if (k == "ID") IDTable[bindings[k]] = 1;
                    if (k == "ATTRIBUTE") AttributeTable[bindings[k]] = 1;
                }
                string IDVKey = bindings["ID"] + "_" + bindings["ATTRIBUTE"];
                IDV[IDVKey] = bindings["VALUE"];
            }
           // Scan to create the header
            foreach (string attr in AttributeTable.Keys)
            {
                this.Columns.Add(attr.Trim());
            }
         // Scan to create the dataset
            foreach (string id in IDTable.Keys)
            {
                DataRow row = this.NewRow();
                foreach (string attr in AttributeTable.Keys)
                {
                    string IDVKey =id + "_" + attr;
                    string val = "unknown";
                    if (IDV.ContainsKey(IDVKey)) val=IDV[IDVKey];
                    row[attr] = val;
                }
                this.Rows.Add(row);
            }

        }
        public PossibleValueCollection GetValuesFromColumn(string columnName)
        {
            PossibleValueCollection returnList = new PossibleValueCollection();
            foreach (DataRow row in this.Rows)
            {
                foreach (DataColumn column in this.Columns)
                {
                    if (column.ColumnName.ToLower() == columnName.ToLower().Trim())
                    {
                        if (!returnList.Contains(row[column].ToString()))
                            returnList.Add(row[column].ToString());
                    }
                }
            }
            return returnList;
        }

        public TreeAttributeCollection GetValidAttributeCollection(string targetAttribute)
        {
            TreeAttributeCollection returnCollection = new TreeAttributeCollection();

            foreach (DataColumn column in this.Columns)
            {
                TreeAttribute currentAttribute = new TreeAttribute(column.ColumnName, GetValuesFromColumn(column.ColumnName));

                if (returnCollection.ContainsAttribute(currentAttribute) || currentAttribute.AttributeName.ToUpper().Trim() == targetAttribute.ToUpper().Trim())
                    continue;
                returnCollection.Add(currentAttribute);
            }
            return returnCollection;

        }
    }
    public class RawDataSource : DataTable
    {
        string _sourcePathAndFile;

        public RawDataSource(string sourcePathAndFile)
        {
            _sourcePathAndFile = sourcePathAndFile;
            load();
        }

        public RawDataSource()
        {
            _sourcePathAndFile = String.Empty;
        }
        
        private void load()
        {
            this.TableName = "samples";

            string[] lines = File.ReadAllLines(_sourcePathAndFile);
            string[] columns;

            for (int k = 0; k < lines.Length; k++)
            {
                string line = lines[k].ToLower();

                if (k == 0)
                {
                    columns = line.Split(',');

                    for (int j = 0; j < columns.Length; j++)
                    {
                        if (!this.Columns.Contains(columns[j]))
                            this.Columns.Add(columns[j].Trim());
                    }
                    continue;
                }

                DataRow row = this.NewRow();

                columns = line.Split(',');

                for (int i = 0; i < columns.Length; i++)
                {
                    string val = columns[i].Trim().ToLower();
                    val = val.Replace("\"", String.Empty);

                    if (val.ToUpper() == "TRUE" || val.ToUpper() == "FALSE")
                    {
                        bool booleanValue = bool.Parse(val);
                        row[i] = booleanValue;
                        continue;
                    }
                    row[i] = val;
                }
                this.Rows.Add(row);
            }
        }

        public PossibleValueCollection GetValuesFromColumn(string columnName)
        {
            PossibleValueCollection returnList = new PossibleValueCollection();
            foreach (DataRow row in this.Rows)
            {
                foreach (DataColumn column in this.Columns)
                {
                    if (column.ColumnName.ToLower() == columnName.ToLower().Trim())
                    {
                        if (!returnList.Contains(row[column].ToString()))
                            returnList.Add(row[column].ToString());
                    }
                }
            }
            return returnList;
        }

        public TreeAttributeCollection GetValidAttributeCollection()
        {
            TreeAttributeCollection returnCollection = new TreeAttributeCollection();

            foreach (DataColumn column in this.Columns)
            {
                TreeAttribute currentAttribute = new TreeAttribute(column.ColumnName, GetValuesFromColumn(column.ColumnName));

                if (returnCollection.ContainsAttribute(currentAttribute) || currentAttribute.AttributeName.ToUpper().Trim() == "RESULT")
                    continue;
                returnCollection.Add(currentAttribute);
            }
            return returnCollection;

        }
    }
}
