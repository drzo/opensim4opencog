using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;
using System.Xml;

namespace AltAIMLbot.Utils
{
    [Serializable]
    public class RandomMemory
    {
        Dictionary<string,double> myMem = null;
        public double Halflife = 180000;
        public double smoother = 0.00001;
        Random r = new Random();

        public RandomMemory()
        {
            myMem = new Dictionary<string, double>();
        }

        public void registerPick(string choice)
        {
            myMem[choice] = Environment.TickCount;
        }

        public double value(string choice)
        {
            double timeInterval;
            double LastUpdate = 0;

            if (myMem.ContainsKey(choice))
            {
                try
                {
                    LastUpdate = myMem[choice];
                }
                catch
                {
                    LastUpdate = 0;
                }
            }

            timeInterval = Environment.TickCount - LastUpdate;
            double CV = Math.Pow(0.5, (timeInterval / Halflife));
            if (CV < smoother) { CV = smoother;} 

            return CV;

        }

        public string selectOne(List<XmlNode> randomList)
        {
            XmlNode chosenNode = (XmlNode)randomList[r.Next(randomList.Count)];
            string result = chosenNode.InnerXml;

            double weight_sum = 0;
            double accum = 0;
            foreach (XmlNode testNode in randomList)
            {
                double weight = 1 - value(testNode.InnerXml); // recent will be close to 1
                weight_sum += weight;
            }

            // Roulette selection, pick one and return
            double randomPoint = r.NextDouble()*weight_sum;
            foreach (XmlNode testNode in randomList)
            {
                double weight = 1 - value(testNode.InnerXml); // 
                accum += weight;
                if (accum >= randomPoint)
                {
                    result = testNode.InnerXml;
                    registerPick(result);
                    return result;
                }
            }
            
            return result;
        }

        public XmlNode selectOneXML(XmlNode randomNode)
        {
            int selected = r.Next(randomNode.ChildNodes.Count);
            XmlNode resultNode = randomNode.ChildNodes[selected];

            double weight_sum = 0;
            double accum = 0;
            foreach (XmlNode testNode in randomNode.ChildNodes)
            {
                double weight = 1 - value(testNode.InnerXml); // recent will be close to 1
                weight_sum += weight;
            }

            // Roulette selection, pick one and return
            double randomPoint = r.NextDouble() * weight_sum;
            foreach (XmlNode testNode in randomNode.ChildNodes)
            {
                double weight = 1 - value(testNode.InnerXml); // 
                accum += weight;
                if (accum >= randomPoint)
                {
                    string result = testNode.InnerXml;
                    registerPick(result);
                    return testNode;
                }
            }

            return resultNode;
        }

        public int selectOneXMLIndex(XmlNode randomNode)
        {
            int selected = r.Next(randomNode.ChildNodes.Count);
            XmlNode resultNode = randomNode.ChildNodes[selected];

            double weight_sum = 0;
            double accum = 0;
            foreach (XmlNode testNode in randomNode.ChildNodes)
            {
                double weight = 1 - value(testNode.InnerXml); // recent will be close to 1
                weight_sum += weight;
            }

            // Roulette selection, pick one and return
            double randomPoint = r.NextDouble() * weight_sum;
            int index = -1;
            int maxindex = randomNode.ChildNodes.Count-1;
            foreach (XmlNode testNode in randomNode.ChildNodes)
            {
                index++;
                double weight = 1 - value(testNode.InnerXml); // 
                accum += weight;
                if (accum >= randomPoint)
                {
                    string result = testNode.InnerXml;
                    registerPick(result);

                    return index;
                }
            }

            return index;
        }

    }
}
