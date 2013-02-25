namespace AltAIMLbot
{
    public class BehaviorContext
    {
        internal AltBot mbot;
        public void RemoveCurrentTask(TaskItem taskItem, TaskList taskList, bool b)
        {
           // throw new System.NotImplementedException();
        }

        public void AddCurrentTask(TaskItem taskItem, TaskList taskList, bool b)
        {
           // throw new System.NotImplementedException();
        }

        public void SetCurrentTask(TaskItem taskItem, TaskList taskList, bool b)
        {
           // throw new System.NotImplementedException();
        }

        public BTXmlDocument templateNode;

        public BehaviorSet myBehaviors
        {
            get { return mbot.myBehaviors; }
        }
    }
}