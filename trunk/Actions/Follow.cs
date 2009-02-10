using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;
using System.Threading;
using System.Windows.Forms;
using cogbot.TheOpenSims;

namespace cogbot.Actions
{
    class Follow : Action
    {
        public SimObject followAvatar;
        public float followDist;
        //public Vector3 LastTarget;
        public Thread thrTracker=null;

        public Follow(BotClient Client)
            : base(Client)
        {
            helpString = "Start or stop following a user.";
            usageString = "To start following an avatar, type \"follow <avatar name>\" \r\n" +
                          "To stop following an avatar, type \"stop-following <avatar name>\"";

            followAvatar = null;
            followDist = 3;
            //Client.Settings.AVATAR_TRACKING = true;
            //Client.Settings.ALWAYS_REQUEST_OBJECTS = true;
            Client.Settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = false;
            Client.Settings.SEND_AGENT_UPDATES = true;
            //Client.Settings.THROTTLE_OUTGOING_PACKETS = false;
            //Client.Settings.SEND_AGENT_THROTTLE = true;

           // Client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);

        }


        bool StartedFlying = false;
        void tracker()
        {
            try
            {
                Random somthing = new Random(Environment.TickCount);// We do stuff randomly here
                Boolean justStopped = false;
                while (true)
                {
                    if (followAvatar != null)
                    {
                        Vector3 targetPosition = new Vector3( followAvatar.GetSimPosition());
                        float ZDist = Math.Abs(targetPosition.Z - Client.Self.SimPosition.Z);

                        // allow flight
                        if (ZDist > followDist)
                        {
                            if (!Client.Self.Movement.Fly)
                            {
                                if (!StartedFlying)
                                {
                                    Client.Self.Fly(true);
                                    StartedFlying = true;
                                }
                            }
                        }
                        else
                        {
                            if (StartedFlying)
                            {
                                Client.Self.Fly(false);
                                StartedFlying = false;
                            }
                        }

                        



                        float curDist = Vector3.Distance(Client.Self.SimPosition, targetPosition);
                        if (curDist > followDist)
                        {

                            //Client.Self.Movement.SendUpdate();
                            if (curDist < (followDist * 1.25))
                            {
                                Client.Self.Movement.TurnToward(targetPosition);
                                Client.Self.Movement.AtPos = true;
                                Thread.Sleep(25);
                                Client.Self.Movement.Stop = true;
                                Client.Self.Movement.AtPos = false;
                                Client.Self.Movement.NudgeAtPos = false;
                                Client.Self.Movement.SendUpdate(false);

                                Thread.Sleep(100);
                            }
                            else
                            {                          
                                Client.Self.Movement.TurnToward(targetPosition);
                                Client.Self.Movement.AtPos = true;
                                Client.Self.Movement.UpdateInterval = 0; //100
                                Client.Self.Movement.SendUpdate(false);
                                Application.DoEvents();
                                //(int)(25 * (1 + (curDist / followDist)))
                                Thread.Sleep(somthing.Next(25, 100));
                            }
                            justStopped = true;
                        }
                        else
                        {
                            if (justStopped)
                            {
                                Client.Self.Movement.TurnToward(targetPosition);
                                Client.Self.Movement.AtPos = false;
                                //Client.Self.Movement.UpdateInterval = 0;
                                Client.Self.Movement.StandUp = true;
                                //Client.Self.Movement.SendUpdate();
                                Client.Self.Movement.FinishAnim = true;
                                Client.Self.Movement.Stop = true;
                                Client.Self.Movement.SendUpdate(false);
                                Thread.Sleep(25);
                                WorldSystem.TheSimAvatar.StopMoving();
                                justStopped = false;
                            }
                            else
                            {
                                Thread.Sleep(100);
                            }


                        }

                    }
                    else
                    {
                        Thread.Sleep(100);
                        return; // if followAvatar is null then we're not interested anymore 
                    }
                }
            }
            finally
            {
                try
                {
                    WorldSystem.TheSimAvatar.StopMoving();
                }
                catch (Exception e) { }
            }
        }

        void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (followAvatar != null)
            {
              //  if (Vector3.Distance(Client.Self.SimPosition, targetPosition) > followDist)
                {
                    //if (Vector3.Dist(LastTarget, targetPosition) > 1)
                    //{
                    //   LastTarget = targetPosition;
                    //    Client.Self.Movement.TurnToward(targetPosition);
                    //    Client.Self.Movement.AtPos = true;
                    //    //Client.Self.AutoPilotCancel();
                    //      Client.Self.Movement.UpdateInterval = 0;
                    //    Client.Self.Movement.SendUpdate();
                    //}
                    //      Client.Self.AutoPilotLocal((int)targetPosition.X,
                    //          (int)targetPosition.Y, targetPosition.Z);
                }
                //else
                {
                    //Client.Self.AutoPilotCancel();
                }
            }
        }

        public override void acceptInput(string verb, Parser args)
        {
           // base.acceptInput(verb, args);
            if (verb == "follow")
            {
                string name = args.objectPhrase;
                if (String.IsNullOrEmpty(name.Trim())) name = "avatar";
                Primitive avatar;
                if (Client.WorldSystem.tryGetPrim(name, out avatar))
                {
                    followAvatar = WorldSystem.GetSimObject( avatar);
                    followDist = followAvatar.GetSizeDistance();
                    String str = ""+Client + " start to follow " + followAvatar + ".";
                    WriteLine(str);
                    // The thread that accepts the Client and awaits messages
                    thrTracker = new Thread(tracker);
                    thrTracker.Name = str;
                    lock (Client.botCommandThreads) Client.botCommandThreads.Add(thrTracker);
                    // needs to interupt prevoius goals
                    WorldSystem.TheSimAvatar.StopMoving();
                    // The thread calls the tracker() method
                    thrTracker.Start();
                }
                else
                {
                    WriteLine("I don't know who " + name + " is.");
                }
            }
            else if (verb == "stop-following") {
                if (followAvatar != null)
                {
                    WriteLine("You stop following " + followAvatar + ".");
                    followAvatar = null;
                }
                else
                {
                    WriteLine("You aren't following anyone.");
                }
                
            }

            Client.describeNext = true;
        }
    }
}
