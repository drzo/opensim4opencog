using System;
using System.Collections.Generic;
using System.Text;
using OpenMetaverse; //using libsecondlife;
using System.Threading;

namespace cogbot.Actions
{
    class Follow : Action
    {
        public Avatar followAvatar;
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

            Client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);

        }

        void tracker()
        {
          Random somthing = new Random(Environment.TickCount);// We do stuff randomly here
          Boolean justStopped = false;
            while (true)
            {
                if (followAvatar != null)
                {
                    float curDist = Vector3.Distance(Client.Self.SimPosition, followAvatar.Position);
                    if (curDist > followDist)
                    {

                        //Client.Self.Movement.SendUpdate();
                        if (curDist < (followDist * 1.25))
                        {
                                Client.Self.Movement.TurnToward(followAvatar.Position);
                                Client.Self.Movement.AtPos = true;
                                Thread.Sleep(25);
                                Client.Self.Movement.Stop = true;
                                Client.Self.Movement.AtPos = false;
                                Client.Self.Movement.NudgeAtPos = false;
                            
                            Thread.Sleep(100);
                        }
                        else
                        {
                            Client.Self.Movement.TurnToward(followAvatar.Position);
                            Client.Self.Movement.AtPos = true;
                            Client.Self.Movement.UpdateInterval = 0; //100
                            //(int)(25 * (1 + (curDist / followDist)))
                            Thread.Sleep(somthing.Next(25, 100));
                        }
                        justStopped = true;
                    }
                    else
                    {
                        if (justStopped)
                        {
                            Client.Self.Movement.TurnToward(followAvatar.Position);
                            Client.Self.Movement.AtPos = false;
                            //Client.Self.Movement.UpdateInterval = 0;
                            Client.Self.Movement.StandUp = true;
                            //Client.Self.Movement.SendUpdate();
                            Client.Self.Movement.FinishAnim = true;
                            Client.Self.Movement.Stop = true;
                            
                            Thread.Sleep(25);
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

        void Objects_OnObjectUpdated(Simulator simulator, ObjectUpdate update, ulong regionHandle, ushort timeDilation)
        {
            if (followAvatar != null)
            {
                if (Vector3.Distance(Client.Self.SimPosition, followAvatar.Position) > followDist)
                {
                    //if (Vector3.Dist(LastTarget, followAvatar.Position) > 1)
                    //{
                    //   LastTarget = followAvatar.Position;
                    //    Client.Self.Movement.TurnToward(followAvatar.Position);
                    //    Client.Self.Movement.AtPos = true;
                    //    //Client.Self.AutoPilotCancel();
                    //      Client.Self.Movement.UpdateInterval = 0;
                    //    Client.Self.Movement.SendUpdate();
                    //}
                    //      Client.Self.AutoPilotLocal((int)followAvatar.Position.X,
                    //          (int)followAvatar.Position.Y, followAvatar.Position.Z);
                }
                else
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
                Avatar avatar;
                if (Client.WorldSystem.tryGetAvatar(name, out avatar))
                {
                    followAvatar = avatar;
                    WriteLine("You start to follow " + followAvatar.Name + ".");
                    // The thread that accepts the Client and awaits messages
                    thrTracker= new Thread(tracker);
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
                    WriteLine("You stop following " + followAvatar.Name + ".");
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
