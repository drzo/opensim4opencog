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

        public Follow(TextForm parent)
            : base(parent)
        {
            helpString = "Start or stop following a user.";
            usageString = "To start following an avatar, type \"follow <avatar name>\" \r\n" +
                          "To stop following an avatar, type \"stop-following <avatar name>\"";

            followAvatar = null;
            followDist = 3;

            client.Objects.OnObjectUpdated += new ObjectManager.ObjectUpdatedCallback(Objects_OnObjectUpdated);

        }

        void tracker()
        {
          Random somthing = new Random(Environment.TickCount);// We do stuff randomly here
          Boolean justStopped = false;
            while (true)
            {
                if (followAvatar != null)
                {
                    float curDist = Vector3.Distance(client.Self.SimPosition, followAvatar.Position);
                    if (curDist > followDist)
                    {

                        //client.Self.Movement.SendUpdate();
                        if (curDist < (followDist * 1.25))
                        {
                                client.Self.Movement.TurnToward(followAvatar.Position);
                                client.Self.Movement.AtPos = true;
                                Thread.Sleep(25);
                                client.Self.Movement.Stop = true;
                                client.Self.Movement.AtPos = false;
                                client.Self.Movement.NudgeAtPos = false;
                            
                            Thread.Sleep(100);
                        }
                        else
                        {
                            client.Self.Movement.TurnToward(followAvatar.Position);
                            client.Self.Movement.AtPos = true;
                            client.Self.Movement.UpdateInterval = 0; //100
                            //(int)(25 * (1 + (curDist / followDist)))
                            Thread.Sleep(somthing.Next(25, 100));
                        }
                        justStopped = true;
                    }
                    else
                    {
                        if (justStopped)
                        {
                            client.Self.Movement.TurnToward(followAvatar.Position);
                            client.Self.Movement.AtPos = false;
                            //client.Self.Movement.UpdateInterval = 0;
                            client.Self.Movement.StandUp = true;
                            //client.Self.Movement.SendUpdate();
                            client.Self.Movement.FinishAnim = true;
                            client.Self.Movement.Stop = true;
                            
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
                if (Vector3.Distance(client.Self.SimPosition, followAvatar.Position) > followDist)
                {
                    //if (Vector3.Dist(LastTarget, followAvatar.Position) > 1)
                    //{
                    //   LastTarget = followAvatar.Position;
                    //    client.Self.Movement.TurnToward(followAvatar.Position);
                    //    client.Self.Movement.AtPos = true;
                    //    //client.Self.AutoPilotCancel();
                    //      client.Self.Movement.UpdateInterval = 0;
                    //    client.Self.Movement.SendUpdate();
                    //}
                    //      client.Self.AutoPilotLocal((int)followAvatar.Position.X,
                    //          (int)followAvatar.Position.Y, followAvatar.Position.Z);
                }
                else
                {
                    //client.Self.AutoPilotCancel();
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
                Listeners.Avatars avatars = (Listeners.Avatars)parent.listeners["avatars"];
                if (avatars.tryGetAvatar(name, out avatar))
                {
                    followAvatar = avatar;
                    parent.output("You start to follow " + followAvatar.Name + ".");
                    // The thread that accepts the client and awaits messages
                    thrTracker= new Thread(tracker);
                    // The thread calls the tracker() method
                    thrTracker.Start();

                }
                else
                {
                    parent.output("I don't know who " + name + " is.");
                }
            }
            else if (verb == "stop-following") {
                if (followAvatar != null)
                {
                    parent.output("You stop following " + followAvatar.Name + ".");
                    followAvatar = null;
                }
                else
                {
                    parent.output("You aren't following anyone.");
                }
                
            }

            parent.describeNext = true;
        }
    }
}
