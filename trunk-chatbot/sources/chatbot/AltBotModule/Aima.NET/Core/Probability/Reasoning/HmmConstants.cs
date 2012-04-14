using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aima.Core.Probability.Reasoning
{
    public static class HmmConstants {
        public static readonly string PushDoor = "push";

        public static readonly string DoNothing = "do_nothing";

        public static readonly string DoorClosed = "closed";

        public static readonly string DoorOpen = "open";

        public static readonly string SeeDoorClosed = "see_closed";

        public static readonly string SeeDoorOpen = "see_open";

        public static readonly string Raining = "rain";

        public static readonly string NotRaining = "no_rain";

        public static readonly string SeeUmbrella = "carries_umbrella";

        public static readonly string SeeNoUmbrella = "does_not_carry_umbrella";
    }
}
