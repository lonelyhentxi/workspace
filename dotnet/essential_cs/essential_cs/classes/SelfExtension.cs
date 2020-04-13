using System;
using System.Collections.Generic;
using System.Text;

namespace essential_cs.classes
{

    public static class SelfExtension
    {
        public static bool ExtendedIsSelf(this Self self)
        {
            return self.IsSelf();
        }
    }
}
