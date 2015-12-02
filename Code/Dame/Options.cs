using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SbsSW.SwiPlCs;

namespace Dame
{
    static class Options
    {
        private static bool IsSaved;
        public static int SearchDepth { get; set; }

        public static void SetOptions()
        {
            if (IsSaved)
                return;
            
        }
    }
}
