using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Dame
{
    class HistoryEventArgs: EventArgs
    {
        public List<string> History { get; private set; }

        public HistoryEventArgs(IEnumerable<string> list)
        {
            History = new List<string>(list);
        }
    }
}
