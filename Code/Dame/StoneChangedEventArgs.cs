using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Dame
{
    class StoneChangedEventArgs: EventArgs
    {
        public List<Stone> Stones { get; private set; }

        public StoneChangedEventArgs(IEnumerable<Stone> list)
        {
            Stones = new List<Stone>(list);
        }
    }
}
