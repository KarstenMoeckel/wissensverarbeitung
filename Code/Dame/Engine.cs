using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SbsSW.SwiPlCs;

namespace Dame
{
    class Engine
    {
        public void Start()
        {
            Options.SetOptions();
        }

        public IEnumerable<Stone> GetStoneList()
        {
            PlQuery query = new PlQuery("stone(Row,Col,Color,Type)");
            IEnumerable<PlTermV> result = query.Solutions;
            return result.Select<PlTermV, Stone>((v) => new Stone(v));
        }
    }
}
