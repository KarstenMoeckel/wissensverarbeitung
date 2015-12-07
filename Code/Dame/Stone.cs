using SbsSW.SwiPlCs;
using System;

namespace Dame
{
    class Stone
    {
        public int Row { get; private set; }
        public int Column { get; private set; }
        public StoneColor Color { get; private set; }
        public StoneType Type { get; private set; }
        
        public Stone(PlTermV stoneVector)
        {
            Row = Convert.ToInt32(stoneVector[0].ToString());
            Column = Convert.ToInt32(stoneVector[1].ToString());
            Color = (StoneColor)Enum.Parse(typeof(StoneColor), stoneVector[2].ToString(), true);
            Type = (StoneType)Enum.Parse(typeof(StoneType), stoneVector[3].ToString(), true);
        }
    }
}
