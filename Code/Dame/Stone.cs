using SbsSW.SwiPlCs;
using System;

namespace Dame
{
    class Stone
    {
        public Field Field { get; private set; }
        public StoneColor Color { get; private set; }
        public StoneType Type { get; private set; }
        
        public Stone(PlTermV stoneVector)
        {
            Field = new Field(stoneVector[0]);
            Color = (StoneColor)Enum.Parse(typeof(StoneColor), stoneVector[1].ToString(), true);
            Type = (StoneType)Enum.Parse(typeof(StoneType), stoneVector[2].ToString(), true);
        }
    }
}
