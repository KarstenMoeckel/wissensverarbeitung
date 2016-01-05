using SbsSW.SwiPlCs;
using System;

namespace Dame
{
    public class Stone
    {
        public Field Field { get; private set; }
        public StoneColor Color { get; private set; }
        public StoneType Type { get; private set; }
        
        public Stone(PlTerm stoneTerm)
        {
            Field = new Field(stoneTerm[1]); // stoneTerm[0] is "stone"
            Color = (StoneColor)Enum.Parse(typeof(StoneColor), stoneTerm[2].ToString(), true);
            Type = (StoneType)Enum.Parse(typeof(StoneType), stoneTerm[3].ToString(), true);
        }
    }
}
