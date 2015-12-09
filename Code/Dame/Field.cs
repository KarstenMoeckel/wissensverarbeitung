using SbsSW.SwiPlCs;
using System;

namespace Dame
{
    class Field
    {
        public int Row { get; private set; }
        public int Column { get; private set; }
        
        public Field(int row, int col)
        {
            Row = row;
            Column = col;
        }

        public Field(PlTerm term)
        {
            Row = Convert.ToInt32(term[1].ToString());
            Column = Convert.ToInt32(term[2].ToString());
        }

        public PlTerm ToTerm()
        {
            return new PlTerm(string.Format("field({0},{1})", Row, Column));
        }
    }
}
