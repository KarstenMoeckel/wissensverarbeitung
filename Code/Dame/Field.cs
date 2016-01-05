using SbsSW.SwiPlCs;
using System;

namespace Dame
{
    public struct Field
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

        public static bool operator == (Field f1, Field f2)
        {
            return f1.Equals(f2);
        }

        public static bool operator !=(Field f1,Field f2)
        {
            return !(f1.Equals(f2));
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Field))
                return false;

            Field f = (Field)obj;
            return f.Column == Column && f.Row == Row;
        }

        public override int GetHashCode()
        {
            int sum = Row * 10 + Column;
            return sum.GetHashCode();
        }
    }
}
