using SbsSW.SwiPlCs;

namespace Dame
{
    partial class Engine
    {
        public class Option
        {
            private Difficulty _Difficulty;
            private StoneColor _Player;
            private StoneColor _StartColor;
            private bool Changed;

            public Difficulty Difficulty
            {
                get
                {
                    return _Difficulty;
                }
                set
                {
                    if (_Difficulty != value)
                    {
                        Changed = true;
                        _Difficulty = value;
                    }
                }
            }
            public StoneColor Player
            {
                get
                {
                    return _Player;
                }
                set
                {
                    if (_Player != value)
                    {
                        Changed = true;
                        _Player = value;
                    }
                }
            }
            public StoneColor StartColor
            {
                get
                {
                    return _StartColor;
                }
                set
                {
                    if (_StartColor != value)
                    {
                        Changed = true;
                        _StartColor = value;
                    }
                }
            }
            internal Option()
            {
                _Difficulty = Difficulty.Medium;
                _Player = StoneColor.White;
                _StartColor = StoneColor.Black;
                Changed = true;
            }

            internal void Save()
            {
                if (!Changed)
                    return;
                PlQuery.PlCall("retractall(option(_,_))");
                PlQuery.PlCall(string.Format("assertz(option(searchDepth,{0}))", (int)Difficulty));
                PlQuery.PlCall(string.Format("assertz(option(player,{0}))", Player.ToString().ToLower()));
                PlQuery.PlCall(string.Format("assertz(option(startColor,{0}))", StartColor.ToString().ToLower()));
                Changed = false;
            }
        }
    }
}