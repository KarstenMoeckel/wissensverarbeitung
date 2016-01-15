using System;
using System.Collections.Generic;
using System.Linq;
using SbsSW.SwiPlCs;
using System.Threading;
using System.ComponentModel;

namespace Dame
{
    public partial class Engine: IDisposable, INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;
        public event EventHandler GameOver;

        private Thread historyThread;
        private Thread stoneThread;
        private Thread gameThread;
        private bool running;
        private IEnumerable<string> _History;
        private IEnumerable<Stone> _Stones;
        private IEnumerable<Field> _PossibleHits;
        private Field _StoneToMove;

        public Option Options { get; private set; }
        public IEnumerable<string> History
        {
            get { return _History; }
            private set
            {
                _History = value;
                FirePropertyChanged(nameof(History));
            }
        }
        public IEnumerable<Stone> Stones
        {
            get { return _Stones; }
            private set
            {
                _Stones = value;
                FirePropertyChanged(nameof(Stones));
            }
        }
        public Field StoneToMove
        {
            get { return _StoneToMove; }
            set
            {
                _StoneToMove = value;
                FirePropertyChanged(nameof(StoneToMove));
            }
        }
        public Field MoveDestination { get; set; }
        public IEnumerable<Field> PossibleHits
        {
            get { return _PossibleHits; }
            set
            {
                _PossibleHits = value;
                FirePropertyChanged(nameof(PossibleHits));
            }
        }

        public Engine()
        {
            historyThread = new Thread(new ThreadStart(CheckHistory_Thread));
            stoneThread = new Thread(new ThreadStart(CheckStones_Thread));
            Options = new Option();
        }

        private void FirePropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        //start engine
        public void Init()
        {
            running = true;
            historyThread.Start();
            stoneThread.Start();
        }

        //start game
        public bool Start()
        {
            Options.Save();
            if (PlQuery.PlCall("main:startGame"))
            {
                gameThread = new Thread(new ThreadStart(runGame_Thread));
                gameThread.Start();
                return true;
            }
            return false;
        }

        public bool LoadFile(string file)
        {
            bool result = PlQuery.PlCall("loadStartPos", new PlTermV(new PlTerm("'" + file.Replace('\\','/') + "'")));
            return result;
        }

        private void doAIMove()
        {
            if (!PlQuery.PlCall("main:aiNextMove"))
                return;
            while (PlQuery.PlCall("main:performAiMove"))
                Thread.Sleep(1000);
        }

        private void doHumanMove()
        {
            while (true)
            {
                while (StoneToMove == default(Field) || MoveDestination == default(Field))
                    Thread.Sleep(250);
                Field destination = MoveStone(StoneToMove, MoveDestination);
                MoveDestination = default(Field);
                if (destination != default(Field))
                {
                    while ((PossibleHits = MoreHitsPossible(destination)).Count() != 0)
                    {
                        StoneToMove = destination;
                        while (MoveDestination == default(Field))
                        {
                            Thread.Sleep(250);
                            if (MoveDestination != default(Field))
                            {
                                destination = MoveStone(destination, MoveDestination);
                                if (destination != default(Field))
                                    break;
                                MoveDestination = default(Field);
                                destination = StoneToMove;
                            }
                        }
                        MoveDestination = default(Field);
                    }
                    StoneToMove = default(Field);
                    break;
                }
            }
        }

        //logic for controlling game
        private void runGame_Thread()
        {
            PlEngine.PlThreadAttachEngine();
            while(PlQuery.PlCall("main:gameRunning"))
            {
                if(PlQuery.PlCall("main:isAIMove"))
                    doAIMove();
                else
                    doHumanMove();
                StartNextTurn();
            }
            GameOver?.Invoke(this, new EventArgs());
        }

        //get logs
        private void CheckHistory_Thread()
        {
            PlEngine.PlThreadAttachEngine();
            while (running)
            {
                using (PlQuery query = new PlQuery("getLog(Log)"))
                {
                    PlTermV termV = query.Solutions.FirstOrDefault();
                    if (termV.Size != 0)
                    {
                        History = termV[0].ToListString();
                    }
                }
                Thread.Sleep(250);
            }
            PlEngine.PlThreadDestroyEngine();
        }

        //get stones
        private void CheckStones_Thread()
        {
            PlEngine.PlThreadAttachEngine();
            while (running)
            {
                using (PlQuery query = new PlQuery("getStoneList(Stones)"))
                {
                    PlTermV termV = query.Solutions.FirstOrDefault();
                    if (termV.Size != 0)
                    {
                        Stones = termV[0].ToList().Select((t) => new Stone(t)).ToList();
                    }
                }
                Thread.Sleep(250);
            }
            PlEngine.PlThreadDestroyEngine();
        }

        //suspend engine
        public void Stop()
        {
            running = false;
            gameThread?.Abort();
        }

        //check for more hits
        private IEnumerable<Field> MoreHitsPossible(Field source)
        {
            PlQuery query = new PlQuery("areMoreHitsPossible", new PlTermV(source.ToTerm(), new PlTerm("Hits")));
            PlTermV termV = query.Solutions.FirstOrDefault();
            if (termV.Size == 0)
                return new List<Field>();
            IEnumerable<PlTerm> list = termV[1].ToList();
            return list.Select<PlTerm, Field>((t) => new Field(t)).ToList();
        }

        //start next turn
        private bool StartNextTurn()
        {
            return PlQuery.PlCall("main:nextTurn");
        }

        //moves a stone (destination is propably not the real destination; real destination is return value)
        private Field MoveStone(Field source, Field destination)
        {
            string direction = GetDirection(source, destination);
            PlQuery query = new PlQuery("moveStone", new PlTermV(source.ToTerm(), new PlTerm(direction), new PlTerm("NewDestination")));
            PlTermV termV = query.Solutions.FirstOrDefault();
            return termV.Size == 0 ? default(Field) : new Field(termV[2]);
        }

        private string GetDirection(Field source, Field destination)
        {
            if (source.Row < destination.Row)
            {
                if (source.Column < destination.Column)
                    return "bottomRight";
                else
                    return "bottomLeft";
            }
            else
            {
                if (source.Column < destination.Column)
                    return "topRight";
                else
                    return "topLeft";
            }
        }

        #region IDisposable Support
        private bool disposedValue = false; // Dient zur Erkennung redundanter Aufrufe.

        protected virtual void Dispose(bool disposing)
        {
            if (!disposedValue)
            {
                if (disposing)
                {
                    running = false;
                    historyThread.Join();
                    stoneThread.Join();
                    gameThread?.Abort();
                }

                // TODO: nicht verwaltete Ressourcen (nicht verwaltete Objekte) freigeben und Finalizer weiter unten überschreiben.
                // TODO: große Felder auf Null setzen.

                disposedValue = true;
            }
        }

        // TODO: Finalizer nur überschreiben, wenn Dispose(bool disposing) weiter oben Code für die Freigabe nicht verwalteter Ressourcen enthält.
        // ~Engine() {
        //   // Ändern Sie diesen Code nicht. Fügen Sie Bereinigungscode in Dispose(bool disposing) weiter oben ein.
        //   Dispose(false);
        // }

        // Dieser Code wird hinzugefügt, um das Dispose-Muster richtig zu implementieren.
        public void Dispose()
        {
            // Ändern Sie diesen Code nicht. Fügen Sie Bereinigungscode in Dispose(bool disposing) weiter oben ein.
            Dispose(true);
            // TODO: Auskommentierung der folgenden Zeile aufheben, wenn der Finalizer weiter oben überschrieben wird.
            // GC.SuppressFinalize(this);
        }
        #endregion
    }
}
