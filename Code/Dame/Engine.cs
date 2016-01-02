using System;
using System.Collections.Generic;
using System.Linq;
using SbsSW.SwiPlCs;
using System.Threading;

namespace Dame
{
    partial class Engine: IDisposable
    {
        public event EventHandler<StoneChangedEventArgs> StonesChanged;
        public event EventHandler<HistoryEventArgs> HistoryChanged;

        private Thread historyThread;
        private Thread stoneThread;
        private bool running;
        public Option Options { get; private set; }
        
        public Engine()
        {
            historyThread = new Thread(new ThreadStart(CheckHistory_Thread));
            stoneThread = new Thread(new ThreadStart(CheckStones_Thread));
            Options = new Option();
        }

        public void Init()
        {
            running = true;
            historyThread.Start();
            stoneThread.Start();
        }

        public void Start()
        {
            Options.Save();
            PlQuery.PlCall("startGame");
        }

        public bool LoadFile(string file)
        {
            bool result = PlQuery.PlCall("loadStartPos", new PlTermV(new PlTerm("'" + file.Replace('\\','/') + "'")));
            //CheckStones_Thread();
            return result;
        }

        private void CheckHistory_Thread()
        {
            PlEngine.PlThreadAttachEngine();
            while (running)
            {
                PlQuery query = new PlQuery("getLog(Log)");
                PlTermV termV = query.Solutions.FirstOrDefault();
                if (termV != default(PlTermV))
                {
                    IEnumerable<string> list = termV[0].ToListString();
                    HistoryEventArgs args = new HistoryEventArgs(list);
                    HistoryChanged.Invoke(this, args);
                }
                Thread.Sleep(250);
            }
        }

        private void CheckStones_Thread()
        {
            PlEngine.PlThreadAttachEngine();
            while (running)
            {
                PlQuery query = new PlQuery("getStoneList(Stones)");
                PlTermV termV = query.Solutions.FirstOrDefault();
                if (termV != default(PlTermV))
                {
                    IEnumerable<PlTerm> list = termV[0].ToList();
                    StoneChangedEventArgs args = new StoneChangedEventArgs(list.Select<PlTerm, Stone>((t) => new Stone(t)));
                    StonesChanged.Invoke(this, args);
                }
                Thread.Sleep(250);
            }
        }

        public void Stop()
        {
            running = false;
        }

        public IEnumerable<Field> MoreHitsPossible(Field source)
        {
            PlQuery query = new PlQuery("moreMovesPossible", new PlTermV(source.ToTerm(), new PlTerm("Hits")));
            PlTermV termV = query.Solutions.FirstOrDefault();
            if (termV == null)
                return null;
            IEnumerable<PlTerm> list = termV[0].ToList();
            return list.Select<PlTerm, Field>((t) => new Field(t));
        }
        
        public void StartNextTurn()
        {
            PlQuery.PlCall("nextTurn");
        }

        public Field MoveStone(Field source, Field destination)
        {
            PlQuery query = new PlQuery("moveStone", new PlTermV(source.ToTerm(), destination.ToTerm(), new PlTerm("NewDestination")));
            PlTermV termV = query.Solutions.FirstOrDefault();
            return termV == null ? null : new Field(termV[0]);
        }

        private string GetDirection(Field source, Field destination)
        {
            if (source.Row > destination.Row)
            {
                if (source.Column > destination.Column)
                    return "bottomRight";
                else
                    return "bottomLeft";
            }
            else
            {
                if (source.Column > destination.Column)
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
