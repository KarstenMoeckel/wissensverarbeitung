using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SbsSW.SwiPlCs;
using System.Threading;

namespace Dame
{
    class Engine: IDisposable
    {
        public event EventHandler<StoneChangedEventArgs> StonesChanged;
        public event EventHandler<HistoryEventArgs> HistoryChanged;

        private Thread historyThread;
        private Thread stoneThread;

        public Engine()
        {
            historyThread = new Thread(new ThreadStart(CheckHistory_Thread));
            stoneThread = new Thread(new ThreadStart(CheckStones_Thread));
        }

        public void Init()
        {
            historyThread.Start();
            stoneThread.Start();
        }

        public void Start()
        {
            Options.SetOptions();
        }

        private void CheckHistory_Thread()
        {
            PlEngine.PlThreadAttachEngine();
            while (Thread.CurrentThread.ThreadState == ThreadState.Running)
            {
                if (PlQuery.PlCall("historyUpdated"))
                {
                    PlQuery query = new PlQuery("history(X)");
                    PlTerm result = query.Solutions.First()[0];
                    HistoryEventArgs args = new HistoryEventArgs(result.ToListString());
                    HistoryChanged.Invoke(this, args);
                }
                Thread.Sleep(250);
            }
        }

        private void CheckStones_Thread()
        {
            PlEngine.PlThreadAttachEngine();
            while (Thread.CurrentThread.ThreadState == ThreadState.Running)
            {
                if (PlQuery.PlCall("stonesUpdated"))
                {
                    PlQuery query = new PlQuery("stone(Row,Col,Color,Type)");
                    IEnumerable<PlTermV> result = query.Solutions;
                    StoneChangedEventArgs args = new StoneChangedEventArgs(result.Select<PlTermV, Stone>((v) => new Stone(v)));
                    StonesChanged.Invoke(this, args);
                }
                Thread.Sleep(250);
            }
        }

        public void MoveStone(int sourceRow, int sourceCol, int destRow, int destCol)
        {
            PlQuery.PlCall("move", new PlTermV(new PlTerm[] { new PlTerm(sourceRow), new PlTerm(sourceCol), new PlTerm(destRow), new PlTerm(destCol) }));
        }

        #region IDisposable Support
        private bool disposedValue = false; // Dient zur Erkennung redundanter Aufrufe.

        protected virtual void Dispose(bool disposing)
        {
            if (!disposedValue)
            {
                if (disposing)
                {
                    if (!historyThread.Join(300))
                        historyThread.Abort();
                    if (!stoneThread.Join(300))
                        stoneThread.Abort();
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
