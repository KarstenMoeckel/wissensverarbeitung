using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
            return PlQuery.PlCall("loadStartPos", new PlTermV(new PlTerm("'" + file.Replace('\\','/') + "'")));
        }

        private void CheckHistory_Thread()
        {
            PlEngine.PlThreadAttachEngine();
            while (running)
            {
                if (PlQuery.PlCall("historyUpdated"))
                {
                    PlQuery query = new PlQuery("history(X)");
                    PlTerm result = query.Solutions.First()[0];
                    PlQuery.PlCall("retractall(historyUpdated)");
                    HistoryEventArgs args = new HistoryEventArgs(result.ToListString());
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
                if (PlQuery.PlCall("stonesUpdated"))
                {
                    PlQuery query = new PlQuery("stone(Field,Color,Type)");
                    IEnumerable<PlTermV> result = query.Solutions;
                    PlQuery.PlCall("retractall(stonesUpdated)");
                    StoneChangedEventArgs args = new StoneChangedEventArgs(result.Select<PlTermV, Stone>((v) => new Stone(v)));
                    StonesChanged.Invoke(this, args);
                }
                Thread.Sleep(250);
            }
        }

        public void MoveStone(Field source, Field destination)
        {
            PlQuery.PlCall("moveStone", new PlTermV(source.ToTerm(), destination.ToTerm()));
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
