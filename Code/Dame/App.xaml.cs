using SbsSW.SwiPlCs;
using System;
using System.Windows;

namespace Dame
{
    /// <summary>
    /// Interaktionslogik für "App.xaml"
    /// </summary>
    public partial class App : Application
    {

        protected override void OnStartup(StartupEventArgs e)
        {
            try
            {
#if X64
                PlEngine.Initialize(new string[] { "-q", "-L5G", "-T5G", "-G5G" });
#else
                PlEngine.Initialize(new string[] { "-q", "-L128M", "-T128M", "-G128M" });
#endif
            }
            catch (System.IO.FileNotFoundException)
            {
                MessageBox.Show("Der Pfad zum Prologinterpreter muss der PATH-Variable hinzugefügt werden!", "FEHLER", MessageBoxButton.OK,MessageBoxImage.Error);
                Shutdown();
                return;
            }
            catch(BadImageFormatException)
            {
                MessageBox.Show("Die Prolog-Architektur passt nicht zur Programmarchitektur!", "FEHLER", MessageBoxButton.OK, MessageBoxImage.Error);
                Shutdown();
                return;
            }
            PlQuery.PlCall("chdir('" + System.IO.Path.Combine(Environment.CurrentDirectory, "Prolog").Replace('\\','/') + "')");
            try
            {
                PlQuery.PlCall("consult('main')");
            }
            catch (SbsSW.SwiPlCs.Exceptions.PlException)
            {
                MessageBox.Show("Konnte Prolog-Hauptdatei nicht konsultieren.", "FEHLER", MessageBoxButton.OK, MessageBoxImage.Error);
                Shutdown();
                return;
            }
            base.OnStartup(e);
        }

        private void Application_Exit(object sender, ExitEventArgs e)
        {
            PlEngine.PlCleanup();
        }
    }
}
