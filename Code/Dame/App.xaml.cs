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
            //Environment.SetEnvironmentVariable("SWI_HOME_DIR", @"E:\Program Files\swipl");
            PlEngine.Initialize(new string[] { "-q" });
            PlQuery.PlCall("chdir('" + System.IO.Path.Combine(Environment.CurrentDirectory, "Prolog").Replace('\\','/') + "')");
            if (!PlQuery.PlCall("consult('main')"))
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
