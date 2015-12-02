using SbsSW.SwiPlCs;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Navigation;

namespace Dame
{
    /// <summary>
    /// Interaktionslogik für "App.xaml"
    /// </summary>
    public partial class App : Application
    {
        protected override void OnStartup(StartupEventArgs e)
        {
            Environment.SetEnvironmentVariable("SWI_HOME_DIR", @"C:\Program Files (x86)\swipl\");
            PlEngine.Initialize(new string[] { "-q", "-f", "main.pl" });
            if (!PlQuery.PlCall("init."))
            {
                MessageBox.Show("Konnte Prologdateien nicht konsultieren.", "FEHLER", MessageBoxButton.OK, MessageBoxImage.Error);
                Shutdown(-1);
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
