using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace Dame
{
    /// <summary>
    /// Interaktionslogik für Options.xaml
    /// </summary>
    public partial class Options : Window
    {
        public Difficulty Difficulty { get; set; }
        public StoneColor Player { get; set; }
        public StoneColor StartColor { get; set; }
        private Engine engine;
        internal Options(Engine engine)
        {
            this.engine = engine;
            Difficulty = engine.Options.Difficulty;
            Player = engine.Options.Player;
            StartColor = engine.Options.StartColor;
            InitializeComponent();
        }

        private void button_Click(object sender, RoutedEventArgs e)
        {
            engine.Options.Difficulty = Difficulty;
            engine.Options.Player = Player;
            engine.Options.StartColor = StartColor;
            Close();
        }

        private void button_Click_1(object sender, RoutedEventArgs e)
        {
            Close();
        }
    }
}
