using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace Dame
{
    /// <summary>
    /// Interaktionslogik für MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private Engine engine;

        public MainWindow()
        {
            InitializeComponent();
            engine = new Engine();
        }

        private void GameFieldButton_Click(object sender, RoutedEventArgs e)
        {
            Button b = (Button)sender;
            int row = Grid.GetRow(b);
            int col = Grid.GetColumn(b);
        }

        private void btn_Start_Click(object sender, RoutedEventArgs e)
        {
            engine.Start();
            UpdateStoneList();
        }

        private void UpdateStoneList()
        {
            IEnumerable<Stone> list = engine.GetStoneList();
            foreach (Button button in gameField.Children.OfType<Button>())
                button.Content = null;
            foreach (Stone stone in list)
            {
                BitmapImage image = new BitmapImage(GetStoneImageUri(stone.Color, stone.Type));
                Image img = new Image();
                img.Source = image;
                img.Stretch = Stretch.Fill;
                Button b = gameField.Children.OfType<Button>().FirstOrDefault((e) => Grid.GetColumn(e) == stone.Column && Grid.GetRow(e) == stone.Row);
                if (b == null)
                    throw new Exception(string.Format("Could not find button in Cell {0}/{1}", stone.Row, stone.Column));
                b.Content = img;
            }
        }
        
        private Uri GetStoneImageUri(StoneColor color, StoneType type)
        {
            if (color == StoneColor.black && type == StoneType.normal)
                return new Uri("pack://application:,,,/Images/black_normal.png");
            if (color == StoneColor.black && type == StoneType.queen)
                return new Uri("pack://application:,,,/Images/black_queen.png");
            if (color == StoneColor.white && type == StoneType.normal)
                return new Uri("pack://application:,,,/Images/white_normal.png");
            if (color == StoneColor.white && type == StoneType.queen)
                return new Uri("pack://application:,,,/Images/white_queen.png");
            throw new ArgumentException("Invalid combination of parameters");
        }
    }
}
