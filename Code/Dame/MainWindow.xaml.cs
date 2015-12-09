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
        private Field moveSourceField;

        public MainWindow()
        {
            InitializeComponent();
            engine = new Engine();
            engine.HistoryChanged += Engine_HistoryChanged;
            engine.StonesChanged += Engine_StonesChanged;
        }

        private void Engine_StonesChanged(object sender, StoneChangedEventArgs e)
        {
            Dispatcher.Invoke(new Action(() =>
            {
                foreach (Button button in gameField.Children.OfType<Button>())
                    button.Content = null;
                foreach (Stone stone in e.Stones)
                {
                    BitmapImage image = new BitmapImage(GetStoneImageUri(stone.Color, stone.Type));
                    Image img = new Image();
                    img.Source = image;
                    img.Stretch = Stretch.Fill;
                    Field f = stone.Field;
                    Button btn = gameField.Children.OfType<Button>().FirstOrDefault((b) => Grid.GetColumn(b) == f.Column && Grid.GetRow(b) == f.Row);
                    if (btn == null)
                        throw new Exception(string.Format("Could not find button in Cell {0}/{1}", f.Row, f.Column));
                    btn.Content = img;
                }
            }));
        }

        private void Engine_HistoryChanged(object sender, HistoryEventArgs e)
        {
            Dispatcher.Invoke(new Action(() =>
            {
                lb_History.Items.Clear();
                foreach (string str in e.History)
                    lb_History.Items.Add(str);
            }));
        }

        private void GameFieldButton_Click(object sender, RoutedEventArgs e)
        {
            Button b = (Button)sender;
            int row = Grid.GetRow(b);
            int col = Grid.GetColumn(b);
            if (moveSourceField == null)
            {
                moveSourceField = new Field(row, col);
            }
            else
            {
                engine.MoveStone(moveSourceField, new Field(row, col));
                moveSourceField = null;
            }
        }
        
        private void btn_Start_Click(object sender, RoutedEventArgs e)
        {
            engine.Start();
        }
        
        private Uri GetStoneImageUri(StoneColor color, StoneType type)
        {
            if (color == StoneColor.Black && type == StoneType.Normal)
                return new Uri("pack://application:,,,/Images/black_normal.png");
            if (color == StoneColor.Black && type == StoneType.Queen)
                return new Uri("pack://application:,,,/Images/black_queen.png");
            if (color == StoneColor.White && type == StoneType.Normal)
                return new Uri("pack://application:,,,/Images/white_normal.png");
            if (color == StoneColor.White && type == StoneType.Queen)
                return new Uri("pack://application:,,,/Images/white_queen.png");
            throw new ArgumentException("Invalid combination of parameters");
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            engine.Init();
        }

        private void Window_Closed(object sender, EventArgs e)
        {
            engine.Dispose();
        }

        private void loadStartPos_Click(object sender, RoutedEventArgs e)
        {
            Microsoft.Win32.OpenFileDialog dialog = new Microsoft.Win32.OpenFileDialog();
            dialog.CheckFileExists = true;
            dialog.InitialDirectory = Environment.CurrentDirectory;
            dialog.Multiselect = false;
            bool? result = dialog.ShowDialog();
            if (result.Value)
            {
                if (!engine.LoadFile(dialog.FileName))
                    MessageBox.Show("Die StartPositionen konnen nicht geladen werden.", "Fehler", MessageBoxButton.OK, MessageBoxImage.Warning);
            }
        }

        private void Options_Click(object sender, RoutedEventArgs e)
        {
            Options opt = new Options(engine);
            opt.ShowDialog();
        }
    }
}
