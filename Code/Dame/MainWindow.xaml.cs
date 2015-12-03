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
        private int moveSourceRow;
        private int moveSourceCol;

        public MainWindow()
        {
            InitializeComponent();
            engine = new Engine();
            engine.HistoryChanged += Engine_HistoryChanged;
            engine.StonesChanged += Engine_StonesChanged;
            moveSourceCol = 0;
            moveSourceRow = 0;
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
                    Button btn = gameField.Children.OfType<Button>().FirstOrDefault((b) => Grid.GetColumn(b) == stone.Column && Grid.GetRow(b) == stone.Row);
                    if (btn == null)
                        throw new Exception(string.Format("Could not find button in Cell {0}/{1}", stone.Row, stone.Column));
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
            if (moveSourceRow == 0)
            {
                moveSourceCol = col;
                moveSourceRow = row;
            }
            else
            {
                engine.MoveStone(moveSourceRow, moveSourceCol, row, col);
            }
        }
        
        private void btn_Start_Click(object sender, RoutedEventArgs e)
        {
            engine.Start();
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
    }
}
