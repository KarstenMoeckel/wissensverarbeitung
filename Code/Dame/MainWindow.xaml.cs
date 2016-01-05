using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Media.Imaging;

namespace Dame
{
    /// <summary>
    /// Interaktionslogik für MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private Engine engine;
        private Field _MoveSourceField;
        private Field MoveSourceField
        {
            get { return _MoveSourceField; }
            set
            {
                IEnumerable<Button> list = gameField.Children.OfType<Button>();
                if (_MoveSourceField != null)
                {

                    Button btn = GetButtonByCell(list, _MoveSourceField);
                    btn.Tag = null;
                }
                _MoveSourceField = value;
                if (value != null)
                {
                    Button btn = GetButtonByCell(list, _MoveSourceField);
                    btn.Tag = "move";
                }
            }
        }

        public IEnumerable<string> History { get; private set; }

        public MainWindow()
        {
            InitializeComponent();
            engine = new Engine();
            engine.HistoryChanged += Engine_HistoryChanged;
            engine.StonesChanged += Engine_StonesChanged;
        }

        private Button GetButtonByCell(Grid grid,Field field)
        {
            IEnumerable<Button> list = grid.Children.OfType<Button>();
            return GetButtonByCell(list, field);
        }

        private Button GetButtonByCell(IEnumerable<Button> list, Field field)
        {
            return list.FirstOrDefault((b) => Grid.GetColumn(b) == field.Column && Grid.GetRow(b) == field.Row);
        }

        private void Engine_StonesChanged(object sender, StoneChangedEventArgs e)
        {
            Dispatcher.Invoke(new Action(() =>
            {
                foreach (Button button in gameField.Children.OfType<Button>())
                {
                    button.Content = null;
                    button.Tag = null;
                }
                foreach (Stone stone in e.Stones)
                {
                    BitmapImage image = new BitmapImage(GetStoneImageUri(stone.Color, stone.Type));
                    Image img = new Image();
                    img.Source = image;
                    img.Stretch = Stretch.Fill;
                    Field f = stone.Field;
                    Button btn = GetButtonByCell(gameField, f);
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
                lb_History.ItemsSource = e.History;
            }));
        }

        private void GameFieldButton_Click(object sender, RoutedEventArgs e)
        {
            Button b = (Button)sender;
            int row = Grid.GetRow(b);
            int col = Grid.GetColumn(b);
            if (MoveSourceField == null)
            {
                if (b.Content != null)
                {
                    MoveSourceField = new Field(row, col);
                    b.Tag = "move";
                }
            }
            else
            {
                Field destination = engine.MoveStone(MoveSourceField, new Field(row, col));
                if (destination != null)
                {
                    IEnumerable<Field> hits = engine.MoreHitsPossible(destination);
                    if (hits != null)
                    {
                        IEnumerable<Button> buttonList = gameField.Children.OfType<Button>();
                        foreach(Field field in hits)
                        {
                            Button button = GetButtonByCell(buttonList, field);
                            button.Tag = "hit";
                        }
                        MoveSourceField = destination;
                        return;
                    }
                    engine.StartNextTurn();
                    MoveSourceField = null;
                    return;
                }
                MoveSourceField = null;
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
            if (color == StoneColor.Black && type == StoneType.King)
                return new Uri("pack://application:,,,/Images/black_queen.png");
            if (color == StoneColor.White && type == StoneType.Normal)
                return new Uri("pack://application:,,,/Images/white_normal.png");
            if (color == StoneColor.White && type == StoneType.King)
                return new Uri("pack://application:,,,/Images/white_queen.png");
            throw new ArgumentException("Invalid combination of parameters");
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            engine.Init();
        }

        private void Window_Closed(object sender, EventArgs e)
        {
            engine.Stop();
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
