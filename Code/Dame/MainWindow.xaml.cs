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
        private Button currentMoveStone;
        private IList<Button> possibleHits;

        public Engine Engine { get; private set; }

        public IEnumerable<string> History { get; private set; }

        public MainWindow()
        {
            InitializeComponent();
            Engine = new Engine();
            Engine.PropertyChanged += Engine_PropertyChanged;
            Engine.GameOver += Engine_GameOver;
            possibleHits = new List<Button>();
        }

        private void Engine_GameOver(object sender, EventArgs e)
        {
            Dispatcher.Invoke(new Action((() => btn_Start.IsEnabled = true)));
        }

        private void Engine_PropertyChanged(object sender, System.ComponentModel.PropertyChangedEventArgs e)
        {
            switch (e.PropertyName)
            {
                case nameof(Engine.Stones):
                    Dispatcher.Invoke(new Action(updateStones));
                    break;
                case nameof(Engine.StoneToMove):
                    updateMoveStone();
                    break;
                case nameof(Engine.PossibleHits):
                    Dispatcher.Invoke(new Action(updateHits));
                    break;
            }
        }

        private void updateHits()
        {
            foreach (Button b in possibleHits)
                b.Tag = null;
            possibleHits.Clear();
            IEnumerable<Button> list = gameField.Children.OfType<Button>();
            foreach(Field f in Engine.PossibleHits)
            {
                Button b = GetButtonByCell(list, f);
                b.Tag = "hit";
                possibleHits.Add(b);
            }
        }

        private void updateMoveStone()
        {
            if (currentMoveStone != null)
                currentMoveStone.Tag = null;
            if (Engine.StoneToMove == default(Field))
            {
                currentMoveStone = null;
            }
            else
            {
                currentMoveStone = GetButtonByCell(gameField, Engine.StoneToMove);
                currentMoveStone.Tag = "move";
            }
        }

        private Button GetButtonByCell(Grid grid,Field field)
        {
            IEnumerable<Button> list = grid.Children.OfType<Button>();
            return GetButtonByCell(list, field);
        }

        private Button GetButtonByCell(IEnumerable<Button> list, Field field)
        {
            return list.First((b) => Grid.GetColumn(b) == field.Column && Grid.GetRow(b) == field.Row);
        }

        private void updateStones()
        {
            foreach (Stone stone in Engine.Stones)
            {
                BitmapImage image = new BitmapImage(GetStoneImageUri(stone.Color, stone.Type));
                Image img = new Image();
                img.Source = image;
                img.Stretch = Stretch.Fill;
                Field f = stone.Field;
                Button btn = GetButtonByCell(gameField, f);
                btn.Content = img;
            }
        }

        private void GameFieldButton_Click(object sender, RoutedEventArgs e)
        {
            Button b = (Button)sender;
            int row = Grid.GetRow(b);
            int col = Grid.GetColumn(b);
            Field field = new Field(row, col);
            if (Engine.StoneToMove == default(Field))
            {
                Engine.StoneToMove = field;
            }
            else
            {
                if(Engine.StoneToMove == field)
                    Engine.StoneToMove = default(Field);
                else
                    Engine.MoveDestination = field;
            }
        }
        
        private void btn_Start_Click(object sender, RoutedEventArgs e)
        {
            if (Engine.Start())
            {
                Button b = (Button)sender;
                b.IsEnabled = false;
            }
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
            Engine.Init();
        }

        private void Window_Closed(object sender, EventArgs e)
        {
            Engine.Stop();
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
                if (!Engine.LoadFile(dialog.FileName))
                    MessageBox.Show("Die StartPositionen konnen nicht geladen werden.", "Fehler", MessageBoxButton.OK, MessageBoxImage.Warning);
            }
        }

        private void Options_Click(object sender, RoutedEventArgs e)
        {
            Options opt = new Options(Engine);
            opt.ShowDialog();
        }
    }
}
