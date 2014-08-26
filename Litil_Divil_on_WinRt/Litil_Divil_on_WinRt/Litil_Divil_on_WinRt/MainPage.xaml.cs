using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;
using Microsoft.Phone.Controls;
using PhoneDirect3DXamlAppComponent;
using Windows.Storage;
using Windows.ApplicationModel;
using System.Text;
using System.Threading.Tasks;

namespace PhoneDirect3DXamlAppInterop
{
    public partial class MainPage : PhoneApplicationPage
    {
        private static StringBuilder folderContents;
        private Direct3DBackground m_d3dBackground = null;

        // Constructor
        public MainPage()
        {
            InitializeComponent();
        }

        private async void DrawingSurfaceBackground_Loaded(object sender, RoutedEventArgs e)
        {
            await CreateFolderStructure();
            if (m_d3dBackground == null)
            {
                m_d3dBackground = new Direct3DBackground();

                // Set window bounds in dips
                m_d3dBackground.WindowBounds = new Windows.Foundation.Size(
                    (float)Application.Current.Host.Content.ActualWidth,
                    (float)Application.Current.Host.Content.ActualHeight
                    );

                // Set native resolution in pixels
                m_d3dBackground.NativeResolution = new Windows.Foundation.Size(
                    (float)Math.Floor(Application.Current.Host.Content.ActualWidth * Application.Current.Host.Content.ScaleFactor / 100.0f + 0.5f),
                    (float)Math.Floor(Application.Current.Host.Content.ActualHeight * Application.Current.Host.Content.ScaleFactor / 100.0f + 0.5f)
                    );

                // Set render resolution to the full native resolution
                m_d3dBackground.RenderResolution = m_d3dBackground.NativeResolution;

                // Hook-up native component to DrawingSurfaceBackgroundGrid
                DrawingSurfaceBackground.SetBackgroundContentProvider(m_d3dBackground.CreateContentProvider());
                DrawingSurfaceBackground.SetBackgroundManipulationHandler(m_d3dBackground);
            }
        }

        private async void Button_Click(object sender, RoutedEventArgs e)
        {
            //hello.Text = m_d3dBackground.GetCallBackMessage();
            //StorageFolder localFolder = Windows.Storage.ApplicationData.Current.LocalFolder;
            StorageFolder localFolder = Windows.Storage.KnownFolders.MusicLibrary;
            hello.Text = await EnumerateFilesAndFolders(localFolder) + "\nPath: " + m_d3dBackground.GetCallBackMessage();
        }

        private void Button_Click_1(object sender, RoutedEventArgs e)
        {
            m_d3dBackground.TestString();
        }

        private static async Task<string> CreateFolderStructure()
        {

            string[] cdFiles = {
                                    "BARMAP.MAP", "CHESS.MAP", "DD.MAP", "DEATHMAP.MAP", "DEMON.MAP", "DEMONMAP.MAP", "DIVIL.EXE", "DRAGMAP.MAP",
                                    "FROGMAP.MAP", "GOB.MAP", "GOBMAP.MAP", "LEVEL1.MAP", "LEVEL1.MOO", "LEVEL2.MAP", "LEVEL2.MOO", "LEVEL3.MAP",
                                    "LEVEL3.MOO", "LEVEL4.MAP","LEVEL4.MOO", "LEVEL5.MAP", "LEVEL5.MOO", "MED.MAP", "NEWMAP.MAP", "OLDDEATH.MAP",
                                    "RODMAP.MAP", "SIREN.MAP", "SPIDER.MAP", "SPIDMAP.MAP", "WALKMAP.MAP", "WITCHMAP.MAP"
                               };
            string[] cdGfxFiles = {
                                    "A1.BIN", "A1.CFX", "A2.BIN", "A2.CFX", "ARENA5.BIN", "ARENA5.CFX", "BAR.BIN", "BAR.CFX", "BIGCHIME.VOC",
                                    "BUB.BIN", "BUB.CFX", "CHESS.BIN", "CHESS.CFX", "DARK.BIN", "DARK.CFX", "DD.BIN", "DD.CFX", "DD1.BIN",
                                    "DD1.CFX", "DEATH.BIN", "DEATH.CFX", "DEMON.BIN", "DEMON.CFX", "DRAGON.BIN", "DRAGON.CFX", "ENDSEQ.BIN",
                                    "ENDSEQ.CFX", "ENT.BIN", "ENT.CFX", "EXIT.BIN", "EXIT.CFX", "FF.CFX", "FONT1.FON", "FROG.BIN", "FROG.CFX",
                                    "FURNACE.BIN", "FURNACE.CFX", "GARDEN.BIN", "GARDEN.CFX", "GOB.BIN", "GOB.CFX", "GOOD.BIN", "GOOD1.CFX",
                                    "GOOD2.CFX", "GOOD4.CFX", "GOOD5.CFX", "GOODIES.CFX", "HOUR.BIN", "HOUR.CFX", "INTRO.BIN", "INTRO.CFX",
                                    "INVEN.BIN", "INVEN.CFX", "LASER.BIN", "LASER.CFX", "LAVA.BIN", "LAVA.CFX", "LEVEL1V.LD1", "LEVEL1W.LDS",
                                    "LEVEL2V.LD1", "LEVEL3.BIN", "LEVEL3.CFX", "LEVEL3V.LD1", "LEVEL4V.LD1", "LEVEL5V.LD1", "LOADG.BIN",
                                    "LOADG.CFX", "MAC.BIN", "MAC.CFX", "MED.BIN", "MED.CFX", "MEMSAVE.CFX", "MUTT.CFX", "OBJECTS.CFX",
                                    "REALDARK.BIN", "REALDARK.CFX", "REQ.BIN", "REQ.CFX", "ROD.BIN", "ROD.CFX", "SAC.BIN", "SAC.CFX",
                                    "SAVE.BIN", "SAVE.CFX", "SIREN.BIN", "SIREN.CFX", "SKULL.BIN", "SKULL.CFX", "SOL.BIN", "SOL.CFX",
                                    "SOUL.BIN", "SOUL.CFX", "SPIDER.BIN", "SPIDER.CFX", "SWAMP.BIN", "SWAMP.CFX", "TOMB.BIN", "TOMB.CFX",
                                    "TORT1.CFX", "TORT2.CFX", "TORT3.CFX", "TORTURE.BIN", "WITCH.BIN", "WITCH.CFX"
                                };
            string[] gfxFiles = {
                                    "ADL.EFF", "AMBIENT.A_V", "AMBIENT.MUS", "AMBIENT.RLD", "AMBIENT.R_V", "BIGCHIME.VOC", "COMBAT.A_V",
                                    "COMBAT.MUS", "COMBAT.RLD", "COMBAT.R_V", "COMIC.A_V", "COMIC.MUS", "COMIC.RLD", "COMIC.R_V", "D.R_V",
                                    "DDSOUND.CFX", "DINTRO.RLD", "DINTRO.R_V", "DIVIL.MUS", "DRIVER.ADL", "DRIVER.ROL", "DRIVER.SC",
                                    "END1.RLD", "END1.R_V", "ENT.NDX", "FONT1.FON", "FULL.EFF", "FXENTRAN.A_V", "INTRO.A_V", "INTRO.MUS",
                                    "LEVEL1V.LD1", "LEVEL1W.LDS", "LEVEL2V.LD1", "LEVEL3V.LD1", "LEVEL4V.LD1", "LEVEL5V.LD1", "LOSTFIX.MUS",
                                    "LOSTSOUL.A_V", "LOSTSOUL.MUS", "LOSTSOUL.RLD", "LOSTSOUL.R_V", "MEMSAVE.CFX", "PREM.RLD", "PREM.R_V",
                                    "PUZZLE.A_V", "PUZZLE.MUS", "PUZZLE.RLD", "PUZZLE.R_V", "SALOON.A_V", "SALOON.MUS", "SALOON.RLD","SALOON.R_V",
                                    "SLEEPY.A_V", "SLEEPY.MUS", "SLEEPY.RLD", "SLEEPY.R_V", "SPIDER.A_V", "SPIDER.MUS", "SPIDER.RLD", "SPIDER.R_V",
                                    "TEST.RLD", "TEST.R_V", "TORT.A_V", "TORT.MUS", "TORT1.A_V", "TORT1.MUS", "TORT1.RLD", "TORT1.R_V", "TORT2.A_V",
                                    "TORT2.MUS", "TORT2.RLD", "TORT2.R_V", "TORTURE.A_V", "TORTURE.MUS", "TUNNEL.A_V", "TUNNEL.MUS", "TUNNEL.RLD",
                                    "TUNNEL.R_V", "WESTERN.A_V", "WESTERN.MUS", "WESTERN.RLD", "WESTERN.R_V"
                                };

            StorageFolder installationFolder = Package.Current.InstalledLocation;

            StorageFolder cdFolder = await installationFolder.CreateFolderAsync("CD", CreationCollisionOption.ReplaceExisting);
            StorageFolder gfxFolder = await installationFolder.CreateFolderAsync("GFX", CreationCollisionOption.ReplaceExisting);
            StorageFolder cdGfxFolder = await cdFolder.CreateFolderAsync("GFX", CreationCollisionOption.ReplaceExisting);

            await MoveFilesToFolder(cdFiles, cdFolder);
            await MoveFilesToFolder(cdGfxFiles, cdGfxFolder);
            await MoveFilesToFolder(gfxFiles, gfxFolder);

            return "true";
        }

        public static async Task<string> MoveFilesToFolder(string[] files, StorageFolder folder)
        {
            StorageFolder installationFolder = Package.Current.InstalledLocation;
            foreach(string name in files){
                StorageFile file = await installationFolder.GetFileAsync(name);
                await file.CopyAsync(folder, name, NameCollisionOption.ReplaceExisting);
            }
            return folder.Name;
        }

        public static async Task<string> EnumerateFilesAndFolders(StorageFolder rootFolder)
        {
            // Initialize StringBuilder to contain output.
            folderContents = new StringBuilder();
            folderContents.AppendLine("\\" + rootFolder.Name);

            await ListFilesInFolder(rootFolder, 1);

            return folderContents.ToString();
        }

        // Continue recursive enumeration of files and folders.
        private static async Task ListFilesInFolder(StorageFolder folder, int indentationLevel)
        {
            string indentationPadding = String.Empty.PadRight(indentationLevel * 3, ' ');

            // Get the subfolders in the current folder.
            // Increase the indentation level of the output.
            // For each subfolder, call this method again recursively.
            var foldersInFolder = await folder.GetFoldersAsync();
            int childIndentationLevel = indentationLevel + 1;
            foreach (StorageFolder currentChildFolder in foldersInFolder)
            {
                    folderContents.AppendLine(indentationPadding + "\\" + currentChildFolder.Name);
                    await ListFilesInFolder(currentChildFolder, childIndentationLevel);
                
            }

             //Get the files in the current folder.
            var filesInFolder = await folder.GetFilesAsync();
            foreach (StorageFile currentFile in filesInFolder)
            {
               // var stream = await currentFile.OpenAsync(Windows.Storage.FileAccessMode.ReadWrite);
                var x = await currentFile.GetBasicPropertiesAsync();
                folderContents.AppendLine(indentationPadding + currentFile.Name + " size: " + x.Size.ToString());
            }
        }
    }
}