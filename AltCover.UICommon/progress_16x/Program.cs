// See https://aka.ms/new-console-template for more information

using System;
using System.IO;
using System.Windows;
using System.Windows.Media.Imaging;

public class Program
{
  private static int Main(string[] args)
  {
    var imageStreamSource = new FileStream(@"C:\Users\steve\Documents\GitHub\altcover\AltCover.UICommon\process-working-32x.png", FileMode.Open, FileAccess.Read, FileShare.Read);
    var decoder = new PngBitmapDecoder(imageStreamSource, BitmapCreateOptions.PreservePixelFormat, BitmapCacheOption.Default);
    var bitmapSource = decoder.Frames[0];

    for (int x = 0; x < 8; ++x)
      for (int y = 0; y < 4; ++y)
      {
        var size = 32;
        var step = x + (8 * y);
        var where = new Int32Rect(x * size, y * size, size, size);
        var image = new CroppedBitmap(bitmapSource, where);
        var stream = new FileStream(@"C:\Users\steve\Documents\GitHub\altcover\AltCover.UICommon\p_" + step + "_" + size + "x.png", FileMode.Create);
        var encoder = new PngBitmapEncoder
        {
          Interlace = PngInterlaceOption.On
        };
        encoder.Frames.Add(BitmapFrame.Create(image));
        encoder.Save(stream);
      }

    Console.WriteLine("Hello, World!");
    return 0;
  }
}