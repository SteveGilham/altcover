using System;
using System.IO;
using System.Net;
using System.Net.Sockets;

namespace Echo
{
    internal class Program
    {
        private static void Main(string[] args)
        {
            IPAddress ip = IPAddress.Any;
            int port = 18000;
            TcpListener server = new TcpListener(ip, port);
            server.Start();
            TcpClient client = server.AcceptTcpClient();
            NetworkStream stream = client.GetStream();

            Echo(stream);
        }

        private static void Echo(NetworkStream stream)
        {
            using (StreamReader reader = new StreamReader(stream))
            using (StreamWriter writer = new StreamWriter(stream))
            {
                String line = reader.ReadLine();
                Console.WriteLine("Client said: {0}", line);
                writer.WriteLine(line);
            }
        }
    }
}