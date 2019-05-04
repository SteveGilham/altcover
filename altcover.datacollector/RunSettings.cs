using System.IO;
using System.Linq;
using System.Reflection;
using System.Resources;
using System.Xml;
using System.Xml.Linq;

using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace AltCover.DataCollector
{
    public class RunSettings : Task
    {
        public string TestSetting { get; set; }

        [Output]
        public string Extended { get; set; }

        public override bool Execute()
        {
            var settings = new XDocument();
            if (!string.IsNullOrWhiteSpace(TestSetting) &&
                File.Exists(TestSetting))
            {
                try
                {
                    using (var s = File.OpenRead(TestSetting))
                        settings = XDocument.Load(s);
                }
                catch (IOException)
                { settings = new XDocument(); }
                catch (XmlException)
                { settings = new XDocument(); }
            }
            else
            {
                TestSetting = Path.GetTempFileName();
            }

            var rs = settings.Descendants("RunSettings").FirstOrDefault();
            if (rs == null)
            {
                rs = new XElement("RunSettings");
                settings.Add(rs);
            }

            var ip1 = rs.Descendants("InProcDataCollectionRunSettings").FirstOrDefault();
            if (ip1 == null)
            {
                ip1 = new XElement("InProcDataCollectionRunSettings");
                rs.Add(ip1);
            }

            var ip2 = ip1.Descendants("InProcDataCollectors").FirstOrDefault();
            if (ip2 == null)
            {
                ip2 = new XElement("InProcDataCollectors");
                ip1.Add(ip2);
            }

            var here = Assembly.GetExecutingAssembly();
            var altcover = new XElement("InProcDataCollector",
                           new XAttribute("friendlyName", "AltCover"),
                           new XAttribute("uri", "InProcDataCollector://AltCover/Recorder/" + here.GetName().Version.ToString()),
                           new XAttribute("assemblyQualifiedName", "AltCover.DataCollector.Recorder, " + here.FullName),
                           new XAttribute("codebase", here.Location),
                           new XElement("Configuration",
                               new XElement("Offload", new XText("true"))));
            ip2.Add(altcover);

            Extended = Path.ChangeExtension(TestSetting, ".altcover.runsettings");
            settings.Save(Extended);

            return true;
        }

        public RunSettings()
        {
        }

        public RunSettings(ResourceManager taskResources) : base(taskResources)
        {
        }

        public RunSettings(ResourceManager taskResources, string helpKeywordPrefix) : base(taskResources, helpKeywordPrefix)
        {
        }
    }
}