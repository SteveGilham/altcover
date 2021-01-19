# The AltCover API

## By package/assembly

An index of which nuget packages contain which assemblies, and which have APIs

| | API? | AltCover.nupkg | AltCover.Api.nupkg | AltCover.Global.nupkg | AltCover.Visualizer.nupkg | AltCover.Fake.nupkg |
| --- | :---: | :---: |  :---: |  :---: | :---: | :---: | 
| [AltCover](./AltCover) |  | ✔️ | ✔️ | ✓ .net core |   |   |
| [AltCover.Cake](./AltCover.Cake/AltCover.Cake-apidoc) | ✔️ |   | ✔️ |   |   |   |
| AltCover.DataCollector |  | ✓ .net core | ✓ .net core | ✓ .net core |   |   |
| [AltCover.DotNet](./AltCover.DotNet) | ✔️ |  | ✔️ |   |   |   |
| [AltCover.Engine](./AltCover.Engine) | ✔️ | ✔️ | ✔️ | ✓ .net core |   |   |
| [AltCover.Fake](./AltCover.Fake/Fake-fsapidoc) | ✔️ |   | ✔️ |   |   |   |
| [AltCover.Fake.DotNet.Testing](./AltCover.Fake.DotNet.Testing.AltCover) | ✔️ |   |   |   |   | ✔️ |
| [AltCover.Monitor](./AltCover.Monitor) | ✔️ | ✔️ | ✔️ |  |   |   |
| AltCover.NetCoreApp |   | ✔️ | ✔️ | ✓ .net core |   |   |
| [AltCover.PowerShell](./AltCover.PowerShell/AltCover.PowerShell-apidoc) | ✔️ | ✔️ | ✔️ | ✓ .net core |   |   |
| [AltCover.Toolkit](./AltCover.Toolkit) | ✔️ | ✔️ | ✔️ | ✓ .net core |   |   |
| AltCover.Visualizer |  | ✔️ | | | ✓ .net core  |   |

Assembly names link to the related API documentation.

## By topic

* [`dotnet test` integration](%60dotnet-test%60-integration)
* [General usage/how-to](Usage)
* [MSBuild tasks](AltCover.Engine/Tasks-fsapidoc)
* [PowerShell tools for coverage reports](PowerShell-integration)
* [The core in-process API](AltCover.Engine/)
* [The utilities APIs](AltCover.Toolkit) -- behind the PowerShell
* [Fake and Cake integration](Fake-and-Cake-integration)
* [The `AltCover.Fake` package](AltCover.Fake.DotNet.Testing.AltCover/)
* [Test coverage monitor](AltCover.Monitor/)

