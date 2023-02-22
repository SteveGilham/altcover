#i "nuget: https://api.nuget.org/v3/index.json"
#i """nuget: {1}"""
#i """nuget: {2}"""

#r "nuget: AltCover.Api, {0}"
#r "nuget: AltCover.Fake, {0}"
#r "nuget: Fake.Core.Target, 6.0.0"
#r "nuget: Fake.DotNet.Cli, 6.0.0"
#r "nuget: Unquote, 6.1.0"
#load "DriveApi.fs"

ApiUse.DriveApi.Execute [||]