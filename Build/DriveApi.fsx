#i "nuget: https://api.nuget.org/v3/index.json"
#i """nuget: {1}"""
#i """nuget: {2}"""

#r "nuget: AltCover.Api, {0}"
#r "nuget: AltCover.Fake, {0}"
#r "nuget: Fake.Core.Target, {3}"
#r "nuget: Fake.DotNet.Cli, {4}"
#r "nuget: Unquote, {5}"
#r "nuget: System.Collections.Immutable, {6}"
#load "DriveApi.fs"

ApiUse.DriveApi.Execute [||]