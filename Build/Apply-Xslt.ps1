<# 
.SYNOPSIS 
    This script applies an XSL transform. 
    
.DESCRIPTION 
    This script applies an XSL transform to a document, both specified by file path, 
    and writes the result as a tring to the object pipeline. 
        
.NOTES 
    File Name  : Apply-Xslt.ps1 
    Requires   : PowerShell Version 2.0
    
.PARAMETER XsltPath

The path to the transform

.PARAMETER DocumentPath

The path to the document being transformed

#> 
param ( 
    [Parameter(Mandatory = $true)] [string] $XsltPath,
    [Parameter(Mandatory = $true)] [string] $DocumentPath
    )
    
$here = Split-Path $MyInvocation.MyCommand.Definition

Add-Type -AssemblyName "System.Xml.Linq"

$XsltPath = Join-Path $here $XsltPath | Resolve-Path
Write-Host $XsltPath

$stylesheet = [System.Xml.XmlReader]::Create($XsltPath)
$xmlTransform = new-object System.Xml.Xsl.XslCompiledTransform
$settings = new-object System.Xml.Xsl.XsltSettings @($false, $true)
$xmlTransform.Load($stylesheet, $Settings, $null)
$styleSheet.Close()

$DocumentPath = Join-Path $here $DocumentPath | Resolve-Path
Write-Host $DocumentPath

$rawStream = new-object system.io.StreamReader -ArgumentList @($DocumentPath)
$rawDocument = [System.Xml.Linq.XDocument]::Load($rawStream)
$rawStream.Close()

$sw = new-object system.io.StringWriter @()

## transform the document:
$xmlTransform.Transform($rawDocument.CreateReader(), $null, $sw)
$sw.ToString()