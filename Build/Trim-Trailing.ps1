$types = ("*.fs", "*.fsi", "*.fsx" )

$types | % {
  dir $_ -Recurse | % {
    if (-not $_.PSIsContainer) {
      Write-Host $_
      $path = $_
      $lines = Get-Content $path
      if ($lines) 
      { $trimmed = [String]::Join("`r`n", $lines)
        [System.IO.File]::WriteAllText($path, $trimmed, [System.Text.Encoding]::UTF8) }
    }
  }
}