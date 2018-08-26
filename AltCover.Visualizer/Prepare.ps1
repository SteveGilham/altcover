$lines = Get-Content "Visualizer.glade"
$lines | % { $_.Replace("glade-interface", "interface").Replace("widget","object") } | Set-Content "Visualizer3.glade"
