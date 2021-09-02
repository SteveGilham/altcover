$files = Get-ChildItem -recurse ./_Reports/_UnitTest*/*.html 

$files | % {
  $file = $_.FullName
  $lines = Get-Content $file
  $mark = $false
  $count = 0
  $limit = 10
  $lines | ? { 
      ($count -le $limit) -and ($_ -like '*class="coverableline"*') } | ? {
           -not( ($_ -like '*class="orange"*') -or ($_ -like '*class="green"*')) } | % {
               $mark = $true
               $count = $count + 1
               Write-Host $_
           }
  if ($mark) { 
      if ($count -gt $limit) { Write-Host "`t..."}
      Write-Host $file
      Write-Host " " 
    }
}