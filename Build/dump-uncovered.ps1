$files = Get-ChildItem -recurse ./_Reports/_UnitTest*/*.html 

$files | % {
  $file = $_.FullName
  $lines = Get-Content $file
  $mark = $false
  $lines | ? { 
      ($_ -like '*class="coverableline"*') } | ? {
           -not( ($_ -like '*class="orange"*') -or ($_ -like '*class="green"*')) } | % {
               $mark = $true
               Write-Host $_
           }
  if ($mark) { 
      Write-Host $file
      Write-Host " " 
    }
}