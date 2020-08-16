dir -recurse ./_Reports/_UnitTest*/*.html % {
  $file = $_.FullName
  Write-Host $file

  $lines = Get-Content $file
  $lines | ? { 
      ($_ -like '*class="coverableline"*') } | ? {
           -not( ($_ -like '*class="orange"*') -or ($_ -like '*class="green"*')) } | % {
               Write-Host $_
           }
}