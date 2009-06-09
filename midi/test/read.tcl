lappend auto_path ..
package require midi

set mf [midi::file]
$mf read [lindex $argv 0]
puts [$mf dump]
foreach te [$mf flatten] {
    puts $te
}
$mf destroy

exit
