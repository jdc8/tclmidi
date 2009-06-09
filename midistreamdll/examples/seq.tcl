lappend auto_path .. ../..
package require midi
package require midistreamdll

# Create a midi stream header, this header will be used to send event to the
# sequencer.
set h [midistreamdll::header]

# Load the midi file specified on the command line
set mf [midi::file]
$mf read [lindex $argv 0]

# Add events to header, skip SysEx and non-tempo Meta events
set cnt 0
foreach te [$mf flatten -no_sys_ex 1 -include_meta {tempo}] {
    $h add event {*}$te
    incr cnt
}

puts "Streamed $cnt event"

# Open a midi stream device
set s [midistreamdll::open 0]

# Set the ticks per beat
if {[$mf cget -time_division_type] eq "ticks_per_beat"} {
    $s set timediv [$mf cget -ticks_per_beat]
}

# Send header to the device
$s out $h

# Sequence the events
$s play

# Wait until header is released by the sequencer
while 1 {
    if {![catch {$h destroy}]} {
	break
    }
    puts "@ [$s position]"
    after 1000
}

# Cleanup
$s close
$mf destroy

exit
