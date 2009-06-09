lappend auto_path ..
package require Tcl 8.5
package require midi

# Create midi file and initialise time division info
set mf [midi::file -type 1 -time_division_type ticks_per_beat -ticks_per_beat 480]

# Control track
set t1el {}
lappend t1el [list 0 [midi::event text -value "control track"]]
lappend t1el [list 0 [midi::event tempo -tempo 0X7A120]] ;# 120 Beats per minute
lappend t1el [list 0 [midi::event end_of_track]]

set t1 [midi::track -events $t1el]

# Piano track
set at2el {}
lappend at2el [list 0 [midi::event text -value "Piano track"]]
lappend at2el [list 0 [midi::event program_change -channel 0 -program "Electric Piano 2"]]
lappend at2el {*}[$mf measure -channel 0 -events {{1/1 C4} {1/1 E4} {1/1 G4} {1/1 C5} {1/1 {C4 E4 G4 C5}}}]
lappend at2el [list 0 [midi::event end_of_track]]

set t2 [midi::track -events $at2el]

# Bass track
set at3el {}
lappend at3el [list 0 [midi::event text -value "Bass track"]]
lappend at3el [list 0 [midi::event program_change -channel 1 -program "Electric Bass(pick)"]]
lappend at3el {*}[$mf measure -channel 1 -events {{1/4 C2} {1/4 D2} {1/4 E2} {1/4 F2}}]
lappend at3el {*}[$mf measure -channel 1 -events {{1/4 E2 60} {1/4 F2} {1/4 G2} {1/4 A2}}]
lappend at3el {*}[$mf measure -channel 1 -events {{1/4 G2} {1/4 A2} {1/4 B2} {1/4  C3}}]
lappend at3el {*}[$mf measure -channel 1 -events {{1/2 C3} {1/4 G2} {1/4 E2}}]
lappend at3el {*}[$mf measure -channel 1 -events {{1/1 C2}}]
lappend at3el [list 0 [midi::event end_of_track]]

set t3 [midi::track -events $at3el]

# Drum track
set at4el {}
lappend at4el [list 0 [midi::event text -value "Drum track"]]
for { set i 0 } { $i < 5 } { incr i } {
    lappend at4el {*}[$mf measure -channel 9 -events {
        {1/4 {"closed hi-hat" "acoustic bass drum"}} \
	{1/4 {"closed hi-hat"}} \
        {1/4 {"closed hi-hat" "acoustic snare"}} \
        {1/4 {"closed hi-hat"}} \
    }]
}
lappend at4el [list 0 [midi::event end_of_track]]

set t4 [midi::track -events $at4el]

# Add tracks and write file
$mf configure -tracks [list $t1 $t2 $t3 $t4]
$mf write test2.mid
$mf destroy

exit

