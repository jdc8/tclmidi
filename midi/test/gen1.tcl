lappend auto_path ..
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
set t2el {}
lappend t2el [list 0 [midi::event text -value "Piano track"]]
lappend t2el [list 0 [midi::event program_change -channel 0 -program "Electric Piano 2"]]

lappend t2el [list 0                [midi::event note_on  -channel 0 -note C4 -velocity 127]]
lappend t2el [list [$mf clicks 1/1] [midi::event note_off -channel 0 -note C4 -velocity 127]]

lappend t2el [list 0                [midi::event note_on  -channel 0 -note E4 -velocity 127]]
lappend t2el [list [$mf clicks 1/1] [midi::event note_off -channel 0 -note E4 -velocity 127]]

lappend t2el [list 0                [midi::event note_on  -channel 0 -note G4 -velocity 127]]
lappend t2el [list [$mf clicks 1/1] [midi::event note_off -channel 0 -note G4 -velocity 127]]

lappend t2el [list 0                [midi::event note_on  -channel 0 -note C5 -velocity 127]]
lappend t2el [list [$mf clicks 1/1] [midi::event note_off -channel 0 -note C5 -velocity 127]]

lappend t2el [list 0                [midi::event note_on  -channel 0 -note C4 -velocity 127]]
lappend t2el [list 0                [midi::event note_on  -channel 0 -note E4 -velocity 127]]
lappend t2el [list 0                [midi::event note_on  -channel 0 -note G4 -velocity 127]]
lappend t2el [list 0                [midi::event note_on  -channel 0 -note C5 -velocity 127]]
lappend t2el [list [$mf clicks 1/1] [midi::event note_off -channel 0 -note C4 -velocity 127]]
lappend t2el [list 0                [midi::event note_off -channel 0 -note E4 -velocity 127]]
lappend t2el [list 0                [midi::event note_off -channel 0 -note G4 -velocity 127]]
lappend t2el [list 0                [midi::event note_off -channel 0 -note C5 -velocity 127]]

lappend t2el [list 0                [midi::event end_of_track]]
set t2 [midi::track -events $t2el]

# Bass track
set t3el {}
lappend t3el [list 0 [midi::event text -value "Bass track"]]
lappend t3el [list 0 [midi::event program_change -channel 1 -program "Electric Bass(pick)"]]

lappend t3el [list 0                [midi::event note_on  -channel 1 -note C2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note C2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note D2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note D2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note E2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note E2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note F2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note F2 -velocity 127]]

lappend t3el [list 0                [midi::event note_on  -channel 1 -note E2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note E2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note F2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note F2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note G2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note G2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note A2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note A2 -velocity 127]]

lappend t3el [list 0                [midi::event note_on  -channel 1 -note G2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note G2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note A2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note A2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note B2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note B2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note C3 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note C3 -velocity 127]]

lappend t3el [list 0                [midi::event note_on  -channel 1 -note C3 -velocity 127]]
lappend t3el [list [$mf clicks 1/2] [midi::event note_off -channel 1 -note C3 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note G2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note G2 -velocity 127]]
lappend t3el [list 0                [midi::event note_on  -channel 1 -note E2 -velocity 127]]
lappend t3el [list [$mf clicks 1/4] [midi::event note_off -channel 1 -note E2 -velocity 127]]

lappend t3el [list 0        [midi::event note_on  -channel 1 -note C2 -velocity 127]]
lappend t3el [list [$mf clicks 1/1]   [midi::event note_off -channel 1 -note C2 -velocity 127]]

lappend t3el [list 0 [midi::event end_of_track]]
set t3 [midi::track -events $t3el]

# Drum track
set t4el {}
lappend t4el [list 0 [midi::event text -value "Drum track"]]

lappend t4el [list 0                [midi::event note_on -channel 9 -note "closed hi-hat"      -velocity 127]]
lappend t4el [list 0                [midi::event note_on -channel 9 -note "acoustic bass drum" -velocity 127]]
lappend t4el [list [$mf clicks 1/4] [midi::event note_on -channel 9 -note "closed hi-hat"      -velocity 127]]
lappend t4el [list [$mf clicks 1/4] [midi::event note_on -channel 9 -note "closed hi-hat"      -velocity 127]]
lappend t4el [list 0                [midi::event note_on -channel 9 -note "acoustic snare"     -velocity 127]]
lappend t4el [list [$mf clicks 1/4] [midi::event note_on -channel 9 -note "closed hi-hat"      -velocity 127]]

for { set i 0 } { $i < 4 } { incr i } { 
    lappend t4el [list [$mf clicks 1/4] [midi::event note_on -channel 9 -note "closed hi-hat"      -velocity 127]]
    lappend t4el [list 0                [midi::event note_on -channel 9 -note "acoustic bass drum" -velocity 127]]
    lappend t4el [list [$mf clicks 1/4] [midi::event note_on -channel 9 -note "closed hi-hat"      -velocity 127]]
    lappend t4el [list [$mf clicks 1/4] [midi::event note_on -channel 9 -note "closed hi-hat"      -velocity 127]]
    lappend t4el [list 0                [midi::event note_on -channel 9 -note "acoustic snare"     -velocity 127]]
    lappend t4el [list [$mf clicks 1/4] [midi::event note_on -channel 9 -note "closed hi-hat"      -velocity 127]]
}

lappend t4el [list 0 [midi::event end_of_track]]
set t4 [midi::track -events $t4el]

# Add tracks and write file
$mf configure -tracks [list $t1 $t2 $t3 $t4]
$mf write test.mid
$mf destroy

exit

