## (This license blatantly stolen from Tktable and Tcl/Tk license and adapted -
## thus assume it falls under similar license terms).
##
## This software is copyrighted by Jos Decoster <jos _dot_ decoster _at_ gmail 
## _dot_ com>.  The  following terms apply to all files associated with the 
## software unless explicitly disclaimed in individual files.
##
## The authors hereby grant permission to use, copy, modify, distribute, and
## license this software and its documentation for any purpose, provided that
## existing copyright notices are retained in all copies and that this notice
## is included verbatim in any distributions.  No written agreement, license,
## or royalty fee is required for any of the authorized uses.
##
## IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY FOR
## DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
## OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY DERIVATIVES THEREOF,
## EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
## INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE IS
## PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE NO
## OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
## MODIFICATIONS.
##
## RESTRICTED RIGHTS: Use, duplication or disclosure by the U.S. government
## is subject to the restrictions as set forth in subparagraph (c) (1) (ii)
## of the Rights in Technical Data and Computer Software Clause as DFARS
## 252.227-7013 and FAR 52.227-19.

## Many thanks to Mo DeJong for testing, feedback and midi-controller code.

# TODO: tests for file and track

package provide midi 0.11

namespace eval ::midi {
    variable knownDuration
    array set knownDuration {
        0               0
        1/1             1.0
        1/1.            1.5
        1/2             0.5
        1/2.            0.75
        1/2t            (1.0/1/3)
        1/4             0.25
        1/4.            0.375
        1/4t            (1.0/2/3)
        1/8             0.125
        1/8.            0.1875
        1/8t            (1.0/4/3)
        1/16            0.0625
        1/16.           0.09375
        1/16t           (1.0/8/3)
        1/32            0.03125
        1/32.           0.046875
        1/32t           (1.0/16/3)
        1/64            0.015625
        1/64.           0.0234375
        1/64t           (1.0/32/3)
    }

    variable knownDrumSounds {
	{acoustic bass drum} {bass drum 1} {side stick}
	{acoustic snare} {hand clap} {electric snare} {low floor tom}
	{closed hi-hat} {high floor tom} {pedal hi-hat}	{low tom} {open hi-hat}
	{low-mid tom} {hi-mid tom} {crash cymbal 1} {high tom} {ride cymbal 1}
	{chinese cymbal} {ride bell} tambourine {splash cymbal} cowbell
	{crash cymbal 2} vibraslap {ride cymbal 2} {hi bongo} {low bongo}
	{mute hi conga} {open hi conga} {low conga} {high timbale} {low timbale}
	{high agogo} {low agogo} cabasa maracas	{short whistle} {long whistle}
	{short guiro} {long guiro} claves {hi wood block} {low wood block}
	{mute cuica} {open cuica} {mute triangle} {open triangle}}

    variable knownPrograms {
	{acoustic grand} {bright acoustic} {electric grand} {honky-tonk}
	{electric piano 1} {electric piano 2} {harpsichord} {clavinet}
	{celesta} {glockenspiel} {music box} {vibraphone} {marimba} {xylophone}
	{tubular bells} {dulcimer} {drawbar organ} {percussive organ} 
	{rock organ} {church organ} {reed organ} {accoridan} {harmonica}
	{tango accordian} {nylon string guitar} {steel string guitar}
	{electric jazz guitar} {electric clean guitar} {electric muted guitar}
	{overdriven guitar} {distortion guitar} {guitar harmonics}
	{acoustic bass} {electric bass(finger)} {electric bass(pick)} 
	{fretless bass} {slap bass 1} {slap bass 2} {synth bass 1} 
	{synth bass 2} {violin} {viola} {cello} {contrabass} {tremolo strings} 
	{pizzicato strings} {orchestral strings} {timpani} {string ensemble 1} 
	{string ensemble 2} {synthstrings 1} {synthstrings 2} {choir aahs} 
	{voice oohs} {synth voice} {orchestra hit} {trumpet} {trombone} {tuba} 
	{muted trumpet} {french horn} {brass section} {synthbrass 1} 
	{synthbrass 2} {soprano sax} {alto sax} {tenor sax} {baritone sax} 
	{oboe} {english horn} {bassoon} {clarinet} {piccolo} {flute} {recorder} 
	{pan flute} {blown bottle} {skakuhachi} {whistle} {ocarina} 
	{lead 1 (square)} {lead 2 (sawtooth)} {lead 3 (calliope)} 
	{lead 4 (chiff)} {lead 5 (charang)} {lead 6 (voice)} {lead 7 (fifths)}
	{lead 8 (bass+lead)} {pad 1 (new age)} {pad 2 (warm)} 
	{pad 3 (polysynth)} {pad 4 (choir)} {pad 5 (bowed)} {pad 6 (metallic)}
	{pad 7 (halo)} {pad 8 (sweep)} {fx 1 (rain)} {fx 2 (soundtrack)} 
	{fx 3 (crystal)} {fx 4 (atmosphere)} {fx 5 (brightness)} 
	{fx 6 (goblins)} {fx 7 (echoes)} {fx 8 (sci-fi)} {sitar} {banjo} 
	{shamisen} {koto} {kalimba} {bagpipe} {fiddle} {shanai} {tinkle bell} 
	{agogo} {steel drums} {woodblock} {taiko drum} {melodic tom} 
	{synth drum} {reverse cymbal} {guitar fret noise} {breath noise} 
	{seashore} {bird tweet} {telephone ring} {helicopter} {applause} 
	{gunshot}}

    variable knownControllerNames
    array set knownControllerNames {
        {bank select (coarse)}                 0
        {modulation wheel (coarse)}            1
        {breath controller (coarse)}           2
        {foot pedal (coarse)}                  4
        {portamento time (coarse)}             5
        {data entry (coarse)}                  6
        {volume (coarse)}                      7
        {balance (coarse)}                     8
        {pan position (coarse)}                10
        {expression (coarse)}                  11
        {effect control 1 (coarse)}            12
        {effect control 2 (coarse)}            13
        {general purpose slider 1}             16
        {general purpose slider 2}             17
        {general purpose slider 3}             18
        {general purpose slider 4}             19
        {bank select (fine)}                   32
        {modulation wheel (fine)}              33
        {breath controller (fine)}             34
        {foot pedal (fine)}                    36
        {portamento time (fine)}               37
        {data entry (fine)}                    38
        {volume (fine)}                        39
        {balance (fine)}                       40
        {pan position (fine)}                  42
        {expression (fine)}                    43
        {effect control 1 (fine)}              44
        {effect control 2 (fine)}              45
        {general purpose slider 1 (fine)}      48
        {general purpose slider 2 (fine)}      49
        {general purpose slider 3 (fine)}      50
        {general purpose slider 4 (fine)}      51
        {hold pedal (on/off)}                  64
        {portamento (on/off)}                  65
        {sustenuto pedal (on/off)}             66
        {soft pedal (on/off)}                  67
        {legato pedal (on/off)}                68
        {hold 2 pedal (on/off)}                69
        {sound variation}                      70
        {sound timbre}                         71
        {sound release time}                   72
        {sound attack time}                    73
        {sound brightness}                     74
        {sound control 6}                      75
        {sound control 7}                      76
        {sound control 8}                      77
        {sound control 9}                      78
        {sound control 10}                     79
        {general purpose button 1 (on/off)}    80
        {general purpose button 2 (on/off)}    81
        {general purpose button 3 (on/off)}    82
        {general purpose button 4 (on/off)}    83
        {effects level}                        91
        {tremulo level}                        92
        {chorus level}                         93
        {celeste level}                        94
        {phaser level}                         95
        {ata button increment}                 96
        {data button decrement}                97
        {non-registered parameter (fine)}      98
        {non-registered parameter (coarse)}    99
        {registered parameter (fine)}          100
        {registered parameter (coarse)}        101
        {all sound off}                        120
        {all controllers off}                  121
        {local keyboard (on/off)}              122
        {all notes off}                        123
        {omni mode off}                        124
        {omni mode on}                         125
        {mono operation}                       126
        {poly operation}                       127
    }
}

namespace eval ::midi::file {}

namespace eval ::midi::track {}

namespace eval ::midi::event {}

proc ::midi::note_number { noteName } {
    variable ::midi::knownDrumSounds
    set nn [string trim [string tolower $noteName]]
    if { [string is integer -strict $nn] } {
	set nn [expr {$nn}]
	if { $nn < 0 || $nn > 127 } {
	    return -code error "Note '$noteName' is out of range \[0..127\]"
	}
	return $nn
    }
    if { [regexp -- {^([abcdefg]{1})([b#]*)(-1|[0123456789])$} $nn -> note modifiers octave] } {
	set noteIdx [expr {[lsearch {c - d - e f - g - a - b} $note] + ($octave+1) * 12}]
	foreach m [split $modifiers ""] {
	    switch $m {
		b  { incr noteIdx -1 }
		\# { incr noteIdx  1 }
	    }
	}
	if { $noteIdx < 0 || $noteIdx > 127 } {
	    return -code error "Note '$noteName' has note number out of range \[0..127\]: $noteIdx"
	}
	return $noteIdx
    }
    set noteIdx [lsearch $knownDrumSounds $noteName]
    if { $noteIdx >= 0 } {
	return [expr {$noteIdx + 35}]
    }
    return -code error "Unknown note name, note number or drum sound name '$noteName'"
}

proc ::midi::program_number { programName } {
    variable ::midi::knownPrograms
    set pn [string trim [string tolower $programName]]
    if { [string is integer -strict $pn] } {
	set pn [expr {$pn}]
	if { $pn < 0 || $pn > 127 } {
	    return -code error "Program '$programName' is out of range \[0..127\]"
	}
	return $pn
    }
    set idx [lsearch $knownPrograms $pn] 
    if { $idx >= 0 } {
	return $idx
    }
    return -code error "Unknown program name or number '$programName'"
}

# Map a controller name string defined in the knownControllerNames
# table to an integer controller number.

proc ::midi::controller_number { controllerName } {
    variable ::midi::knownControllerNames
    if {[string is integer -strict $controllerName]} {
        error "integer controller number should not be passed to this method"
    }
    set cn [string trim [string tolower $controllerName]]
    if {[info exists knownControllerNames($cn)]} {
        set cnum $knownControllerNames($cn)
        return $cnum
    }
    return -code error "Unknown controller name '$controllerName'"
}

proc midi::make_abs {ell} {
    set abt 0
    set l {}
    foreach el $ell {
	set dt [lindex $el 0]
	set e  [lindex $el 1]
	set abt [expr {$abt + $dt}]
	lappend l [list $abt $e]
    }
    return $l
}

proc midi::on_to_end {el} {
    set nell {}
    foreach e $el {
	if {[midi::event::type $e] eq "note_on"} {
	    set te [list $e 1]
	} else {
	    set te [list $e 0]
	}
	lappend nell $te
    }
    set nell [lsort -index 1 -integer -increasing $nell]
    set rel {}
    foreach el $nell {
	lappend rel [lindex $el 0]
    }
    return $rel
}

proc midi::make_delta {abtl} {
    # Put note_on after note_off events for group of events with same delta-time
    set rat 0
    set ctml {}
    set pdtl {}
    set pdtt 0
    foreach el $abtl {
	set at [lindex $el 0]
	set e  [lindex $el 1]
	set dt [expr {$at - $rat}]
	if { $dt } {
	    foreach pe [midi::on_to_end $pdtl] {
		lappend ctml [list $pdtt $pe]
		set pdtt 0
	    }
	    set pdtl {}
	    set pdtt $dt
	}
	lappend pdtl $e
	set rat $at
    }
    foreach pe [midi::on_to_end $pdtl] {
	lappend ctml [list $pdtt $pe]
	set pdtt 0
    }
    return $ctml
}

proc midi::merge {ell1 ell2} {
    # Make times absolute
    set abtl [midi::make_abs $ell1]
    eval lappend abtl [midi::make_abs $ell2]
    # Sort
    set abtl [lsort -index 0 -integer -increasing $abtl]
    # Make delta times again
    return [midi::make_delta $abtl]
}

namespace eval ::midi::file {
    variable file_options  {-type -time_division_type -ticks_per_beat -frames_per_second -ticks_per_frame -tracks}
    variable file_defaults {1     "ticks_per_beat"    480             -1                 -1               {}}
    variable file_id 0
}

proc ::midi::file { args } {
    variable ::midi::file::file_options
    variable ::midi::file::file_defaults
    variable ::midi::file::file_id
    variable ::midi::file::options
    variable ::midi::file::curr_velocity
    # Check arguments
    foreach {o v} $args {
	if { [lsearch $file_options $o] < 0 } {
	    error "Unknown option '$o' for midi file.  Only these options are supported: $file_options"
	}
	::midi::file::validate_option $o $v
    }
    # Create new file
    set id "midifile$file_id"
    incr file_id
    # Set default options
    foreach o $file_options v $file_defaults {
	set options($id,$o) $v
    }
    # Set options specified by user
    foreach {o v} $args {
	set options($id,$o) $v
	if { [string equal $o "-tracks"] } { 
	    ::midi::file::init_ref_counts $id $v
	}
    }
    set curr_velocity($id) 127
    # Create object command
    proc ::$id { cmd args } "eval ::midi::file::\$cmd $id \$args"
    # Return new file
    return $id
}

proc ::midi::file::init_ref_counts { id tl } { 
    variable refcnt
    foreach k [array names refcnt "*,$id"] {
	foreach {t f} [split $k ","] { break } 
	incr refcnt($t) -$refcnt($k)
	unset refcnt($k)
    }
    foreach t $tl {
	if { [info exists refcnt($t,$id)] } { 
	    incr refcnt($t,$id)
	} else {
	    set refcnt($t,$id) 1
	}
	if { [info exists refcnt($t)] } {
	    incr refcnt($t)
	} else {
	    set refcnt($t) 1
	}
    }    
}

proc ::midi::file::destroy { id } {
    variable file_options
    variable options
    variable refcnt
    variable curr_velocity
    # Unset tracks
    foreach t $options($id,-tracks) {
	if { [info exists refcnt($t,$id)] } { 
	    if { $refcnt($t,$id)  } { 
		incr refcnt($t,$id) -1
		incr refcnt($t) -1
	    } 
	    if { $refcnt($t,$id) <= 0 } { 
		unset refcnt($t,$id)
		if { $refcnt($t) <= 0 } {
		    unset refcnt($t)
		    $t destroy
		}
	    }
	}
    }
    # Unset all options
    foreach o $file_options {
	unset -nocomplain options($id,$o)
    }
    unset curr_velocity($id)
    # Remove object command
    rename ::$id {}
}

proc ::midi::file::reset { id } {
    variable file_defaults
    variable file_options
    variable options
    # Unset tracks
    foreach t $options($id,-tracks) {
	if { [info exists refcnt($t,$id)] } { 
	    if { $refcnt($t,$id)  } { 
		incr refcnt($t,$id) -1
		incr refcnt($t) -1
	    } 
	    if { $refcnt($t,$id) <= 0 } { 
		unset refcnt($t,$id)
		if { $refcnt($t) <= 0 } {
		    unset refcnt($t)
		    $t destroy
		}
	    }
	}
    }
    # Reset all options
    foreach o $file_options d $file_defaults {
	set options($id,$o) $d
    }
}

proc ::midi::file::validate { id } {
    variable file_options
    variable options
    validate_option -type $options($id,-type)
    validate_option -time_division_type $options($id,-time_division_type)
    switch -exact -- $options($id,-time_division_type) {
	"ticks_per_beat"    {
	    validate_option -ticks_per_beat $options($id,-ticks_per_beat)
	}
	"frames_per_second" {
	    validate_option -frames_per_second $options($id,-frames_per_second )
	    validate_option -ticks_per_frame $options($id,-ticks_per_frame)
	}
    }
    validate_option -tracks $options($id,-tracks)
}

proc ::midi::file::validate_option { option value } { 
    variable file_options
    switch -exact -- $option {
	"-type" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 2 } { 
		error "Unknown value '$value' for option '-type' of midi file. Use 0, 1 or 2."
	    }
	}
	"-time_division_type" {
	    if { [lsearch {ticks_per_beat frames_per_second} $value] < 0 } { 
		error "Unknown value '$value' for option '-time_division_type'. Use 'ticks_per_beat' or 'frames_per_second'."	
	    }
	}
	"-ticks_per_beat" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 32767 } { 
		error "Invalid value '$value' for option '-ticks_per_beat'. Value must be integer in range \[0..32767\]."
	    }
	}
	"-frames_per_second" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 127 } { 
		error "Invalid value '$value' for option '-frames_per_second'. Value must be integer in range \[0..127\]."
	    }
	}
	"-ticks_per_frame" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '-ticks_per_frame'. Value must be integer in range \[0..255\]."
	    }
	}
	"-tracks" {
	    foreach t $value {
		if { [catch {::midi::track::validate $t} msg] } { 
		    error "Invalid value '$t' for option '$option' of midi file: $msg. Value must be a valid midi track."
		}		
	    }
	}
	default {
	    error "Invalid option '$option' for midi file. Only these options are supported: $file_options"
	}
    }
}

proc ::midi::file::configure { id args } {
    variable file_options
    variable file_defaults
    variable options
    if { [llength $args] == 0 } { 
	# Return all options
	set l {}
	foreach o $file_options d $file_defaults {
	    set t [list $o $d $options($id,$o)]
	    lappend l $t
	}
	return $l
    } elseif { [llength $args] == 1 } { 
	# Return requested option
	set option [lindex $args 0]
	set idx [lsearch $file_options $option]
	if { $idx < 0 } { 
	    error "Unknown option '$option' for midi file.  Only these options are supported: $file_options"
	}
	return [list $option [lindex $file_defaults $idx] $options($id,$option)]
    } elseif { ([llength $args] % 2) == 0 } { 
	# Set specified option values
	# Check arguments
	foreach {o v} $args {
	    if { [lsearch $file_options $o] < 0 } {
		error "Unknown option '$o' for midi file.  Only these options are supported: $file_options"
	    }
	    validate_option $o $v
	}
	# Set options specified by user
	foreach {o v} $args {
	    set options($id,$o) $v
	    if { [string equal $o "-tracks"] } { 
		init_ref_counts $id $v
	    }
	}
    } else {
	error "Invalid number of arguments. Use without arguments to query all options, with 1 argument to query one option or with option-value pairs to set one or more options"
    }
}

proc ::midi::file::cget { id option } {
    variable file_options
    variable options
    if { [lsearch $file_options $option] < 0 } { 
	error "Unknown option '$option' for midi file.  Only these options are supported: $file_options"
    }
    return $options($id,$option)
}

proc ::midi::file::read { id filename } {
    variable options
    variable refcnt
    # Reset
    reset $id
    # Open in binary mode
    set f [open $filename r]
    fconfigure $f -encoding binary -translation binary

    # Read header tag
    if { [catch {_read_chars $f 4} hdr_tag] } {
	set errpos [tell $f]
	close $f
	reset $id
	error "Error reading MIDI file at position $errpos: could not read header tag: $hdr_tag"
    }
    if { ![string equal $hdr_tag "MThd"] } { 
	set errpos [tell $f]
	close $f
	reset $id
	error "Error reading MIDI file at position $errpos: file does not start with header tag 'MThd'"
    }
    # Read header size
    if { [catch {_read_uint32 $f} hdr_sz] } {
	set errpos [tell $f]
	close $f
	reset $id
	error "Error reading MIDI file at position $errpos: could not read header size: $hdr_sz"
    }
    if { $hdr_sz != 6 } { 
	set errpos [tell $f]
	close $f
	reset $id
	error "Error reading MIDI file at position $errpos: header size must be 6"
    }
    # Read format type
    if { [catch {_read_uint16 $f} hdr_ft] } {
	set errpos [tell $f]
	close $f
	reset $id
	error "Error reading MIDI file at position $errpos: could not read format type: $hdr_ft"
    }
    if { $hdr_ft != 0 && $hdr_ft != 1 } { 
	set errpos [tell $f]
	close $f
	reset $id
	error "Error reading MIDI file at position $errpos: unsupported format type '$hdr_ft'. Only format types '0' and '1' are allowed."
    }
    set options($id,-type) $hdr_ft
    # Read number of tracks
    if { [catch {_read_uint16 $f} hdr_nt] } {
	set errpos [tell $f]
	close $f
	reset $id
	error "Error reading MIDI file at position $errpos: could not read number of tracks: $hdr_nt"
    }
    # Read time division
    if { [catch {_read_uint16 $f} hdr_td] } {
	set errpos [tell $f]
	close $f
	reset $id
	error "Error reading MIDI file at position $errpos: could not read time division: $hdr_td"
    }
    if { $hdr_td & 0x8000 } { 
	set options($id,-time_division_type) "frames_per_second"
	set s [binary format S $hdr_td]
	binary scan $s cc t1 t2
	set options($id,-frames_per_second) [expr {-$t1}]
	set options($id,-ticks_per_frame)   [expr {$t2 & 0xff}]
    } else {
	set options($id,-time_division_type) "ticks_per_beat"
	set options($id,-ticks_per_beat)     [expr {$hdr_td & 0x7fff}]
    }
    # Read tracks
    for { set cnt 0 } { $cnt < $hdr_nt } { incr cnt } { 
	set t [::midi::track]
	if { [catch {$t _read $f} msg] } {
	    set errpos [tell $f]
	    close $f
	    reset $id
	    error "Error reading MIDI file at position $errpos: could not read track $cnt: $msg"
	}
	lappend options($id,-tracks) $t
	set refcnt($t,$id) 1
	set refcnt($t) 1
    }
    # Validate 
    if { [catch {validate $id} msg] } {
	close $f
	reset $id
	error "Error reading MIDI file: $msg"
    }
    # Close file
    close $f
}

proc ::midi::file::write { id filename args } {
    variable options
    variable running_status 0
    set tracks {}
    # If no -type has been given, assume user wants to write a type 1 file.
    set o -type
    if {$options($id,$o) == -1} {
        set options($id,$o) 1
    }
    # Check validity
    if { [catch {validate $id} msg] } {
	error "Error writing MIDI file: $msg"
    }
    # Parse arguments
    foreach {arg val} $args {
	switch -exact -- $arg {
	    -running_status {
		if { [string is boolean -strict $val] } { 
		    set running_status $val
		} else {
		    return -code error "Invalid value '$val' for argument '$arg'"
		}
	    }
	    -tracks {
		foreach t $val {
		    if { [string match "midi_track*" $t] && ([lsearch $options($id,-tracks) $t] >= 0) } {
			lappend tracks $t
		    } elseif { [string is integer -strict $t] && ($t < [llength $options($id,-tracks)]) } { 
			lappend tracks [lindex $options($id,-tracks) $t]
		    } else {
			return -code error "Unknown track '$t'"
		    }
		}
	    }
	    default {
		return -code error "Invalid argument '$arg'"
	    }
	}
    }
    # Write file
    set s ""
    # Write header tag
    append s "MThd"
    # Write header size
    append s [_write_uint32 6]
    # Write format type
    append s [_write_uint16 $options($id,-type)]
    # Write number of tracks
    if { [llength $tracks] } { 
	append s [_write_uint16 [llength $tracks]]
    } else {
	append s [_write_uint16 [llength $options($id,-tracks)]]
    }
    # Write time division
    switch -exact -- $options($id,-time_division_type) {
	"frames_per_second" {
	    set v [expr {((-$options($id,-frames_per_second)) & 0xff) << 8}]
	    set v [expr {$v + ($options($id,-ticks_per_frame) & 0xff)}]
	    append s [_write_uint16 $v]
	}
	"ticks_per_beat" {
	    append s [_write_uint16 $options($id,-ticks_per_beat)]
	}
    }
    foreach t $options($id,-tracks) {
	if { [llength $tracks] && ([lsearch $tracks $t] < 0) } { 
	    continue
	}
	if { [catch {$t _write $running_status} st]} {
	    error "Error writing MIDI file: $st"
	}
	append s $st
    }
    # Open file in binary mode
    set f [open $filename w]
    if { [catch {fconfigure $f -encoding binary -translation binary} msg] } {
	close $f
	reset $id
	error "Error writing MIDI file: could not put file in binary mode: $msg"
    }
    # Write midi data
    puts -nonewline $f $s
    # Close file
    close $f
    
}

proc ::midi::file::flatten { id args } { 
    variable options
    set tracks {}
    set no_sys_ex 0
    set no_meta 0
    set include_meta {}
    # Parse arguments
    foreach {arg val} $args {
	switch -exact -- $arg {
	    -tracks {
		foreach t $val {
		    if { [string match "midi_track*" $t] && ([lsearch $options($id,-tracks) $t] >= 0) } {
			lappend tracks $t
		    } elseif { [string is integer -strict $t] && ($t < [llength $options($id,-tracks)]) } { 
			lappend tracks [lindex $options($id,-tracks) $t]
		    } else {
			return -code error "Unknown track '$t'"
		    }
		}
	    }
	    -include_meta {
		set include_meta $val
	    }
	    -no_meta {
		set no_meta $val
	    }
	    -no_sys_ex {
		set no_sys_ex $val
	    }
	    default {
		return -code error "Invalid argument '$arg'"
	    }
	}
    }
    # Make all time stamps absolute
    set abtl {}
    foreach t $options($id,-tracks) {
	if { [llength $tracks] && ([lsearch $tracks $t] < 0) } { 
	    continue
	}
	set el [$t cget -events]
	if { $no_meta || $no_sys_ex || [llength $include_meta] } {
	    set nel {}
	    foreach del $el {
		set e [lindex $del 1]
		switch -exact -- [midi::event::type $e] {
		    Meta {
			if { !$no_meta } {
			    if {[llength $include_meta] == 0 || ([lsearch $include_meta [midi::event::type_meta_event $e]] >= 0) } {
				lappend nel $del
			    }
			}
		    }
		    SysEx {
			if { !$no_sys_ex } {
			    lappend nel $del
			}
		    }
		    default {
			lappend nel $del
		    }
		}
	    }
	    set el $nel
	}
	eval lappend abtl [midi::make_abs $el]
    }
    # Sort on absolute time
    set abtl [lsort -index 0 -integer -increasing $abtl]
    # Make delta times again
    return [midi::make_delta $abtl]
}

proc ::midi::file::dump { id args } { 
    variable options
    set tracks {}
    set filename {}
    # Parse arguments
    foreach {arg val} $args {
	switch -exact -- $arg {
	    -tracks {
		foreach t $val {
		    if { [string match "midi_track*" $t] && ([lsearch $options($id,-tracks) $t] >= 0) } {
			lappend tracks $t
		    } elseif { [string is integer -strict $t] && ($t < [llength $options($id,-tracks)]) } { 
			lappend tracks [lindex $options($id,-tracks) $t]
		    } else {
			return -code error "Unknown track '$t'"
		    }
		}
	    }
	    -file {
		set filename $val
	    }
	    default {
		return -code error "Invalid argument '$arg'"
	    }
	}
    }
    # Check validity
    if { [catch {validate $id} msg] } {
	error "Error dumping MIDI file: $msg"
    }

    set s ""

    # Write header tag
    append s "MThd" "\n"

    # Write header size
    append s "  header size: 6" "\n"

    # Write format type
    append s "  format type: $options($id,-type)" "\n"

    # Write number of tracks
    if { [llength $tracks] } { 
        append s "  number of tracks: [llength $tracks]" "\n"
    } else {
        append s "  number of tracks: [llength $options($id,-tracks)]" "\n"
    }

    # Write time division
    append s "  time division: "

    switch -exact -- $options($id,-time_division_type) {
	"frames_per_second" {
            append s "$options($id,-frames_per_second) frames per second, $options($id,-ticks_per_frame) ticks per frame" "\n"
	}
	"ticks_per_beat" {
            append s "$options($id,-ticks_per_beat) ticks per beat" "\n"
	}
    }

    append s "\n"
    foreach t $options($id,-tracks) {
	if { [llength $tracks] && ([lsearch $tracks $t] < 0) } { 
	    continue
	}
	#if { [catch {set ts [$t dump]} st]} {
	#    error "Error dumping MIDI file: $st"
	#}
        set ts [$t dump]
        append s $ts
    }

    if {$filename == {}} {
        return $s
    } else {
        # Open file in text mode
        set f [open $filename w]
        puts -nonewline $f $s
        close $f
    }
}

proc ::midi::file::clicks { id noteLength } {
    variable ::midi::file::options
    variable ::midi::knownDuration
    if { [string equal $options($id,-time_division_type) "ticks_per_beat"] } { 
	if { [info exists knownDuration($noteLength)] } {
	    set noteLength $knownDuration($noteLength)
	    return [expr int(round($options($id,-ticks_per_beat) * 4 * $noteLength))]
	}
	return -code error "Unknown note length: $noteLength, try any of [lsort -dictionary [array names knownDuration]]"
    }
    return -code error "This function can only be used when time division type was set to \"ticks_per_beat\""
}

proc midi::file::measure {id args} {
    variable curr_velocity
    set channel 0
    set elnm ""
    set eventl {}
    # Parse arguments
    foreach {arg val} $args {
	switch -exact -- $arg {
	    -channel {
		set channel $val
	    }
	    -events {
		set eventl $val
	    }
	}
    }    
    set el {}
    set dt 0
    foreach event $eventl {
	foreach {d nl v} $event break
	if {[string is integer -strict $v]} { set curr_velocoty $v }
	# Play notes
	foreach n $nl {
	    if {$n ne "r" && $n ne "R"} {
		lappend el [list $dt [midi::event note_on  -channel $channel -note $n -velocity $curr_velocity($id)]]
		set dt 0
	    }
	}
	# Stop notes
	incr dt [midi::file::clicks $id $d]
	foreach n $nl {
	    if {$n ne "r" && $n ne "R"} {
		lappend el [list $dt [midi::event note_off  -channel $channel -note $n -velocity $curr_velocity($id)]]
		set dt 0
	    }
	}
    }
    return $el
}

namespace eval midi::track {
    variable track_options  {-events}
    variable track_defaults {{}}
    variable track_id 0
}

proc ::midi::track { args } {
    variable ::midi::track::track_options
    variable ::midi::track::track_defaults
    variable ::midi::track::track_id
    variable ::midi::track::options
    # Check arguments
    foreach {o v} $args {
	if { [lsearch $track_options $o] < 0 } {
	    error "Unknown option '$o' for midi track.  Only these options are supported: $track_options"
	}
	::midi::track::validate_option $o $v
    }
    # Create new track
    set id "miditrack$track_id"
    incr track_id
    # Set default options
    foreach o $track_options v $track_defaults {
	set options($id,$o) $v
    }
    # Set options specified by user
    foreach {o v} $args {
	set options($id,$o) $v
    }
    # Create object command
    proc ::$id { cmd args } "eval ::midi::track::\$cmd $id \$args"
    # Return new track
    return $id
}

proc ::midi::track::destroy { id } {
    variable track_options
    variable options
    variable refcnt
    # Unset all options
    foreach o $track_options {
	unset options($id,$o)
    }
    # Remove object command
    rename ::$id {}
}

proc ::midi::track::reset { id } {
    variable track_options
    variable track_defaults
    variable options
    variable refcnt
    # Reset all options
    foreach o $track_options d $track_defaults {
	set options($id,$o) $d
    }
}

proc ::midi::track::validate { id } {
    variable track_options
    variable options
    foreach o $track_options {
	validate_option $o $options($id,$o)
    }
}

proc ::midi::track::validate_option { option value } { 
    variable track_options
    switch -exact -- $option {
	"-events" {
	    foreach el $value {
		set t [lindex $el 0]
		set e [lindex $el 1]
		if { ![string is integer -strict $t] || $t < 0 } { 
		    error "Invalid value '$el' for option '$option' of midi track. Delta time must be positive integer."
		}
		if { [llength $e] == 0 } { 
		    error "Invalid value '$el' for option '$option' of midi track: $msg. Not a valid midi event."
		}
		if { [catch {midi::event::validate $e} msg] } { 
		    error "Invalid value '$el' for option '$option' of midi track: $msg. Not a valid midi event: $msg"
		}
	    }
	}
	default {
	    error "Invalid option '$option' for midi track. Only these options are supported: $track_options"
	}
    }
}

proc ::midi::track::configure { id args } {
    variable track_options
    variable track_defaults
    variable options
    if { [llength $args] == 0 } { 
	# Return all options
	set l {}
	foreach o $track_options d $track_defaults {
	    set t [list $o $d $options($id,$o)]
	    lappend l $t
	}
	return $l
    } elseif { [llength $args] == 1 } { 
	# Return requested option
	set option [lindex $args 0]
	set idx [lsearch $track_options $option]
	if { $idx < 0 } { 
	    error "Unknown option '$option' for midi track.  Only these options are supported: $track_options"
	}
	return [list $option [lindex $track_defaults $idx] $options($id,$option)]
    } elseif { ([llength $args] % 2) == 0 } { 
	# Set specified option values
	# Check arguments
	foreach {o v} $args {
	    if { [lsearch $track_options $o] < 0 } {
		error "Unknown option '$o' for midi track.  Only these options are supported: $track_options"
	    }
	    validate_option $o $v
	}
	# Set options specified by user
	foreach {o v} $args {
	    set options($id,$o) $v
	}
    } else {
	error "Invalid number of arguments. Use without arguments to query all options, with 1 argument to query one option or with option-value pairs to set one or more options"
    }
}

proc ::midi::track::cget { id option } {
    variable track_options
    variable options
    if { [lsearch $track_options $option] < 0 } { 
	error "Unknown option '$option' for midi track.  Only these options are supported: $track_options"
    }
    return $options($id,$option)
}

proc ::midi::track::_read { id f } {
    variable options

    # Read track tag
    if { [catch {_read_chars $f 4} trk_tag] } {
	error "Error reading MIDI track: could not read track tag: $trk_tag"
    }
    if { ![string equal $trk_tag "MTrk"] } { 
	error "Error reading MIDI track: track does not start with header tag 'MTrk'"
    }
    # Read track size
    if { [catch {_read_uint32 $f} trk_sz] } {
	error "Error reading MIDI track: could not read track size: $trk_sz"
    }
    # Calculate position of end of track
    set eot [expr {[tell $f] +  $trk_sz}]

    # Read events
    set rstatus_type -1
    set end_of_track 0

    while { [tell $f] < $eot } {
#        puts "track: reading event at file position [expr {[tell $f] + 1}]"

	# Read delta time
	if { [catch {_read_vlv $f} ev_dt] } {
	    error "Error reading MIDI track: could not read delta time: $ev_dt"
	}
	# Read event type byte
	if { [catch {_read_uint8 $f} ev_tp] } {
	    error "Error reading midi event: could not read event type: $ev_tp"
	}
	if { (($ev_tp & 0x80) == 0) && $rstatus_type >= 0 } {
	    set ev_tp $rstatus_type
	    seek $f [expr {[tell $f] - 1}]
	}

        # Read event

        set bytes [list [_format_byte $ev_tp]]

        set type [midi::event::type $bytes]

	if { $type == "Meta" } {
	    # Read meta event
	    if { [catch {_read_uint8 $f} mev_tp] } {
		error "Error reading MIDI track: could not read meta event type: $mev_tp"
	    }
            lappend bytes [_format_byte $mev_tp]

            set mtype [midi::event::type_meta_event $bytes]

            if { [catch {::midi::event::_read $mtype $f bytes} msg] } {
                error "Error reading MIDI track: could not read meta event $mtype: $msg"
            }

            if {$mtype == "end_of_track"} {
		set end_of_track 1
            }
	} elseif { $type == "SysEx" } {
	    if { [catch {::midi::event::_read SysEx $f bytes} msg] } { 
		error "Error reading MIDI track: could not read sysex event: $msg"
	    }
	    if { $ev_tp <= 0xF7 } {
		# Cancel running status for System Common and System Exclusive events
		set rstatus_type -1
	    }
	} else {
	    # Channel event

            set channel [expr {$ev_tp & 0x0F}]

	    if { $channel < 0 || $channel > 15 } {
		error "Invalid MIDI channel event data : invalid channel '$channel'"
	    }

            if { [catch {::midi::event::_read $type $f bytes} msg] } {
                error "Error reading MIDI track: could not read channel event \"$type\" : $msg"
            }

	    set rstatus_type $ev_tp
	}
	if { [llength $bytes] == 0 } { 
	    error "Error reading MIDI event"
	}
	lappend options($id,-events) [list $ev_dt $bytes]
	if { $end_of_track } { 
	    break
	}
    }
}

proc ::midi::track::_write { id running_status } { 
    variable options
    set s ""
    # Write track tag
    append s "MTrk"
    # Write events
    set es ""
    set rstat 0
    foreach e $options($id,-events) {
	append es [_write_vlv [lindex $e 0]]
	if { $running_status } { 
	    set el [lindex $e 1]
	    set et [midi::event::type $el]
	    switch -exact -- $et {
		"Meta" -
		"SysEx" {
		    append es [binary format c* $el]
		    set rstat 0
		}
		default {
		    set cstat [lindex $el 0]
		    if { $rstat == $cstat } { 
			append es [binary format c* [lrange $el 1 end]]
		    } else {
			append es [binary format c* $el]
			set rstat $cstat
		    }
		}
	    }
	} else {
	    append es [binary format c* [lindex $e 1]]
	}
    }
    # Write track size
    append s [_write_uint32 [string length $es]]
    # Append events
    append s $es
    # Return string
    return $s
}

proc ::midi::track::dump { id } { 
    variable options
    set s ""

    append s "MTrk" "\n"
    append s "\n"
    append s "  Delta time    Absolute time Event" "\n"
    append s "  ------------- ------------- ----------------------------------------" "\n"

    set at 0
    foreach el $options($id,-events) {
	set dt [lindex $el 0]
	set e  [lindex $el 1]
	incr at $dt
        append s [format "  %13d %13d %s" $dt $at [midi::event::dump $e]] "\n"
    }
    append s "\n"

    return $s
}

namespace eval midi::event {
    variable event_types { "sequence_number" "text" "copyright" "track_name" "instrument_name" "lyric" "marker" "cue_point" "program_name" "device_name" "channel" "port" "end_of_track" "tempo" "smpte_offset" "time_signature" "key_signature" "sequencer_specific" "meta_unknown" "SysEx" "note_off" "note_on" "note_aftertouch" "controller" "program_change" "channel_aftertouch" "pitch_bend" }
    variable event_options
    variable event_defaults

    set event_options(sequence_number)     {-value}
    set event_defaults(sequence_number)    {0}
    set event_options(text)                {-value}
    set event_defaults(text)               {""}
    set event_options(copyright)           {-value}
    set event_defaults(copyright)          {""}
    set event_options(track_name)          {-value}
    set event_defaults(track_name)         {""}
    set event_options(instrument_name)     {-value}
    set event_defaults(instrument_name)    {""}
    set event_options(lyric)               {-value}
    set event_defaults(lyric)              {""}
    set event_options(marker)              {-value}
    set event_defaults(marker)             {""}
    set event_options(cue_point)           {-value}
    set event_defaults(cue_point)          {""}
    set event_options(program_name)        {-value}
    set event_defaults(program_name)       {""}
    set event_options(device_name)         {-value}
    set event_defaults(device_name)        {""}
    set event_options(channel)             {-channel}
    set event_defaults(channel)            {0}
    set event_options(port)                {-port}
    set event_defaults(port)               {0}
    set event_options(end_of_track)        {}
    set event_defaults(end_of_track)       {}
    set event_options(tempo)               {-tempo}
    set event_defaults(tempo)              {0x07A120}
    set event_options(smpte_offset)        {-hours -minutes -seconds -frames -framefractions}
    set event_defaults(smpte_offset)       {0      0        0        0       0}
    set event_options(time_signature)      {-numerator -denominator -clocks_ticks_between_metronome_clicks -notated_32nd_notes_in_quarter_note}
    set event_defaults(time_signature)     {0          1            0                                      0}
    set event_options(key_signature)       {-number_of_modifications -scale}
    set event_defaults(key_signature)      {0                        0}
    set event_options(sequencer_specific)  {-data}
    set event_defaults(sequencer_specific) {""}
    set event_options(meta_unknown)        {-meta_unknown_event_type -data}
    set event_defaults(meta_unknown)       {0                        ""}
    set event_options(SysEx)               {-sysex_event_type -data}
    set event_defaults(SysEx)              {0xF0              ""}
    set event_options(note_off)            {-channel -note -velocity}
    set event_defaults(note_off)           {0        0     0}
    set event_options(note_on)             {-channel -note -velocity}
    set event_defaults(note_on)            {0        0     0}
    set event_options(note_aftertouch)     {-channel -note -amount}
    set event_defaults(note_aftertouch)    {0        0     0}
    set event_options(controller)          {-channel -controller -value}
    set event_defaults(controller)         {0        0           0}
    set event_options(program_change)      {-channel -program}
    set event_defaults(program_change)     {0        0}
    set event_options(channel_aftertouch)  {-channel -amount}
    set event_defaults(channel_aftertouch) {0        0}
    set event_options(pitch_bend)          {-channel -amount}
    set event_defaults(pitch_bend)         {0        0}
}

proc ::midi::event { type args } {
    return [eval midi::event::create $type $args]
}

proc ::midi::event::create { type args } {
    variable event_options
    variable event_defaults
    set l [list $type]
    foreach option $event_options($type) def $event_defaults($type) {
	lappend l $option $def
    }
    foreach {o v} $args {
	if { [lsearch $event_options($type) $o] < 0 } {
	    error "Invalid option '$o' for event of type '$type'. Known options for this event type are: $event_options($type)"
	}
	_validate_option $type $o $v
	lappend l $o $v
    }
    set e [_unparse_event $l]
    return $e
}

proc ::midi::event::_read { type f varname } { 
    upvar $varname bytes
    switch -exact -- $type {
	"sequence_number" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta sequence number event: could not read length: $len"
	    }
	    if { $len != 2 } {
		error "Error reading MIDI meta sequence number event: invalid length $len, must be 2"
	    }
	    if { [catch {_read_uint16 $f} ev_seqnr] } {
		error "Error reading MIDI meta sequence number event: could not read sequence number: $ev_seqnr"
	    }
	    _vlv_to_bytes bytes 2
	    _uint16_to_bytes bytes $ev_seqnr
	}
	"text" -
	"copyright" -
	"track_name" -
	"instrument_name" -
	"lyric" -
	"marker" -
	"cue_point" -
	"program_name" -
	"device_name" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta $type event: could not read length: $len"
	    }
	    if { [catch {_read_chars $f $len} dta] } {
		error "Error reading MIDI meta $type event: could not read value: $dta"
	    }
	    _vlv_to_bytes bytes $len
	    _chars_to_bytes bytes $dta
	}
	"channel" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta channel prefix event: could not read length: $len"
	    }
	    if { $len != 1 } {
		error "Error reading MIDI meta channel prefix event: invalid length $len, must be 1"
	    }
	    if { [catch {_read_uint8 $f} ev_chan] } {
		error "Error reading MIDI meta channel prefix event: could not read channel: $ev_chan"
	    }
	    _vlv_to_bytes bytes 1
	    _uint8_to_bytes bytes $ev_chan
	}
	"port" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta port event: could not read length: $len"
	    }
	    if { $len != 1 } {
		error "Error reading MIDI meta port event: invalid length $len, must be 1"
	    }
	    if { [catch {_read_uint8 $f} ev_port] } {
		error "Error reading MIDI meta port event: could not read port: $ev_port"
	    }
	    _vlv_to_bytes bytes 1
	    _uint8_to_bytes bytes $ev_port
	}
	"end_of_track" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta end of track event: could not read length: $len"
	    }
	    if { $len != 0 } {
		error "Error reading MIDI meta end of track event: invalid length $len, must be 0"
	    }
	    _vlv_to_bytes bytes 0
	}
	"tempo" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta tempo event: could not read length: $len"
	    }
	    if { $len != 3 } {
		error "Error reading MIDI meta tempo event: invalid length $len, must be 3"
	    }
	    if { [catch {_read_uint24 $f} ev_tmp] } {
		error "Error reading MIDI meta tempo event: could not read tempo: $ev_tmp"
	    }
	    _vlv_to_bytes bytes 3
	    _uint24_to_bytes bytes $ev_tmp
	}
	"smpte_offset" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta SMPTE offset event: could not read length: $len"
	    }
	    if { $len != 5 } {
		error "Error reading MIDI meta SMPTE offset event: invalid length $len, must be 5"
	    }
	    if { [catch {_read_uint8 $f} ev_hr] } {
		error "Error reading MIDI meta SMPTE offset event: could not read hours: $ev_tmp"
	    }
	    if { [catch {_read_uint8 $f} ev_mn] } {
		error "Error reading MIDI meta SMPTE offset event: could not read minutes: $ev_tmp"
	    }
	    if { [catch {_read_uint8 $f} ev_se] } {
		error "Error reading MIDI meta SMPTE offset event: could not read seconds: $ev_tmp"
		    }
	    if { [catch {_read_uint8 $f} ev_fr] } {
			error "Error reading MIDI meta SMPTE offset event: could not read frames: $ev_tmp"
	    }
	    if { [catch {_read_uint8 $f} ev_ff] } {
		error "Error reading MIDI meta SMPTE offset event: could not read frame fractions: $ev_tmp"
	    }
	    _vlv_to_bytes bytes 5
	    _uint8_to_bytes bytes $ev_hr
	    _uint8_to_bytes bytes $ev_mn
	    _uint8_to_bytes bytes $ev_se
	    _uint8_to_bytes bytes $ev_fr
	    _uint8_to_bytes bytes $ev_ff
	}
	"time_signature" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta time signature event: could not read length: $len"
	    }
	    if { $len != 4 } {
		error "Error reading MIDI meta time signature event: invalid length $len, must be 4"
	    }
	    if { [catch {_read_uint8 $f} ev_nn] } {
		error "Error reading MIDI meta time signature event: could not read numerator: $ev_tmp"
	    }
	    if { [catch {_read_uint8 $f} ev_dd] } {
		error "Error reading MIDI meta time signature event: could not read denominator: $ev_tmp"
	    }
	    if { [catch {_read_uint8 $f} ev_cc] } {
		error "Error reading MIDI meta time signature event: could not read number of MIDI clocks between metronome clicks: $ev_tmp"
	    }
	    if { [catch {_read_uint8 $f} ev_bb] } {
		error "Error reading MIDI meta time signature event: could not read number of notated 32nd-notes in a MIDI quarter-note: $ev_tmp"
	    }
	    _vlv_to_bytes bytes 4
	    _uint8_to_bytes bytes $ev_nn
	    _uint8_to_bytes bytes $ev_dd
	    _uint8_to_bytes bytes $ev_cc
	    _uint8_to_bytes bytes $ev_bb
	}
	"key_signature" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta key signature event: could not read length: $len"
	    }
	    if { $len != 2 } {
		error "Error reading MIDI meta key signature event: invalid length $len, must be 2"
	    }
	    if { [catch {_read_int8 $f} ev_nsf] } {
		error "Error reading MIDI meta key signature event: could not read number of sharps/flats: $ev_nsf"
	    }
	    if { [catch {_read_uint8 $f} ev_scl] } {
		error "Error reading MIDI meta time signature event: could not read key: $ev_scl"
	    }
	    _vlv_to_bytes bytes 2
	    _int8_to_bytes bytes $ev_nsf
	    _uint8_to_bytes bytes $ev_scl
	}
	"sequencer_specific" -
	"meta_unknown" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI meta event: could not read length: $len"
	    }
	    if { [catch {_read_chars $f $len} dta] } {
		error "Error reading MIDI meta event: could not read data: $dta"
	    }
	    _vlv_to_bytes bytes $len
	    _chars_to_bytes bytes $dta
	}
	"SysEx" {
	    if { [catch {_read_vlv $f} len] } {
		error "Error reading MIDI sysex event: could not read length: $len"
	    }
	    if { [catch {_read_chars $f $len} dta] } {
		error "Error reading MIDI sysex event: could not read data: $dta"
	    }
	    _vlv_to_bytes bytes $len
	    _chars_to_bytes bytes $dta
	}
	"note_off" -
	"note_on" -
	"note_aftertouch" {
	    if { [catch {_read_uint8 $f} cev_nt] } {
		error "Error reading MIDI channel $type event: could not read note: $cev_nt"
	    }
	    if { [catch {_read_uint8 $f} cev_vel] } {
		error "Error reading MIDI channel $type event: could not read velocity: $cev_vel"
	    }
	    _uint8_to_bytes bytes $cev_nt
	    _uint8_to_bytes bytes $cev_vel
	}
	"controller" {
	    if { [catch {_read_uint8 $f} cev_nt] } {
		error "Error reading MIDI channel $type event: could not read controller: $cev_nt"
	    }
	    if { [catch {_read_uint8 $f} cev_vel] } {
		error "Error reading MIDI channel $type event: could not read velocity: $cev_vel"
	    }
	    _uint8_to_bytes bytes $cev_nt
	    _uint8_to_bytes bytes $cev_vel
	}
	"program_change" {
	    if { [catch {_read_uint8 $f} cev_pg] } {
		error "Error reading MIDI channel program change event: could not read program: $cev_pg"
	    }
	    _uint8_to_bytes bytes $cev_pg
	}
	"channel_aftertouch" {
	    if { [catch {_read_uint8 $f} cev_vel] } {
		error "Error reading MIDI channel aftertouch event: could not read amount: $cev_vel"
	    }
	    _uint8_to_bytes bytes $cev_vel
	}
	"pitch_bend" {
	    if { [catch {_read_uint8 $f} cev_lsb] } {
		error "Error reading MIDI channel pitch bend event: could not read LSB: $cev_lsb"
	    }
	    if { [catch {_read_uint8 $f} cev_msb] } {
		error "Error reading MIDI channel pitch bend event: could not read MSB: $cev_msb"
	    }
	    _uint8_to_bytes bytes $cev_lsb
	    _uint8_to_bytes bytes $cev_msb
	}
        default {
            error "unmatched event type \"$type\""
        }
    }

    # Validate bytes for event that was just read in.

    _parse_event $bytes
}

proc ::midi::event::dump { ebytes } {

    _parse_event_as_option_array $ebytes type options

    # Channel event

    set b1 [lindex $ebytes 0]

    set channel [expr {$b1 & 0x0F}]

    set s "$type "

    switch -exact -- $type {
	"sequence_number" {
            append s $options(-value)
	}
	"text" -
	"copyright" -
	"track_name" -
	"instrument_name" -
	"lyric" -
	"marker" -
	"cue_point" -
	"program_name" -
	"device_name" {
            append s $options(-value)
        }
	"channel" {        
            append s $options(-channel)
        }
	"port" {
            append s $options(-port)
        }
	"end_of_track" {
            # Trim last space off string
            set s [string range $s 0 end-1]
	}
	"tempo" {
            append s $options(-tempo)
	}
	"smpte_offset" {
            append s "hours $options(-hours), minutes $options(-minutes), seconds $options(-seconds), "
            append s "frames $options(-frames), framefractions $options(-framefractions)"
	}
	"time_signature" {
	    append s "$options(-numerator)/$options(-denominator), $options(-clocks_ticks_between_metronome_clicks) clocks ticks between metronome clicks, $options(-notated_32nd_notes_in_quarter_note) notated 32nd notes in quarter note"
	}
	"key_signature" {
            if {$options(-scale) == 0} {
                set mtype "major"
            } else {
                set mtype "minor"
            }

            if {$options(-number_of_modifications) < 0} {
                set mod_type "flats"
                set mod_num [expr {$options(-number_of_modifications) * -1}]
            } else {
                set mod_type "sharps"
                set mod_num $options(-number_of_modifications)
            }

	    append s "$mod_num $mod_type, $mtype"
	}
	"sequencer_specific" {
            append s $options(-data)
        }
	"meta_unknown" {
            append s "meta_unknown_event_type $options(-meta_unknown_event_type), data $options(-data)"
	}
	"SysEx" {
            append s "sysex_event_type $options(-sysex_event_type), data $options(-data)"
	}
	"note_off" -
	"note_on" {
	    append s "channel $options(-channel), note $options(-note), velocity $options(-velocity)"
	}
	"note_aftertouch" {
	    append s "channel $options(-channel), note $options(-note), amount $options(-amount)"
	}
	"controller" {
            append s "channel $options(-channel), controller $options(-controller), value $options(-value)"
	}
	"program_change" {
            append s "channel $options(-channel), program $options(-program)"
	}
	"channel_aftertouch" {
            append s "channel $options(-channel), amount $options(-amount)"
	}
	"pitch_bend" {
            append s "channel $options(-channel), amount $options(-amount)"
	}
    }
    return $s
}

proc ::midi::event::validate { ebytes } { 
    variable event_options
    foreach b $ebytes {
	if { $b < 0 || $b > 255 } { 
	    error "byte value $b out of range \[0..255\]"
	}
    }
    _parse_event_as_option_array $ebytes type options
    foreach o $event_options($type) {
	_validate_option $type $o $options($o)
    }
}

proc ::midi::event::validate_option { ebytes option value } { 
    variable event_options
    foreach b $ebytes {
	if { $b < 0 || $b > 255 } { 
	    error "byte value $b out of range \[0..255\]"
	}
    }
    _parse_event_as_option_array $ebytes type options
    if { [lsearch $event_options($type) $option] < 0 } {
	error "Invalid option '$option' for event of type '$type'. Known options for this event type are: $event_options($type)"
    }
    _validate_option $type $option $value
}

proc ::midi::event::_validate_option { type option value } { 
    variable event_options
    switch -exact -- $option {
	"-channel" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 15 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..15\]."
	    }
	}
	"-port" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 127 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..127\]."
	    }
	}	    
	"-tempo" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 0xffffff } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..16777215\]."
	    }
	}
	"-hours" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }
	}
	"-minutes" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
		    }
	}
	"-seconds" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }
	}
	"-frames" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }
	}
	"-framefractions" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }
	}
	"-numerator" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }
	}
	"-denominator" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }
	}
	"-clocks_ticks_between_metronome_clicks" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }
		}
	"-notated_32nd_notes_in_quarter_note" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }
	}
	"-number_of_modifications" {
	    if { ![string is integer -strict $value] || $value < -7 || $value > 7 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[-7..7\]."
	    }
	}
	"-scale" {
	    if { $value != 0 && $value != 1 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be 0 for 'major' or 1 for 'minor'."
	    }
	}
	"-note" {
	    if { [catch {::midi::note_number $value} msg] } {
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be an integer in range \[0..127\], a valid note name or a valid drum sound name."
	    }
	}	    
	"-velocity" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 127 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..127\]."
	    }
	}	    
	"-amount" {
	    switch -exact -- $type {
		"note_aftertouch" -
		"channel_aftertouch" {
		    if { ![string is integer -strict $value] || $value < 0 || $value > 127 } { 
			error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..127\]."
		    }
		}
		"pitch_bend" {
		    if { ![string is integer -strict $value] || $value < -8192 || $value > 8191 } { 
			error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[-8192..8191\]."
		    }		    
		}
	    }
	}	    
	"-controller" {
	    if {[string is integer -strict $value]} {
		set num [expr {int($value)}]
		if {$num < 0 || $num > 127} {
		    error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be an integer in range \[0..127\]."
		}
	    } else {
		# If value is not an integer, it must be a controller name
		if { [catch {::midi::controller_number $value} msg] } {
		    error "Invalid value '$value' for option '$option' for event of type '$type'. $msg."
		}
	    }
	}	    
	"-value" {
	    switch -exact -- $type {
		"text" -
		"copyright" -
		"track_name" -
		"instrument_name" -
		"lyric" -
		"marker" -
		"cue_point" -
		"program_name" -
		"device_name" {
		}
		"controller" {
		    if { ![string is integer -strict $value] || $value < 0 || $value > 127 } { 
			error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..127\]."
		    }
		}
		"sequence_number" {
		    if { ![string is integer -strict $value] || $value < 0 || $value > 65535 } { 
			error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..65535\]."
		    }
		}
	    }
	}	    
	"-program" {
	    if { [catch {midi::program_number $value} msg] } {
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be an integer in range \[0..127\] or a valid program name."
	    }
	}
	"-sysex_event_type" {
	    if { ![string is integer -strict $value] || $value < 240 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[240..255\]."
	    }		    
	}
	"-meta_unknown_event_type" {
	    if { ![string is integer -strict $value] || $value < 0 || $value > 255 } { 
		error "Invalid value '$value' for option '$option' for event of type '$type'. Value must be integer in range \[0..255\]."
	    }		    
	}
	"-data" {
	}
	default {
	    error "Invalid option '$option' for event of type '$type'. Known options for this event type are: $event_options($type)"
	}
    }
}

proc ::midi::event::configure { ebytes args } {
    variable event_options
    variable event_defaults
    _parse_event_as_option_array $ebytes type options
    if { [llength $args] == 0 } {
	# Return all options
	set results {}
	foreach option $event_options($type) d $event_defaults($type) {
	    lappend results [list $option $d $options($option)]
	}
	return $results
    } elseif { [llength $args] == 1 } {
	# Return requested option
	set option [lindex $args 0]
	set idx [lsearch $event_options($type) $option]
	if { $idx < 0 } { 
	    error "Invalid option '$option' for event of type '$type'. Known options for this event type are: $event_options($type)"
	}
	return [list $option [lindex $event_defaults($type) $idx] $options($option)]    
    } elseif { ([llength $args] % 2) == 0 } {
	# Set specified option values
	foreach {o v} $args {
	    _validate_option $type $o $v
	    if { [lsearch $event_options($type) $o] < 0 } {
		error "Invalid option '$o' for event of type '$type'. Known options for this event type are: $event_options($type)"
	    }
	    set options($o) $v
	}
	set l [list $type]
	foreach option $event_options($type) {
	    lappend l $option $options($option)
	}
	return [_unparse_event $l]
    } else {
	error "Invalid number of arguments. Use without arguments to query all options, with 1 argument to query one option or with option-value pairs to set one or more options"
    }
}

proc ::midi::event::cget { ebytes option } { 
    variable event_options
    _parse_event_as_option_array $ebytes type options
    if { ![info exists options($option)] } {
	error "Invalid option '$option' for event of type '$type'. Known options for this event type are: $event_options($type)"
    }
    return $options($option)
}

proc ::midi::event::reset { ebytes } { 
    variable event_options
    variable event_defaults
    _parse_event_as_option_array $ebytes type options
    set l [list $type]
    foreach option $event_options($type) dval $event_defaults($type) {
	lappend l $option $dval
    }
    return [_unparse_event $l]
}

# Parse a list of event type and option-values pairs and return 
# a list of event bytes

proc ::midi::event::_unparse_event { tolist } {
    variable event_types
    set type [lindex $tolist 0]
    array set options [lrange $tolist 1 end]
    set results {}
    switch -- $type {
	"sequence_number" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x00
	    if { $options(-value) } {
		_uint8_to_bytes results 0x02
		_uint16_to_bytes results $options(-value)
	    } else {
		_uint8_to_bytes results 0x00
	    }
	}
	"text" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x01
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
	"copyright" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x02
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
        "track_name" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x03
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
        "instrument_name" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x04
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
        "lyric" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x05
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
        "marker" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x06
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
        "cue_point" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x07
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
	"program_name" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x08
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
        "device_name" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x09
	    _vlv_to_bytes results [string length $options(-value)]
	    _chars_to_bytes results $options(-value)
	}
	"channel" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x20
	    _uint8_to_bytes results 0x01
	    _uint8_to_bytes results $options(-channel)
        }
        "port" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x21
	    _uint8_to_bytes results 0x01
	    _uint8_to_bytes results $options(-port)
        }
	"end_of_track" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x2f
	    _uint8_to_bytes results 0x00
	}
	"tempo" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x51
	    _uint8_to_bytes results 0x03
	    _uint24_to_bytes results $options(-tempo)
	}
	"smpte_offset" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x54
	    _uint8_to_bytes results 0x05
	    _uint8_to_bytes results $options(-hours)
	    _uint8_to_bytes results $options(-minutes)
	    _uint8_to_bytes results $options(-seconds)
	    _uint8_to_bytes results $options(-frames)
	    _uint8_to_bytes results $options(-framefractions)
	}
	"time_signature" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x58
	    _uint8_to_bytes results 0x04
	    _uint8_to_bytes results $options(-numerator)
	    _uint8_to_bytes results [expr {round(log($options(-denominator)) / log(2))}]
	    _uint8_to_bytes results $options(-clocks_ticks_between_metronome_clicks)
	    _uint8_to_bytes results $options(-notated_32nd_notes_in_quarter_note)
	}
	"key_signature" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x59
	    _uint8_to_bytes results 0x02
	    _int8_to_bytes results $options(-number_of_modifications)
	    _uint8_to_bytes results $options(-scale)
	}
	"sequencer_specific" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results 0x7f
	    _vlv_to_bytes results [llength $options(-data)]
	    foreach b $options(-data) {
		lappend results [_format_byte $b]
	    }
        }
	"meta_unknown" {
	    _uint8_to_bytes results 0xff
	    _uint8_to_bytes results $options(-meta_unknown_event_type)
	    _vlv_to_bytes results [llength $options(-data)]
	    foreach b $options(-data) {
		lappend results [_format_byte $b]
	    }
	}
	"SysEx" {
	    _uint8_to_bytes results $options(-sysex_event_type)
	    _vlv_to_bytes results [llength $options(-data)]
	    foreach b $options(-data) {
		lappend results [_format_byte $b]
	    }
	}
	"note_off" {
	    _uint8_to_bytes results [expr {0x80 | ($options(-channel) & 0x0F)}]
	    _uint8_to_bytes results [expr {[midi::note_number $options(-note)] & 0xff}]
	    _uint8_to_bytes results [expr {$options(-velocity) & 0xff}]
	}
	"note_on" {
	    _uint8_to_bytes results [expr {0x90 | ($options(-channel) & 0x0F)}]
	    _uint8_to_bytes results [expr {[midi::note_number $options(-note)] & 0xff}]
	    _uint8_to_bytes results [expr {$options(-velocity) & 0xff}]
	}
	"note_aftertouch" {
	    _uint8_to_bytes results [expr {0xA0 | ($options(-channel) & 0x0F)}]
	    _uint8_to_bytes results [expr {[midi::note_number $options(-note)] & 0xff}]
	    _uint8_to_bytes results [expr {$options(-amount) & 0xff}]
	}
	"controller" {
	    _uint8_to_bytes results [expr {0xB0 | ($options(-channel) & 0x0F)}]
	    set ctrl $options(-controller)
	    if {![string is integer -strict $ctrl]} {
		set ctrl [midi::controller_number $ctrl]
	    }
	    _uint8_to_bytes results [expr {$ctrl & 0xff}]
	    _uint8_to_bytes results [expr {$options(-value) & 0xff}]
	}
	"program_change" {
	    _uint8_to_bytes results [expr {0xC0 | ($options(-channel) & 0x0F)}]
	    _uint8_to_bytes results [expr {[midi::program_number $options(-program)] & 0xff}]
	}
	"channel_aftertouch" {
	    _uint8_to_bytes results [expr {0xD0 | ($options(-channel) & 0x0F)}]
	    _uint8_to_bytes results [expr {$options(-amount) & 0xff}]
	}
	"pitch_bend" {
	    _uint8_to_bytes results [expr {0xE0 | ($options(-channel) & 0x0F)}]
	    set lsb [expr {($options(-amount) + 8192) & 0x7f}]
	    set msb [expr {(($options(-amount) + 8192) >> 7) & 0x7f}]
	    _uint8_to_bytes results $lsb
	    _uint8_to_bytes results $msb
	}
	default {
	    error "Invalid event type '$type'. Known event types are: $event_types"
	}
    }

    return $results
}

# Parse a list of event bytes into a configuration style
# list that contains options and values specific to each
# type of event.
#
# Example: 
#
# {0x90 0xC0 0x7F}
#
# returns
#
# {note_on -channel 0 -note 0xC0 -velocity 0x7F}

proc ::midi::event::_parse_event { ebytes } {
#    puts "midi::event::_parse_event \{$ebytes\}"

    set results [list]

    set type [type $ebytes]

    switch -- $type {
        "" {
            error "empty type result for \{$ebytes\}"
        }
        "Meta" {
            # Switch on meta event type
            lappend results $type
            set type [type_meta_event $ebytes]
        }
        "SysEx" {
            # No-op for SysEx message
        }
        default {
            # No-op for Channel message
        }
    }

    lappend results $type

    # Channel event

    set b1 [lindex $ebytes 0]

    set channel [_format_byte [expr {$b1 & 0x0F}]]

    set s "$type "

    switch -exact -- $type {
	"sequence_number" {
            # FF 00 00 : MTrk location is midi cue
            # FF 00 02 BYTE BYTE : 2 BYTEs indicate midi cue

            set seq_bytes [lrange $ebytes 2 end]
            set tuple [_get_vlv $seq_bytes]
            set num [lindex $tuple 0]
            set rest [lindex $tuple 1]

            #  Where should event be validated?

            if {[llength $seq_bytes] == 1} {
                if {$num != 0} {
                    error "sequence_number byte 3 must be zero : \{$ebytes\}"
                }
                lappend results -value $num
            } else {
                if {$num != 2 || [llength $rest] != 2} {
                    error "sequence_number must have length 2 and 2 bytes values : \{$ebytes\}"
                }
                set rest_num [_bytes_to_uint16 $rest]
                lappend results -value $rest_num
            }
	}
	"text"  -
	"copyright"  -
        "track_name" -
        "instrument_name" -
        "lyric" -
        "marker" -
        "cue_point" -
	"program_name" -
        "device_name" {
            # Text Type Events
            #
            # 0xFF NUM VARNUM TEXT
            #
            # NUM is one of:
            #
            # 0x1 : Text
            # 0x2 : Copyright
            # 0x3 : Sequence/Track Name
            # 0x4 : Instrument Name
            # 0x5 : Lyric
            # 0x6 : Marker
            # 0x7 : Cue Point
            # 0x8 : Program Name (readable name of program that plays MTrk)
            # 0x9 : Device (Port) Name

            set text_bytes [lrange $ebytes 2 end]
            set tuple [_get_vlv $text_bytes]
            set len [lindex $tuple 0]
            set rest [lindex $tuple 1]
            set rest_str [_bytes_to_chars $rest]
            set rest_len [string length $rest_str]

            if {$len != $rest_len} {
                error "text length should be $len but it was $rest_len"
            }

            lappend results -value $rest_str
	}
	"channel" {
            # FF 20 01 CHANNEL_BYTE

            if {[lindex $ebytes 2] != 1} {
                error "channel event must have the value 0x01 as the third byte"
            }

            set bytes [lrange $ebytes 3 end]
            set channel [_bytes_to_uint8 $bytes]

            lappend results -channel $channel
        }
        "port" {
            # FF 21 01 PORT_BYTE

            if {[lindex $ebytes 2] != 1} {
                error "port event must have the value 0x01 as the third byte"
            }

            set bytes [lrange $ebytes 3 end]
            set port [_bytes_to_uint8 $bytes]

            lappend results -port $port
        }
	"end_of_track" {
            # FF 21 00

            if {[lindex $ebytes 2] != 0} {
                error "end_of_track event must have the value 0x00 as the third byte"
            }
	}
	"tempo" {
            # FF 51 03 BYTE BYTE BYTE

            if {[lindex $ebytes 2] != 3} {
                error "tempo event must have the value 0x03 as the third byte"
            }

            set bytes [lrange $ebytes 3 end]
            set tempo [_bytes_to_uint24 $bytes]

            lappend results -tempo $tempo
	}
	"smpte_offset" {
            # FF 54 05 HR MN SE FR FF

            if {[lindex $ebytes 2] != 5} {
                error "smpte_offset event must have the value 0x05 as the third byte"
            }

            set hours [_bytes_to_uint8 [lindex $ebytes 3]]
            set minutes [_bytes_to_uint8 [lindex $ebytes 4]]
            set seconds [_bytes_to_uint8 [lindex $ebytes 5]]
            set frames [_bytes_to_uint8 [lindex $ebytes 6]]
            set framefractions [_bytes_to_uint8 [lindex $ebytes 7]]

            lappend results -hours $hours -minutes $minutes -seconds $seconds \
                -frames $frames -framefractions $framefractions
	}
	"time_signature" {
            # FF 58 04 NN DD CC BB

            if {[lindex $ebytes 2] != 4} {
                error "time_signature event must have the value 0x04 as the third byte"
            }

            set numerator [_bytes_to_uint8 [lindex $ebytes 3]]
            set pow [_bytes_to_uint8 [lindex $ebytes 4]]
            set denominator [expr {int(pow(2,$pow))}]
            set clocks [_bytes_to_uint8 [lindex $ebytes 5]]
            set notes [_bytes_to_uint8 [lindex $ebytes 6]]

            lappend results -numerator $numerator -denominator $denominator \
                -clocks_ticks_between_metronome_clicks $clocks \
                -notated_32nd_notes_in_quarter_note $notes
	}
	"key_signature" {
            # FF 59 02 SF MI

            if {[lindex $ebytes 2] != 2} {
                error "key_signature event must have the value 0x02 as the third byte"
            }

            set sf [_bytes_to_int8 [lindex $ebytes 3]]
            set mi [_bytes_to_uint8 [lindex $ebytes 4]]

            if {$sf < -7 || $sf > 7} {
                error "key_signature must have value -7 to 7 for fifth byte"
            }

            if {$mi != 0 && $mi != 1} {
                error "key_signature must have value 0x0 or 0x1 as fifth byte"
            }

            lappend results \
                -number_of_modifications $sf \
                -scale $mi
	}
	"sequencer_specific" {
            # FF 7F LEN DATA

            set data_bytes [lrange $ebytes 2 end]
            set tuple [_get_vlv $data_bytes]
            set len [lindex $tuple 0]
            set rest [lindex $tuple 1]
            set rest_len [llength $rest]

            if {$len != $rest_len} {
                error "data length should be $len but it was $rest_len"
            }

            lappend results -data $rest
        }
	"meta_unknown" {
            # FF ?? ?? ?? ??

            # Don't bother trying to validate unknown data bytes.

            set type [lindex $ebytes 1]
            set data_bytes [lrange $ebytes 2 end]

            set tuple [_get_vlv $data_bytes]
            set len [lindex $tuple 0]
            set rest [lindex $tuple 1]
            set rest_len [llength $rest]

            if {$len != $rest_len} {
                error "data length should be $len but it was $rest_len"
            }

            lappend results -meta_unknown_event_type $type -data $rest
	}
	"SysEx" {
	    # >=F0 ?? ?? ?? ?? 

            set type [lindex $ebytes 0]
            set data_bytes [lrange $ebytes 1 end]

            set tuple [_get_vlv $data_bytes]
            set len [lindex $tuple 0]
            set rest [lindex $tuple 1]
            set rest_len [llength $rest]

            if {$len != $rest_len} {
                error "data length should be $len but it was $rest_len"
            }

            lappend results -sysex_event_type $type -data $rest 
	}
	"note_off" -
	"note_on" {
            set note [lindex $ebytes 1]
            set velocity [lindex $ebytes 2]

            lappend results -channel $channel -note $note -velocity $velocity
	}
	"note_aftertouch" {
            set note [lindex $ebytes 1]
            set amount [lindex $ebytes 2]

            lappend results -channel $channel -note $note -amount $amount
	}
	"controller" {
            set controller [lindex $ebytes 1]
            set value [lindex $ebytes 2]

            lappend results -channel $channel -controller $controller -value $value
	}
	"program_change" {
            set program [lindex $ebytes 1]

            lappend results -channel $channel -program $program
	}
	"channel_aftertouch" {
            set amount [lindex $ebytes 1]

            lappend results -channel $channel -amount $amount
	}
	"pitch_bend" {
            set lsb [lindex $ebytes 1]
	    set msb [lindex $ebytes 2]
	    if { ![string is integer -strict $lsb] } {
                error "lsb must be integer"
	    }
	    if { ![string is integer -strict $msb] } {
                error "msb must be integer"
	    }
	    set amount [expr {($msb << 7) + $lsb - 8192}]
            lappend results -channel $channel -amount $amount
	}
    }

    return $results
}

proc ::midi::event::_parse_event_as_option_array { ebytes tp_name opar_name } {
    upvar $tp_name type
    upvar $opar_name options
    set results [_parse_event $ebytes]
    set type [lindex $results 0]
    set olist [lrange $results 1 end]
    if {$type == "Meta"} {
        set type [lindex $results 1]
        set olist [lrange $results 2 end]
    }
    array set options $olist
}

# Given a list of bytes, return the event type.
#
# Note that a partial list can be passed here,
# since this method only reads as many bytes
# as are needed to determine the type.
#
# Return types:
#
# "Meta" : Event is a non-midi Meta event
#          since it beings with the 0xFF byte
#
# "SysEx" : Event is a System Exclusive event type.
#
# Any of the channel specific events types could
# also be returned.

proc ::midi::event::type { ebytes } {
#    puts "midi::event::type \{$ebytes\}"

    set byte [lindex $ebytes 0]

    # FIXME: Validate byte list

    if {$byte == 0xFF} {
        # Non-midi meta event
        return "Meta"
    } elseif {$byte >= 0xF0} {
        # SysEx event
        return "SysEx"
    } else {
        # Channel event
        return [_type_channel_event $ebytes]
    }
}

# Return type for an event known to be a channel event.

proc ::midi::event::_type_channel_event { ebytes } {
#    if {[llength $ebytes] != 3} {
#        error "channel event should be 3 bytes : \{$ebytes\}"
#    }

    set b [lindex $ebytes 0]
    set byte [expr {$b & 0xF0}]

    if {$byte == 0x80} {
        return "note_off"
    } elseif {$byte == 0x90} {
        return "note_on"
    } elseif {$byte == 0xA0} {
        return "note_aftertouch"
    } elseif {$byte == 0xB0} {
        return "controller"
    } elseif {$byte == 0xC0} {
        return "program_change"
    } elseif {$byte == 0xD0} {
        return "channel_aftertouch"
    } elseif {$byte == 0xE0} {
        return "pitch_bend"
    } else {
        error "invalid channel event : [_format_byte $byte]"
    }
}

# Return type for an event known to be a Meta event.

proc ::midi::event::type_meta_event { ebytes } {
    set byte [lindex $ebytes 0]

    if {$byte != 0xFF} {
        error "first byte of Meta event must be 0xFF"
    }

    set byte [lindex $ebytes 1]

    # FIXME: Create generic byte list validation method and
    # invoke it from helper methods?

    if {$byte == 0x00} {
        return "sequence_number"
    } elseif {$byte == 0x01} {
        return "text"
    } elseif {$byte == 0x02} {
        return "copyright"
    } elseif {$byte == 0x03} {
        # Sequence/Track name event
        return "track_name"
    } elseif {$byte == 0x04} {
        return "instrument_name"
    } elseif {$byte == 0x05} {
        return "lyric"
    } elseif {$byte == 0x06} {
        return "marker"
    } elseif {$byte == 0x07} {
        return "cue_point"
    } elseif {$byte == 0x08} {
        return "program_name"
    } elseif {$byte == 0x09} {
        return "device_name"
    } elseif {$byte == 0x20} {
        return "channel"
    } elseif {$byte == 0x21} {
        return "port"
    } elseif {$byte == 0x2F} {
        return "end_of_track"
    } elseif {$byte == 0x51} {
        return "tempo"
    } elseif {$byte == 0x54} {
        return "smpte_offset"
    } elseif {$byte == 0x58} {
        return "time_signature"
    } elseif {$byte == 0x59} {
        return "key_signature"
    } elseif {$byte == 0x7F} {
        return "sequencer_specific"
    } else {
        return "meta_unknown"
    }
}

# Format a byte as a hex string

proc ::midi::_format_byte { byte } {
    return [format "0x%.2X" $byte]
}

# Convert a list of byte values into a
# character string.

proc ::midi::_bytes_to_chars { bytes } {
    set str ""
    foreach byte $bytes {
        append str [format %c $byte]
    }
    return $str
}

# Convert a list of 4 bytes to a uint32

proc ::midi::_bytes_to_uint32 { bytes } {
    set bstr [binary format c4 $bytes]
    return [_get_uint32 $bstr]
}

# Convert a list of 3 bytes to a uint24

proc ::midi::_bytes_to_uint24 { bytes } {
    set bstr \x00
    append bstr [binary format c3 $bytes]
    return [_get_uint24 $bstr]
}

# Convert a list of 2 bytes to a uint16

proc ::midi::_bytes_to_uint16 { bytes } {
    set bstr [binary format c2 $bytes]
    return [_get_uint16 $bstr]
}

# Convert a list of 1 bytes to a uint8

proc ::midi::_bytes_to_uint8 { bytes } {
    set bstr [binary format c1 $bytes]
    return [_get_uint8 $bstr]
}

# Convert a list of 1 bytes to a int8

proc ::midi::_bytes_to_int8 { bytes } {
    set bstr [binary format c1 $bytes]
    return [_get_int8 $bstr]
}

proc ::midi::_read_chars { f l } {
    return [read $f $l]
}

proc ::midi::_write_chars { dta } { 
    return $dta
}

proc ::midi::_chars_to_bytes { snm dta } { 
    upvar $snm s
    if { [binary scan $dta c* cl] } {
	foreach c $cl {
            set b [_format_byte [expr {$c & 0xFF}]]
	    lappend s $b
	}
    }
    return
}

proc ::midi::_read_uint32 { f } { 
    set s [read $f 4]
    return [_get_uint32 $s]
}

# Read an unsigned 32 bit integer value
# stored in Big Endian byte order from a
# binary string of length 4.

proc ::midi::_get_uint32 { bstr } {
    if {[string length $bstr] != 4} {
        error "bstr must be a string of length 4"
    }
    if {[binary scan $bstr I num] != 1} {
        # This should never happen
        error "could not convert bytes to uint32"
    }
    return [expr {$num & 0xFFFFFFFF}]
}

proc ::midi::_write_uint32 { i } { 
    return [binary format I $i]
}
proc ::midi::_uint32_to_bytes { snm i } { 
    upvar $snm s
    foreach val [list \
            [expr {($i >> 24) & 0xff}] \
            [expr {($i >> 16) & 0xff}] \
            [expr {($i >> 8) & 0xff}] \
            [expr {$i & 0xff}]] {
        set b [_format_byte $val]
        lappend s $b
    }
    return
}

proc ::midi::_read_uint24 { f } { 
    set s \x00
    append s [read $f 3]
    return [_get_uint24 $s]
}

# Read an unsigned 24 bit integer value
# stored in Big Endian byte order from a
# binary string of length 4.

proc ::midi::_get_uint24 { bstr } {
    if {[string length $bstr] != 4} {
        error "bstr must be a string of length 4"
    }
    if {[binary scan $bstr I num] != 1} {
        # This should never happen
        error "could not convert bytes to uint24"
    }
    return [expr {$num & 0xFFFFFF}]
}

proc ::midi::_write_uint24 { i } { 
    return [string range [binary format I $i] 1 end]
}

proc ::midi::_uint24_to_bytes { snm i } { 
    upvar $snm s
    foreach val [list \
            [expr {($i >> 16) & 0xff}] \
            [expr {($i >> 8) & 0xff}] \
            [expr {$i & 0xff}]] {
        set b [_format_byte $val]
        lappend s $b
    }
    return
}

proc ::midi::_read_uint16 { f } { 
    set s [read $f 2]
    return [_get_uint16 $s]
}

# Read an unsigned 16 bit integer value
# stored in Big Endian byte order from a
# binary string of length 2.

proc ::midi::_get_uint16 { bstr } {
    if {[string length $bstr] != 2} {
        error "bstr must be a string of length 2"
    }
    if {[binary scan $bstr S num] != 1} {
        # This should never happen
        error "could not convert bytes to uint16"
    }
    return [expr {$num & 0xFFFF}]
}

proc ::midi::_write_uint16 { i } { 
    return [binary format S $i]
}

proc ::midi::_uint16_to_bytes { snm i } { 
    upvar $snm s
    foreach val [list \
            [expr {($i >> 8) & 0xff}] \
            [expr {$i & 0xff}]] {
        set b [_format_byte $val]
        lappend s $b
    }
    return
}

# Read an unsigned 8 bit integer value
# from a binary string of length 1.

proc ::midi::_read_uint8 { f } { 
    set s [read $f 1]
    return [_get_uint8 $s]
}

proc ::midi::_get_uint8 { bstr } {
    if {[string length $bstr] != 1} {
        error "bstr must be a string of length 1"
    }
    if {[binary scan $bstr c num] != 1} {
        # This should never happen
        error "could not convert byte to uint8"
    }
    return [expr {$num & 0xFF}]
}

proc ::midi::_write_uint8 { i } { 
    return [binary format c [expr {$i}]]
}

proc ::midi::_uint8_to_bytes { snm i } { 
    upvar $snm s
    set b [_format_byte [expr {$i & 0xff}]]
    lappend s $b
    return
}

proc ::midi::_read_int8 { f } { 
    set s [read $f 1]
    return [_get_int8 $s]
}

# Read a signed 8 bit integer value
# from a binary string of length 1.

proc ::midi::_get_int8 { bstr } {
    if {[string length $bstr] != 1} {
        error "bstr must be a string of length 1"
    }
    if {[binary scan $bstr c num] != 1} {
        # This should never happen
        error "could not convert byte to int8"
    }
    return $num
}

proc ::midi::_write_int8 { i } { 
    return [binary format c $i]
}

proc ::midi::_int8_to_bytes { snm i } { 
    upvar $snm s
    set b [_format_byte [expr {$i & 0xff}]]
    lappend s $b
    return
}

proc ::midi::_read_vlv { f } {
    set s [read $f 1]
    if { [binary scan $s c t] != 1 } {
	error "Could not read variable length value from $f"
    }
    set dt [expr {$t & 0x7F}]
    while { $t & 0x80 } {
	set s [read $f 1]
	if { [binary scan $s c t] != 1 } {
	    error "Could not read variable length value from $f"
	}
	set dt [expr {($dt << 7) + ($t & 0x7F)}]
    }
    return $dt
}

# Given a list of bytes, parse a variable length value
# from the bytes at the front of the list. Return a
# tuple of the variable length value and the bytes
# that appear after the variable length value.

proc ::midi::_get_vlv { bytes } {
    set i 0
    set t [lindex $bytes $i]
    incr i
    set dt [expr {$t & 0x7F}]
    while { $t & 0x80 } {
        set t [lindex $bytes $i]
        incr i
	set dt [expr {($dt << 7) + ($t & 0x7F)}]
    }
    return [list $dt [lrange $bytes $i end]]
}

proc ::midi::_write_vlv { v } {
    set v3 [expr {($v >> 21) & 0x7F}]
    set v2 [expr {($v >> 14) & 0x7F}]
    set v1 [expr {($v >>  7) & 0x7F}]
    set v0 [expr { $v        & 0x7F}]
    set s ""
    if { $v3 } {
	append s [binary format c [expr {$v3 | 0x80}]]
    }
    if { $v3 || $v2 } {
	append s [binary format c [expr {$v2 | 0x80}]]
    }
    if { $v3 || $v2 || $v1 } {
	append s [binary format c [expr {$v1 | 0x80}]]
    }
    append s [binary format c $v0]	   
    return $s
}

proc ::midi::_vlv_to_bytes { varname v } {
    upvar $varname s

    set bstr [_write_vlv $v]
    set len [string length $bstr]
    for {set i 0} {$i < $len} {incr i} {
        set c [string index $bstr $i]
        if {[scan $c %c v] != 1} {
            error "could not scan binary string index $i"
        }
        set b [_format_byte $v]
        lappend s $b
    }
    return
}


namespace eval ::midi {
    # The following are private helper methods used in the
    # midi::file, midi::track, and midi::event namespaces.
    namespace export \
      _format_byte \
      _bytes_to_chars \
      _bytes_to_uint32 \
      _bytes_to_uint24 \
      _bytes_to_uint16 \
      _bytes_to_uint8 \
      _bytes_to_int8 \
      _chars_to_bytes \
      _uint32_to_bytes \
      _uint24_to_bytes \
      _uint16_to_bytes \
      _uint8_to_bytes \
      _int8_to_bytes \
      _vlv_to_bytes \
      _read_chars \
      _read_uint32 \
      _read_uint24 \
      _read_uint16 \
      _read_uint8 \
      _read_int8 \
      _read_vlv \
      _write_chars \
      _write_uint32 \
      _write_uint24 \
      _write_uint16 \
      _write_uint8 \
      _write_int8 \
      _write_vlv \
      _get_vlv
}

foreach ns {midi::file midi::track midi::event} {
    # Import util method from midi namespace
    namespace eval $ns {
        namespace import \
            ::midi::_format_byte \
            ::midi::_bytes_to_chars \
            ::midi::_bytes_to_uint32 \
            ::midi::_bytes_to_uint24 \
            ::midi::_bytes_to_uint16 \
            ::midi::_bytes_to_uint8 \
            ::midi::_bytes_to_int8 \
            ::midi::_chars_to_bytes \
            ::midi::_uint32_to_bytes \
            ::midi::_uint24_to_bytes \
            ::midi::_uint16_to_bytes \
            ::midi::_uint8_to_bytes \
            ::midi::_int8_to_bytes \
            ::midi::_vlv_to_bytes \
            ::midi::_read_chars \
            ::midi::_read_uint32 \
            ::midi::_read_uint24 \
            ::midi::_read_uint16 \
            ::midi::_read_uint8 \
            ::midi::_read_int8 \
            ::midi::_read_vlv \
            ::midi::_write_chars \
            ::midi::_write_uint32 \
            ::midi::_write_uint24 \
            ::midi::_write_uint16 \
            ::midi::_write_uint8 \
            ::midi::_write_int8 \
            ::midi::_write_vlv \
            ::midi::_get_vlv
    }
}

