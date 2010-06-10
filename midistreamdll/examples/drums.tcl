lappend auto_path ..

package require Tk
package require midistreamdll

set bank 10
set program 1
set s1 35
set s2 38
set s3 54
set s4 76
set s5 36
set s6 39
set s7 55
set s8 77

namespace eval ::tseq {
}

proc ::tseq::open_dev { dev } {
    return [midistreamdll::open $dev]
}

proc ::tseq::close_dev { dev } {
    $dev close
}

proc ::tseq::change_prog { dev channel prog } {
    $dev short [expr {0xbf + $channel}] $prog
}

proc ::tseq::set_volume { dev channel h l } {
    $dev short [expr {$channel+0xaf}] 0x7 $h
    $dev short [expr {$channel+0xaf}] 0x27 $l
}

proc ::tseq::play_note { dev channel note velocity } {
    $dev short [expr {$channel+0x8f}] $note $velocity
}

proc ::tseq::end_note { dev channel note velocity } {
    $dev short [expr {$channel+0x7f}] $note $velocity
}


set dev [::tseq::open_dev 0]
::tseq::set_volume $dev 10 127 127

proc all_chan10 { dev } {
    set r 0
    set c 5
    for {set i 35} {$i < 77 } { incr i } {
	button .b$i -text $i -command [list ::tseq::play_note $dev 10 $i 127]
	grid .b$i -row $r -column $c -sticky ewns
	incr c
	if { ($c % 10) == 0 } {
	    set c 0
	    incr r
	}
    }
}

set f [frame .f -bd 2 -relief raised]
pack $f -fill both -expand true

label $f.lb -text Bassdrum
label $f.ls -text Snaredrum
label $f.lh -text Hihat
label $f.lm -text Metronome
label $f.l5 -text Bassdrum2
label $f.l6 -text Snaredrum2
label $f.l7 -text Hihat2
label $f.l8 -text Metronome2
grid $f.lb -row 1 -column 1
grid $f.ls -row 2 -column 1
grid $f.lh -row 3 -column 1
grid $f.lm -row 4 -column 1
grid $f.l5 -row 5 -column 1
grid $f.l6 -row 6 -column 1
grid $f.l7 -row 7 -column 1
grid $f.l8 -row 8 -column 1

set col 2
for { set i 0 } { $i < 16 } { incr i } {
    set ba($i) 0
    set sa($i) 0
    set ha($i) 0
    set ma($i) 0
    set b5($i) 0
    set b6($i) 0
    set h7($i) 0
    set m8($i) 0
    set cb [checkbutton $f.cb$i -variable ::ba($i)]
    set cs [checkbutton $f.cs$i -variable ::sa($i)]
    set ch [checkbutton $f.ch$i -variable ::ha($i)]
    set cm [checkbutton $f.cm$i -variable ::ma($i)]
    set c5 [checkbutton $f.c5$i -variable ::b5($i)]
    set c6 [checkbutton $f.c6$i -variable ::b6($i)]
    set c7 [checkbutton $f.c7$i -variable ::h7($i)]
    set c8 [checkbutton $f.c8$i -variable ::m8($i)]
    grid $cb -row 1 -column $col
    grid $cs -row 2 -column $col
    grid $ch -row 3 -column $col
    grid $cm -row 4 -column $col
    grid $c5 -row 5 -column $col
    grid $c6 -row 6 -column $col
    grid $c7 -row 7 -column $col
    grid $c8 -row 8 -column $col
    set r [radiobutton $f.r$i -variable ::tseq::pidx -value $i]
    grid $r -row 9 -column $col
    incr col
}

set bf [frame .bf -bd 2 -relief raised]
pack $bf -fill x

button $bf.be -text exit -command [list exit_prog $dev]
button $bf.bp -text play -command [list ::tseq::start_seq $dev]
button $bf.bs -text stop -command ::tseq::stop_seq

set ::tseq::beatspmin 60

label $bf.ls -text "Beats/min"
entry $bf.es -textvariable ::tseq::beatspmin

label $bf.lbn -text "Bank"
entry $bf.ebn -textvariable bank

label $bf.lpg -text "Program"
entry $bf.epg -textvariable program

pack $bf.be $bf.bp $bf.bs $bf.ls $bf.es $bf.lbn $bf.ebn $bf.lpg $bf.epg -side left -fill both -expand true

proc exit_prog { dev } {
    tseq::close_dev $dev
    exit
}

proc ::tseq::start_seq { dev } {

    global bank program

    set ::tseq::playing 1
    set ::tseq::pidx -1
    set ::tseq::delay [expr {int(60.0 / $::tseq::beatspmin / 4.0 * 1000.0)}]
    ::tseq::play_seq $dev
}

proc ::tseq::stop_seq { } {
    set ::tseq::playing 0
}

proc ::tseq::play_seq { dev } {

    global bank program s1 s2 s3 s4 s5 s6 s7 s8

    if { !$::tseq::playing } {
	return
    }

    ::tseq::change_prog $dev $bank $program

    incr ::tseq::pidx
    if { ($::tseq::pidx % 16) == 0 } {
	set ::tseq::pidx 0
    }

    if { $::ba($::tseq::pidx) } {
	::tseq::play_note $dev $bank $s1 127
    }
    if { $::sa($::tseq::pidx) } {
	::tseq::play_note $dev $bank $s2 127
    }
    if { $::ha($::tseq::pidx) } {
	::tseq::play_note $dev $bank $s3 127
    }
    if { $::ma($::tseq::pidx) } {
	::tseq::play_note $dev $bank $s4 127
    }
    if { $::b5($::tseq::pidx) } {
	::tseq::play_note $dev $bank $s5 127
    }
    if { $::b6($::tseq::pidx) } {
	::tseq::play_note $dev $bank $s6 127
    }
    if { $::h7($::tseq::pidx) } {
	::tseq::play_note $dev $bank $s7 127
    }
    if { $::m8($::tseq::pidx) } {
	::tseq::play_note $dev $bank $s8 127
    }
    after $::tseq::delay "::tseq::play_seq $dev"
}





