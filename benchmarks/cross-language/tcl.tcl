if {[llength $argv] > 0 && [lindex $argv 0] eq "--version"} {
    puts "Tcl [info patchlevel]"
    exit 0
}

# DIVREC needs 500 nested calls. Leave room for Tcl's own evaluator
# frames on implementations whose default recursion limit is lower.
catch {interp recursionlimit {} 5000}

set takInput [list 18 12 6]
set derivInput [list \
    + \
    [list * 3 x x] \
    [list * a x x] \
    [list * b x] \
    5]
set derivExpected [list \
    + \
    [list * [list * 3 x x] \
        [list + [list / 0 3] [list / 1 x] [list / 1 x]]] \
    [list * [list * a x x] \
        [list + [list / 0 a] [list / 1 x] [list / 1 x]]] \
    [list * [list * b x] \
        [list + [list / 0 b] [list / 1 x]]] \
    0]

proc makeNilList {count} {
    set result [list]
    while {$count > 0} {
        set result [list [list] $result]
        incr count -1
    }
    return $result
}

set dividend [makeNilList 1000]

proc tak {x y z} {
    if {$y >= $x} {
        return $z
    }
    set a [tak [expr {$x - 1}] $y $z]
    set b [tak [expr {$y - 1}] $z $x]
    set c [tak [expr {$z - 1}] $x $y]
    return [tak $a $b $c]
}

proc deriv {value} {
    if {[llength $value] < 2} {
        return [expr {$value eq "x" ? 1 : 0}]
    }

    set op [lindex $value 0]
    set arguments [lrange $value 1 end]
    switch -- $op {
        "+" -
        "-" {
            set result [list $op]
            foreach argument $arguments {
                lappend result [deriv $argument]
            }
            return $result
        }
        "*" {
            set sum [list +]
            foreach argument $arguments {
                lappend sum [list / [deriv $argument] $argument]
            }
            return [list * $value $sum]
        }
        "/" {
            set numerator [lindex $value 1]
            set denominator [lindex $value 2]
            return [list \
                - \
                [list / [deriv $numerator] $denominator] \
                [list / $numerator \
                    [list * $denominator $denominator \
                        [deriv $denominator]]]]
        }
        default {
            error "no derivation method available"
        }
    }
}

proc diviter {value} {
    set result [list]
    while {[llength $value] != 0} {
        set result [list [lindex $value 0] $result]
        set value [lindex [lindex $value 1] 1]
    }
    return $result
}

proc divrec {value} {
    if {[llength $value] == 0} {
        return [list]
    }
    set next [lindex [lindex $value 1] 1]
    return [list [lindex $value 0] [divrec $next]]
}

proc pairLength {value} {
    set result 0
    while {[llength $value] != 0} {
        incr result
        set value [lindex $value 1]
    }
    return $result
}

proc runOnce {name} {
    global takInput derivInput dividend
    switch -- $name {
        "tak" {
            return [tak \
                [lindex $takInput 0] \
                [lindex $takInput 1] \
                [lindex $takInput 2]]
        }
        "deriv" {
            return [deriv $derivInput]
        }
        "diviter" {
            return [diviter $dividend]
        }
        "divrec" {
            return [divrec $dividend]
        }
        default {
            error "unknown benchmark: $name"
        }
    }
}

proc runMany {name count} {
    set result [list]
    for {set index 0} {$index < $count} {incr index} {
        set result [runOnce $name]
    }
    return $result
}

set name [lindex $argv 0]
set iterations [lindex $argv 1]
set warmup [expr {[llength $argv] > 2 ? [lindex $argv 2] : 0}]
if {$iterations < 1} {
    error "iterations must be positive"
}

runMany $name $warmup
set started [clock microseconds]
set result [runMany $name $iterations]
set elapsedNs [expr {([clock microseconds] - $started) * 1000}]

if {$name eq "tak"} {
    set correct [expr {$result == 7}]
} elseif {$name eq "deriv"} {
    set correct [expr {$result eq $derivExpected}]
} else {
    set correct [expr {[pairLength $result] == 500}]
}
if {!$correct} {
    error "wrong result for $name"
}

puts [format \
    {{"benchmark":"%s","iterations":%d,"elapsed_ns":%d,"ns_per_iteration":%d}} \
    $name \
    $iterations \
    $elapsedNs \
    [expr {$elapsedNs / $iterations}]]
