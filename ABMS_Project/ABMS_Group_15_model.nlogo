globals [
  selected-car   ; the currently selected car
  lanes          ; a list of the y coordinates of different lanes

  jam-distance
  acceleration-exponent

  crashcount
  no-alc-crashcount
  low-alc-crashcount
  mod-alc-crashcount
  hig-alc-crashcount

  turtlebreedset
  turtlecount
  lane-changes
  left-lane-changes
  right-lane-changes
  mid-lane-changes
  left-crashes
  right-crashes
  mid-crashes
  density
  density_util
  measurestart
  mindensity
  maxdensity

  crash-deceleration
  min-politeness
  max-politeness
  averagepoliteness
  carpoliteness
  truckpoliteness
  buspoliteness
  bikepoliteness

  ;error intro
  error-correlation-time
]

breed [cars car]
breed [trucks truck]
breed [bikes bike]
breed [buses bus]

turtles-own [
  speed                        ; (float) the current speed of the vehicle
  preferred-speed              ; (float) the maximum speed of the vehicle (different for all drivers)
  safe-time-headway            ; (float) the time required to react to an event
  target-lane                  ; (int)   the driver's desired lane
  max-acceleration             ; (float) the vehicle's maximum acceleration
  comfortable-deceleration     ; (float) the driver's comfortable deceleration
  max-deceleration             ; (float) the vehicle's maximum deceleration
  crashed?                     ; (bool)  if the driver has crashed the vehicle
  crash-tick                   ; (int)   the time of crash of vehicle
  lead-vehicle
  politeness-factor
  curr-acc
  leading-dist
  comfortable-dist
  category
  age
  height
  weight
  tbw-value
  bac-value

  ;error intro
  wv
  ws
  vdiff

  samples
  lambda

]

to decide-vehicles ;Related to setups
  foreach n-values 4 [i -> i + 1][i ->
    if (substring world-setup 0 2) = (word "S" i)[
      ifelse i > 1 [
        set-indian-speeds
        ifelse i = 3[set-us-acceleration][set-indian-acceleration]
        if i = 4 [
          set cars-preferred-speed 0.625
        ]
      ][
        set-us-speeds
        set-us-acceleration
      ]
      set-deceleration
      ifelse i = 1 [ set turtlebreedset (list "car" "truck") ][
        ifelse i = 2[ set turtlebreedset (list "car" "truck" "bike" "bus") ][ set turtlebreedset (list "car" "truck") ]
      ]
    ]
  ]
end

to setprobs   ; set probabilities based on increased-variety variable
  foreach n-values 4 [i -> i + 1][i ->
    if (substring world-setup 0 2) = (word "S" i)[
      ifelse i mod 2 != 1 [
        set car-probability 0.385
        set truck-probability 0.275
        set bike-probability 0.245
      ][
        set car-probability 0.756
        set truck-probability 0.244
        set bike-probability 0
      ]
    ]
  ]
end

to set-deceleration
  set cars-comfortable-deceleration 0.00518
  set trucks-comfortable-deceleration 0.00104
  set bikes-comfortable-deceleration 0.00116
  set buses-comfortable-deceleration 0.00104
  set cars-max-deceleration 0.00794
  set trucks-max-deceleration 0.00176
  set bikes-max-deceleration 0.00118
  set buses-max-deceleration 0.00176
end

to set-indian-acceleration
  set cars-max-acceleration 0.00574
  set trucks-max-acceleration 0.002
  set bikes-max-acceleration 0.00392
  set buses-max-acceleration 0.002
end

to set-us-acceleration
  set cars-max-acceleration 0.001939
  set trucks-max-acceleration 0.001195
end

to set-indian-speeds
  set cars-preferred-speed 0.416
  set trucks-preferred-speed 0.305
  set bikes-preferred-speed 0.389
  set buses-preferred-speed 0.361
end

to set-us-speeds
  set cars-preferred-speed 0.625
  set trucks-preferred-speed 0.535
  set bikes-preferred-speed 0
  set buses-preferred-speed 0
end

to spawn [ breedname ycord] ; Common spawn function for all vehicle types
  let future-lead-vehicle min-one-of ( turtles with [ ycor = ycord ] ) [ xcor ]
  let breedshape (word breedname "-top")
  create-turtles (1)[
    run (word "set breed " breedname)
    set shape breedshape
    set color vehicle-color
    move-to patch ((- world-width + 1) / 2) ycord
    set target-lane ycor
    set heading 90
    set preferred-speed runresult(word breedname "-preferred-speed")
    set max-acceleration runresult(word breedname "-max-acceleration");precision (0.002 + random-float 0.00375) 6
    set comfortable-deceleration runresult(word breedname "-comfortable-deceleration");precision (0.00104 + random-float 0.00414) 6
    set max-deceleration runresult(word breedname "-max-deceleration")
    set age  18 + random 42
    set height 152 + random 38
    set weight 40 + random 50
    set tbw-value (2.447 - (0.09516 * age) + (0.1074 * height) + (0.3362 * weight))
    let duration-drinking (0.3 + random 1.2)
    ifelse random-float 1 > 0.6 [
      set bac-value ((alcohol-dose - (10 * 0.015 * (duration-drinking + 0.5) * (tbw-value / 0.8))) * (0.08 / tbw-value))
    ] [ set bac-value 0 ]
    if (bac-value < 0.015) [ set bac-value 0 ]
    if (bac-value = 0) [
      set color white
      set category 0
    ]
    if (bac-value > 0.015) and (bac-value < 0.06) [
      set color yellow
      set category 1
      set max-acceleration max-acceleration + 0.013
      set max-deceleration max-deceleration - 0.0006
      set preferred-speed preferred-speed + 1
    ]
    if (bac-value > 0.06) and (bac-value < 0.08) [
      set color blue
      set category 2
      set max-acceleration max-acceleration + 0.026
      set max-deceleration max-deceleration - 0.0011
      set preferred-speed preferred-speed + 1.025
    ]
    if (bac-value > 0.08) [
      set color red
      set category 3
      set max-acceleration max-acceleration + 0.027
      set max-deceleration max-deceleration - 0.0013
      set preferred-speed preferred-speed + 1.147
    ]
    set crashed? false
    set jam-distance 1.4
    set safe-time-headway 16
    set speed ifelse-value (future-lead-vehicle != nobody)[ precision(spawn-speed future-lead-vehicle) 6 ][ runresult(word breedname "-preferred-speed") ];precision (0.205 + random-float 0.21) 6
  ]
end

to spawn-vehicles
  foreach lanes[ lane ->       ; randomly select the lanes and iterate
    if not any? turtles with [ xcor <= ((- world-width + 5) / 2) and ycor = lane ][ ;make sure there are no turtles in the first column of the world.
      let spawn? random 2
      let new-vehicle random-float 1                     ; number that decides the type of vehicle
      if spawn? = 1[
        ifelse new-vehicle < car-probability [ spawn "cars" lane ]
        [
          ifelse new-vehicle < car-probability + truck-probability [ spawn "trucks" lane ]
          [
            ifelse new-vehicle < car-probability + truck-probability + bike-probability [ spawn "bikes" lane][spawn "buses" lane]
          ]
        ]
      ]
    ]
  ]
end


to setup
  clear-all
  set acceleration-exponent 4
  decide-vehicles
  setprobs
  draw-road
  spawn-vehicles
  set error-correlation-time 200
  set max-politeness 1
  set density_util false
  reset-ticks
end

to-report spawn-speed [ leading-vehicle ]
  let constant 2 * sqrt( max-acceleration * comfortable-deceleration )
  ;show "b"
  ;show [ speed ] of leading-vehicle - ( constant * safe-time-headway )
  ;show "c"
  ;show constant * ( jam-distance - [distance-from-left] of leading-vehicle )
  report (( [ speed ] of leading-vehicle ) - ( constant * safe-time-headway ) + ( sqrt( (( ( constant * safe-time-headway ) - [ speed ] of leading-vehicle ) ^ 2) - (4 * constant * ( jam-distance - (precision (([ xcor ] of leading-vehicle - xcor)) 6) )) ) ) ) / 2
end

to-report distance-from-left
  report xcor - ((- world-width + 1) / 2)
end

to draw-road
  ask patches [
    ; the road is surrounded by green grass of varying shades
    set pcolor green - random-float 0.5
  ]
  set lanes n-values number-of-lanes [ n -> number-of-lanes - (n * 2) - 1 ]
  ask patches with [ abs pycor <= number-of-lanes ] [
    ; the road itself is varying shades of grey
    set pcolor grey - 2.5 + random-float 0.25
  ]
  draw-road-lines
end

to draw-road-lines
  let y (last lanes) - 1 ; start below the "lowest" lane
  while [ y <= first lanes + 1 ] [
    if not member? y lanes [
      ; draw lines on road patches that are not part of a lane
      ifelse abs y = number-of-lanes
        [ draw-line y yellow 0 ]  ; yellow for the sides of the road
        [ draw-line y white 0.5 ] ; dashed white between lanes
    ]
    set y y + 1 ; move up one patch
  ]
end

to draw-line [ y line-color gap ]
  ; We use a temporary turtle to draw the line:
  ; - with a gap of zero, we get a continuous line;
  ; - with a gap greater than zero, we get a dasshed line.
  create-turtles 1 [
    setxy (min-pxcor - 0.5) y
    hide-turtle
    set color line-color
    set heading 90
    repeat world-width [
      pen-up
      forward gap
      pen-down
      forward (1 - gap)
    ]
    die
  ]
end

;IDM

to-report comfortable-gap [ vehicle-ahead ]
  ifelse (vehicle-ahead = nobody)[ report precision ( jam-distance + ( safe-time-headway * speed )) 6]
  [
    set wv exp(- 1 / error-correlation-time) * wv + ((random-normal 0 1) * sqrt(2 * 1 / error-correlation-time))
    set vdiff ( speed - [ speed ] of vehicle-ahead ) + ( ([ xcor ] of vehicle-ahead - xcor) * estimation-error * wv)
    report precision ( jam-distance + ( safe-time-headway * speed ) + (( speed * vdiff /( 2 * sqrt( max-acceleration * comfortable-deceleration ))))) 6
  ]
end

to-report acceleration
  let vehicle-ahead min-one-of ( other turtles in-cone world-width 180 with [ y-distance <= 1 and x-distance > 0 ] ) [ distance myself ]
  set lead-vehicle vehicle-ahead
  set comfortable-dist comfortable-gap vehicle-ahead
  ifelse (vehicle-ahead = nobody)[
    ;show max-acceleration
    ;show preferred-speed
    ;show speed
    set curr-acc precision ( max-acceleration * ( 1 - (( speed / preferred-speed ) ^ acceleration-exponent ))) 6
    report precision ( max-acceleration * ( 1 - (( speed / preferred-speed ) ^ acceleration-exponent ))) 6
  ][
    set leading-dist leading-gap vehicle-ahead
    set curr-acc precision ( max-acceleration * ( 1 - (( speed / preferred-speed ) ^ acceleration-exponent ) - (( comfortable-gap vehicle-ahead / leading-gap vehicle-ahead ) ^ 2 ))) 6
    if ( curr-acc < 0 ) and ( abs(curr-acc) > max-deceleration )[ set curr-acc -1 * max-deceleration ]
    report curr-acc
  ]
end

to-report leading-gap [ vehicle-ahead ]
  ;show who
  ;show vehicle-ahead
  ;set ws exp(- 1 / error-correlation-time) * ws + ((random-normal 0 1) * sqrt(2 * 1 / error-correlation-time))
  ;set ws 0
  report precision (([ xcor ] of vehicle-ahead - xcor) * exp (bac-value * 3)) 6
end

;MOBIL

to-report future-acceleration [ leading-vehicle ]
  ifelse (leading-vehicle = nobody)[ report precision ( max-acceleration * ( 1 - (( speed / preferred-speed ) ^ acceleration-exponent ))) 6 ]
  [
    report precision ( max-acceleration * ( 1 - (( speed / preferred-speed ) ^ acceleration-exponent ) - (( comfortable-gap leading-vehicle / leading-gap leading-vehicle) ^ 2 ))) 6
  ]
end

to-report lane-change-utility [ lane ] ;MOBIL Function
  set heading -90
  let future-following-vehicle min-one-of ( other turtles in-cone world-width 150 with [ abs( ycor - lane ) <= 0.75 ] ) [ distance myself ]
  let current-following-vehicle min-one-of ( other turtles in-cone world-width 150 with [ y-distance <= 0.75 ] ) [ distance myself ]
  set heading 90
  let future-leading-vehicle min-one-of ( other turtles in-cone world-width 150 with [ abs( ycor - lane ) <= 0.75 ] ) [ distance myself ]
  let current-leading-vehicle min-one-of ( other turtles in-cone world-width 150 with [ y-distance <= 0.75 ] ) [ distance myself ]
  let utility 0
  set utility utility + future-acceleration future-leading-vehicle - acceleration
  if (future-following-vehicle != nobody) [ set utility utility + politeness-factor * ( [ future-acceleration myself ] of future-following-vehicle - [ acceleration ] of future-following-vehicle ) ]
  if (current-following-vehicle != nobody) [ set utility utility + politeness-factor * ( [ future-acceleration current-leading-vehicle ] of current-following-vehicle - [ acceleration ] of current-following-vehicle ) ]
  report utility
end

to check-lane-change
  let other-lanes remove ycor lanes
  let currbest lane-change-threshold
  if not empty? other-lanes [
    let min-dist min map [ y -> abs (y - ycor) ] other-lanes
    let closest-lanes filter [ y -> abs (y - ycor) = min-dist ] other-lanes
    foreach closest-lanes[ [ lane ] ->
      set heading -90
      let future-follower min-one-of ( other turtles in-cone world-width 150 with [ abs( ycor - lane ) <= 0.75 ] ) [ distance myself ]
      if (future-follower = nobody) or (abs([ future-acceleration myself ] of future-follower) < [ abs(max-deceleration) ] of future-follower)[
        if not any? other turtles with [ ycor = lane and abs(xcor - [xcor] of myself) < 1 ] and lane-change-utility lane > currbest[
          set currbest lane-change-utility lane
          set target-lane lane
        ]
      ]
    ]
  ]
end

to go
  if turtlecount >= 10000 [stop]
  spawn-vehicles
  ask turtles with [ crashed? and ticks - crash-tick > 3][ die ]   ; kill turtles that are alive for more than 3 ticks after crashing
  foreach sort-on [(- xcor)] turtles [ [vehicle] ->                ; sort turtles based on xcor in decending order.
    ask vehicle [
      if not crashed?[         ; do only if turtle has not crashed.
        crash-check
;        ifelse bac-value = 0 [ calculate-speeds-nor ]
;        [ if color = yellow [ calculate-speeds-low ]
;          if color = blue [ calculate-speeds-mod ]
;          if color = red [ calculate-speeds-hig ]
;        ]
        calculate-speeds-nor

        check-lane-change
        if ycor != target-lane [ move-to-target-lane ]
        update-politeness
      ]
      set heading 90
      fd speed
      set xcor precision xcor 6
    ]
  ]
  ask turtles with [xcor > ((world-width - 2) / 2)][die]  ;kill the turtles that reached the end of the world
  if any? turtles and ticks >= 1[
    set turtlecount last sort [who] of turtles - 2
    set averagepoliteness precision (((averagepoliteness * ( ticks - 1 )) + mean [ politeness-factor ] of turtles) / ticks) 6
    if averagepoliteness > 1 [set averagepoliteness 1]
    if any? cars [ set carpoliteness precision (((carpoliteness * ( ticks - 1 )) + mean [ politeness-factor ] of cars) / ticks) 6 ]
    if any? trucks [ set truckpoliteness precision (((truckpoliteness * ( ticks - 1 )) + mean [ politeness-factor ] of trucks) / ticks) 6 ]
    if any? buses [ set buspoliteness precision (((buspoliteness * ( ticks - 1 )) + mean [ politeness-factor ] of buses) / ticks) 6 ]
    if any? bikes [ set bikepoliteness precision (((bikepoliteness * ( ticks - 1 )) + mean [ politeness-factor ] of bikes) / ticks) 6 ]
  ]
  ifelse density_util = true [
    set density precision ((density * (ticks - measurestart - 1) + count(turtles)) / (ticks - measurestart)) 3
    if measurestart = 0[
      set measurestart ticks
      set mindensity count(turtles)
    ]
    if count(turtles) < mindensity[set mindensity count(turtles)]
    if count(turtles) > maxdensity[set maxdensity count(turtles)]
  ][if any? turtles with [ xcor > (world-width / 2) - 3 ][set density_util true]]
  tick
end

to update-politeness
  ifelse (samples < 10)[
    set lambda precision (((lambda * samples) + (speed / preferred-speed)) / (samples + 1) ) 3
    set samples samples + 1
  ][set lambda precision (weighting-factor * (speed / preferred-speed) + ((1 - weighting-factor) * lambda)) 3]
  set politeness-factor (lambda * max-politeness) + ((1 - lambda) * min-politeness)
end


to calculate-speeds-nor
  set speed max(list 0 precision (speed + acceleration) 6 )
end

to move-to-target-lane ; turtle procedure
  set heading ifelse-value target-lane = ycor [ 90 ] [
    ifelse-value target-lane < ycor [ 180 ][ 0 ]
  ]
  let blocking-vehicles other turtles in-cone (1 + abs (ycor - target-lane)) 180 with [ x-distance <= 1.25]
  let blocking-vehicle min-one-of blocking-vehicles [ distance myself ]
  if blocking-vehicle = nobody [
    forward 2
    set ycor precision ycor 2 ; to avoid floating point errors
  ]
  ifelse xcor < -60 + (world-width / 3)[set left-lane-changes left-lane-changes + 1][
    ifelse xcor < -60 + (2 * world-width / 3)[set mid-lane-changes mid-lane-changes + 1][set right-lane-changes right-lane-changes + 1];!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ]
  set lane-changes lane-changes + 1
  set heading 90
end

to-report x-distance
  report distancexy [ xcor ] of myself ycor
end

to-report y-distance
  report distancexy xcor [ ycor ] of myself
end

to select-car
  ; allow the user to select a different car by clicking on it with the mouse
  if mouse-down? [
    let mx mouse-xcor
    let my mouse-ycor
    if any? turtles-on patch mx my [
      ask selected-car [ set color vehicle-color ]
      set selected-car one-of turtles-on patch mx my
      ask selected-car [ set color red ]
      display
    ]
  ]
end

to crash-check
  if any? other turtles in-cone 1 180 with [ y-distance < 1 and x-distance < 1 ][
    let color1 category
    crash
    ask other turtles in-cone 1 180 with [ y-distance < 1 and x-distance < 1 ][
      let color2 category
      let cate max list color1 color2
      (ifelse cate = 0 [ set no-alc-crashcount no-alc-crashcount + 1 ]
        cate = 1 [ set low-alc-crashcount low-alc-crashcount + 1 ]
        cate = 2 [ set mod-alc-crashcount mod-alc-crashcount + 1 ]
        [ set hig-alc-crashcount hig-alc-crashcount + 1 ])
      crash
    ]
    ifelse xcor < -60 + (world-width / 3)[set left-crashes left-crashes + 1][
      ifelse xcor < -60 + (2 * world-width / 3)[set mid-crashes mid-crashes + 1][set right-crashes right-crashes + 1];!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ]
    set crashcount crashcount + 1
  ]
end

to crash ; turtle procedure
  ;if not crashed? [ inccount ]
  set color yellow
  set shape "fire"
  set crashed? true
  set crash-tick ticks
end

to-report vehicle-color
  ; give all cars a blueish color, but still make them distinguishable
  report one-of [ blue cyan sky ] + 1.5 + random-float 1.0
end

to-report number-of-lanes
  report no-of-lanes
end


; Copyright 1998 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
225
10
1443
349
-1
-1
10.0
1
10
1
1
1
0
0
0
1
-60
60
-16
16
1
1
1
ticks
30.0

BUTTON
10
10
75
45
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
150
10
215
45
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
80
10
145
45
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
130
50
220
95
mean speed
mean [speed] of turtles
2
1
11

MONITOR
10
50
125
95
Lane Changes
lane-changes
2
1
11

SLIDER
10
360
220
393
cars-max-acceleration
cars-max-acceleration
0
1
0.001939
0.001
1
NIL
HORIZONTAL

BUTTON
10
100
220
133
Reset Vehicle Characteristics
decide-vehicles\nsetprobs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
10
135
220
180
world-setup
world-setup
"S1- US speeds, Decreased variance" "S2 - Indian speeds, Increased variance" "S3 - Indian speeds, Decreased variance" "S4 - US Car Speed, Increased variance"
0

SLIDER
225
395
435
428
trucks-comfortable-deceleration
trucks-comfortable-deceleration
0
1
0.00104
0.001
1
NIL
HORIZONTAL

SLIDER
225
360
435
393
cars-comfortable-deceleration
cars-comfortable-deceleration
0
1
0.00518
0.001
1
NIL
HORIZONTAL

SLIDER
10
395
220
428
trucks-max-acceleration
trucks-max-acceleration
0
1
0.001195
0.001
1
NIL
HORIZONTAL

SLIDER
10
430
220
463
bikes-max-acceleration
bikes-max-acceleration
0
1
0.00392
0.001
1
NIL
HORIZONTAL

SLIDER
10
465
220
498
buses-max-acceleration
buses-max-acceleration
0
1
0.002
0.001
1
NIL
HORIZONTAL

SLIDER
225
430
435
463
bikes-comfortable-deceleration
bikes-comfortable-deceleration
0
1
0.00116
0.001
1
NIL
HORIZONTAL

SLIDER
225
465
435
498
buses-comfortable-deceleration
buses-comfortable-deceleration
0
1
0.00104
0.001
1
NIL
HORIZONTAL

SLIDER
655
360
865
393
cars-preferred-speed
cars-preferred-speed
0
1
0.625
0.001
1
NIL
HORIZONTAL

SLIDER
655
395
865
428
trucks-preferred-speed
trucks-preferred-speed
0
1
0.535
0.001
1
NIL
HORIZONTAL

SLIDER
655
430
865
463
bikes-preferred-speed
bikes-preferred-speed
0
1
0.0
0.001
1
NIL
HORIZONTAL

SLIDER
655
465
865
498
buses-preferred-speed
buses-preferred-speed
0
1
0.0
0.001
1
NIL
HORIZONTAL

SLIDER
865
360
1037
393
car-probability
car-probability
0
1
0.756
0.01
1
NIL
HORIZONTAL

SLIDER
865
395
1037
428
truck-probability
truck-probability
0
1
0.244
0.01
1
NIL
HORIZONTAL

SLIDER
865
430
1037
463
bike-probability
bike-probability
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
440
360
650
393
cars-max-deceleration
cars-max-deceleration
0
1
0.00794
0.001
1
NIL
HORIZONTAL

SLIDER
440
395
650
428
trucks-max-deceleration
trucks-max-deceleration
0
1
0.00176
0.001
1
NIL
HORIZONTAL

SLIDER
440
430
650
463
bikes-max-deceleration
bikes-max-deceleration
0
1
0.00118
0.001
1
NIL
HORIZONTAL

SLIDER
440
465
650
498
buses-max-deceleration
buses-max-deceleration
0
1
0.00176
0.001
1
NIL
HORIZONTAL

SLIDER
10
215
220
248
estimation-error
estimation-error
0
0.05
0.001
0.001
1
NIL
HORIZONTAL

SLIDER
10
285
220
318
weighting-factor
weighting-factor
0
1
0.5
0.001
1
NIL
HORIZONTAL

SLIDER
10
250
220
283
distance-error
distance-error
0
1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
10
180
220
213
lane-change-threshold
lane-change-threshold
0.001
0.01
0.006
0.0005
1
NIL
HORIZONTAL

SLIDER
1045
360
1217
393
alcohol-dose
alcohol-dose
10
90
40.0
0.01
1
NIL
HORIZONTAL

SLIDER
1045
395
1217
428
no-of-lanes
no-of-lanes
1
5
3.0
1
1
NIL
HORIZONTAL

PLOT
1225
355
1425
505
accidents-plot
time
no-of-accidents
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Total Crashes" 1.0 0 -16777216 true "" "plot crashcount"
"No Alcohol" 1.0 0 -13840069 true "" "plot no-alc-crashcount"
"Low Alcohol" 1.0 0 -1184463 true "" "plot low-alc-crashcount"
"Moderate Alcohol" 1.0 0 -13345367 true "" "plot mod-alc-crashcount"
"High Alcohol" 1.0 0 -2674135 true "" "plot hig-alc-crashcount"

MONITOR
1095
465
1177
510
Total Crashs
crashcount
2
1
11

MONITOR
865
465
927
510
no-alc
no-alc-crashcount
17
1
11

MONITOR
1040
465
1095
510
high
hig-alc-crashcount
17
1
11

MONITOR
925
465
980
510
low
low-alc-crashcount
17
1
11

MONITOR
980
465
1040
510
moderate
mod-alc-crashcount
17
1
11

@#$#@#$#@
## WHAT IS IT?

This model is a more sophisticated version of the "Traffic Basic" model.  Much like the simpler model, this model demonstrates how traffic jams can form. In this version, drivers have a new option; they can react by changing lanes, although this often does little to solve their problem. This model also studies the effect of alcohol on drivers perception of vision and how it could cause road accidents.

As in the Traffic Basic model, traffic may slow down and jam without any centralized cause.

## KEY TERMS


The Blood Alcohol Level(BAL) of each driver was calculated according to  Equation  can be used to calculate the BAL for the expected alcohol dose level:

![alt text](file:Bac.jpg)

where BAL is the target blood-alcohol level, TBW is the total body water amount, MR is the metabolic rate (generally 0.015 g/100 mL/h), DDP is the duration of the drinking period, and TPB is the time to peak BAL (generally 0.5 h). Generally, TBW for men is as follows:
![alt text](file:tbw.jpg)

We categorised the drivers into 4 types.
1) Normal Drivers ( BAL = 0)
2) Low alcohol consumed(0.015<BAL<0.06)
3) Moderately drunk(0.06<BAL<0.08)
4) Heavy drinkers (BAL>0.08)

BAL levels affect the percieved distance, accelerating and breaking behaviours of drivers.



## HOW TO USE IT

Click on the SETUP button to set up the cars. Click on GO to start the cars moving. The GO ONCE button drives the cars for just one tick of the clock.

The SELECTED CAR SPEED monitor displays the speed of the selected car. The MEAN-SPEED monitor displays the average speed of all the cars.

You can change the number of lanes using the slider provided

You can also set the alcohol dose using the slider. 


## THINGS TO NOTICE

Traffic jams can start from small "seeds." Cars start with random positions. If some cars are clustered together, they will move slowly, causing cars behind them to slow down, and a traffic jam forms.

Even though all of the cars are moving forward, the traffic jams tend to move backwards. This behavior is common in wave phenomena: the behavior of the group is often very different from the behavior of the individuals that make up the group.

Just as each car has a current speed, each driver has a current patience. Each time the driver has to hit the brakes to avoid hitting the car in front of them, they loose a little patience. When a driver's patience expires, the driver tries to change lane. The driver's patience gets reset to the maximum patience.

When the number of cars in the model is high, drivers lose their patience quickly and start weaving in and out of lanes. This phenomenon is called "snaking" and is common in congested highways. And if the number of cars is high enough, almost every car ends up trying to change lanes and the traffic slows to a crawl, making the situation even worse, with cars getting momentarily stuck between lanes because they are unable to change. Does that look like a real life situation to you?

Watch the MEAN-SPEED monitor, which computes the average speed of the cars. What happens to the speed over time? What is the relation between the speed of the cars and the presence (or absence) of traffic jams?

The Accident plot plot displays five quantities over time:

- the accidents caused by a normal driver - GREEN
- the accidents caused by a low alcohol consumed driver - YELLOW
- the accidents caused by a moderately drunk driver - BLUE
- the accidents caused by a heavy drinker - RED
- total count of accidents - BLACK


The grass patches on each side of the road are all a slightly different shade of green. The road patches, to a lesser extent, are different shades of grey. This is not just about making the model look nice: it also helps create an impression of movement when using the FOLLOW SELECTED CAR button.

## THINGS TO TRY

What could you change to minimize the chances of traffic jams forming, besides just the number of cars? What is the relationship between number of cars, number of lanes, and (in this case) the length of each lane?

Explore changes to the sliders SLOW-DOWN and SPEED-UP. How do these affect the flow of traffic? Can you set them so as to create maximal snaking?

Change the code so that all cars always start on the same lane. Does the proportion of cars on each lane eventually balance out? How long does it take?

Try using the `"default"` turtle shape instead of the car shape, either by changing the code or by typing `ask turtles [ set shape "default" ]` in the command center after clicking SETUP. This will allow you to quickly spot the cars trying to change lanes. What happens to them when there is a lot of traffic?

## REFERENCES


1. Dose-Related Effects of Alcohol on Cognitive Functioning
   Link: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0050977#s3

2. Study of the Effects of Alcohol on Drivers and Driving Performance on Straight Road
   Link: https://doi.org/10.1155/2014/607652

3. Effect of alcohol use on accelerating and braking behaviors of drivers
   Link: https://doi.org/10.1080/15389588.2019.1587167

4. Alcohol-impaired driving in rural and urban road environments: Effect on speeding 		   behaviour and crash probabilities.
   Link: https://pubmed.ncbi.nlm.nih.gov/32234551/



## NETLOGO FEATURES

Note the use of `mouse-down?` and `mouse-xcor`/`mouse-ycor` to enable selecting a car for special attention.

Each turtle has a shape, unlike in some other models. NetLogo uses `set shape` to alter the shapes of turtles. You can, using the shapes editor in the Tools menu, create your own turtle shapes or modify existing ones. Then you can modify the code to use your own shapes.

## RELATED MODELS

- "Traffic Basic": a simple model of the movement of cars on a highway.

- "Traffic Basic Utility": a version of "Traffic Basic" including a utility function for the cars.

- "Traffic Basic Adaptive": a version of "Traffic Basic" where cars adapt their acceleration to try and maintain a smooth flow of traffic.

- "Traffic Basic Adaptive Individuals": a version of "Traffic Basic Adaptive" where each car adapts individually, instead of all cars adapting in unison.

- "Traffic Intersection": a model of cars traveling through a single intersection.

- "Traffic Grid": a model of traffic moving in a city grid, with stoplights at the intersections.

- "Traffic Grid Goal": a version of "Traffic Grid" where the cars have goals, namely to drive to and from work.

- "Gridlock HubNet": a version of "Traffic Grid" where students control traffic lights in real-time.

- "Gridlock Alternate HubNet": a version of "Gridlock HubNet" where students can enter NetLogo code to plot custom metrics.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. & Payette, N. (1998).  NetLogo Traffic 2 Lanes model.  http://ccl.northwestern.edu/netlogo/models/Traffic2Lanes.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1998 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2001.

<!-- 1998 2001 Cite: Wilensky, U. & Payette, N. -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

bikes-top
true
0
Polygon -7500403 true true 135 180 120 135 120 105 135 90 165 90 180 105 180 135 165 180 135 180
Polygon -16777216 true false 165 285 135 285 120 195 135 165 165 165 180 195 165 285
Rectangle -16777216 true false 142 25 158 63
Polygon -7500403 true true 120 90 180 90 165 60 135 60 120 90
Polygon -7500403 true true 136 91 107 106 106 102 137 82 138 90
Polygon -7500403 true true 163 82 194 103 191 107 159 88 163 83
Polygon -16777216 true false 193 106 214 125 214 113 194 102
Polygon -16777216 true false 107 106 86 125 86 113 106 102

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

buses-top
true
0
Polygon -7500403 true true 150 15 120 15 90 30 90 45 90 225 90 270 105 285 150 285 195 285 210 270 210 225 210 45 210 30 180 15
Polygon -16777216 true false 106 58 194 58 204 44 182 36 153 32 120 36 98 44
Polygon -1 true false 205 29 180 30 182 16
Polygon -1 true false 95 29 120 30 118 16

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

cars-top
true
0
Polygon -7500403 true true 151 8 119 10 98 25 86 48 82 225 90 270 105 289 150 294 195 291 210 270 219 225 214 47 201 24 181 11
Polygon -16777216 true false 210 195 195 210 195 135 210 105
Polygon -16777216 true false 105 255 120 270 180 270 195 255 195 225 105 225
Polygon -16777216 true false 90 195 105 210 105 135 90 105
Polygon -1 true false 205 29 180 30 181 11
Line -7500403 true 210 165 195 165
Line -7500403 true 90 165 105 165
Polygon -16777216 true false 121 135 180 134 204 97 182 89 153 85 120 89 98 97
Line -16777216 false 210 90 195 30
Line -16777216 false 90 90 105 30
Polygon -1 true false 95 29 120 30 119 11

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

rickshaws-top
true
4
Rectangle -16777216 true false 60 210 75 270
Rectangle -16777216 true false 225 210 240 270
Polygon -1184463 true true 240 270 225 90 210 60 180 45 120 45 90 60 75 90 60 270 225 270 225 270
Rectangle -16777216 true false 144 6 157 31
Polygon -7500403 true false 120 45 135 30 165 30 180 45 120 45

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

trucks-top
true
0
Polygon -7500403 true true 150 15 135 15 118 23 109 45 104 75 105 114 150 114
Polygon -7500403 true true 150 15 165 15 182 23 191 45 196 75 195 114 150 114
Rectangle -7500403 true true 90 120 210 285
Rectangle -7500403 true true 135 120 165 120
Rectangle -7500403 true true 135 105 165 135
Rectangle -16777216 true false 120 60 180 75
Polygon -16777216 true false 180 60 164 51 148 51 149 61
Polygon -16777216 true false 120 60 136 51 152 51 151 61
Polygon -1184463 true false 137 16 137 26 120 25
Polygon -1184463 true false 163 16 163 26 180 25
Line -16777216 false 193 136 194 275
Line -16777216 false 107 136 106 275
Line -16777216 false 177 139 177 279
Line -16777216 false 123 139 123 279
Line -16777216 false 158 143 157 279
Line -16777216 false 142 143 143 279

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Accident Check" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="5000"/>
    <metric>crashcount</metric>
    <metric>no-alc-crashcount</metric>
    <metric>low-alc-crashcount</metric>
    <metric>mod-alc-crashcount</metric>
    <metric>hig-alc-crashcount</metric>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;S1- US speeds, Decreased variance&quot;"/>
      <value value="&quot;S2 - Indian speeds, Increased variance&quot;"/>
      <value value="&quot;S3 - Indian speeds, Decreased variance&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="no-of-lanes">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <steppedValueSet variable="alcohol-dose" first="20" step="10" last="80"/>
  </experiment>
  <experiment name="Main" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>left-crashes</metric>
    <metric>mid-crashes</metric>
    <metric>right-crashes</metric>
    <metric>crashcount</metric>
    <metric>left-lane-changes</metric>
    <metric>mid-lane-changes</metric>
    <metric>right-lane-changes</metric>
    <metric>lane-changes</metric>
    <metric>turtlecount</metric>
    <enumeratedValueSet variable="memory">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-error">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="estimation-error" first="0.007" step="0.001" last="0.012"/>
    <enumeratedValueSet variable="min-politeness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-politeness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lane-change-threshold">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;S1- US speeds, Decreased variance&quot;"/>
      <value value="&quot;S2 - Indian speeds, Increased variance&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Basic" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>left-crashes</metric>
    <metric>mid-crashes</metric>
    <metric>right-crashes</metric>
    <metric>crashcount</metric>
    <metric>left-lane-changes</metric>
    <metric>mid-lane-changes</metric>
    <metric>right-lane-changes</metric>
    <metric>lane-changes</metric>
    <metric>averagepoliteness</metric>
    <metric>turtlecount</metric>
    <enumeratedValueSet variable="memory">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-error">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="estimation-error">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-politeness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-politeness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lane-change-threshold">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;S1- US speeds, Decreased variance&quot;"/>
      <value value="&quot;S2 - Indian speeds, Increased variance&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="AIO" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>density</metric>
    <metric>mindensity</metric>
    <metric>maxdensity</metric>
    <metric>left-crashes</metric>
    <metric>mid-crashes</metric>
    <metric>right-crashes</metric>
    <metric>crashcount</metric>
    <metric>left-lane-changes</metric>
    <metric>mid-lane-changes</metric>
    <metric>right-lane-changes</metric>
    <metric>lane-changes</metric>
    <metric>averagepoliteness</metric>
    <metric>carpoliteness</metric>
    <metric>truckpoliteness</metric>
    <metric>buspoliteness</metric>
    <metric>bikepoliteness</metric>
    <metric>turtlecount</metric>
    <enumeratedValueSet variable="memory">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distance-error">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="estimation-error">
      <value value="0.001"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min-politeness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-politeness">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lane-change-threshold">
      <value value="0.002"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-setup">
      <value value="&quot;S1- US speeds, Decreased variance&quot;"/>
      <value value="&quot;S2 - Indian speeds, Increased variance&quot;"/>
      <value value="&quot;S3 - Indian speeds, Decreased variance&quot;"/>
      <value value="&quot;S4 - US Car Speed, Increased variance&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
