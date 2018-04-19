﻿//#nowarn"58"
namespace global
[<AutoOpen>]
module Utilities=
    type ObsoleteAttribute=System.ObsoleteAttribute
    module Random=
        let private a=lazy System.Random()
        let get()=a.Force()
        let next()=get().Next()
        let lessThan exclusiveMax=get().Next exclusiveMax
        let pick seq=
            assert(not<|Seq.isEmpty seq)
            lessThan<|Seq.length seq|>Seq.item<|seq
module Geometry=
    type Quantity=float
    ///1 dimensional
    type Distance=Quantity
    type Length=Distance
    type Line=Length
    ///two dimensional
    module Flat=
        type Shape=Rectangle of Length*Length|Circle of Length
        module CoordinateSystem=
            type Coordinate=Distance*Distance
            type Point=Coordinate
            type Line=Point*Point
            type Polyline=Point list
module Grid=
    type Grid=int*int
    type Index=int
    type Position=Index*Index
module Metro=
    //open Geometry
    //open Flat
    //open CoordinateSystem
    open Grid
    type Size=Grid
    type Station=Position
    type Line=Station list
    type Metro=Station list*Line list
    type Game=Size*Metro
    module Grids=
        let create w h=(w,h)
        let enum(w,h)={5..10..w-1}|>Seq.collect(fun x->{5..10..h-1}|>Seq.map(fun y->x,y))
    module Stop=
        let getStation,getNumber=fst,snd
    module Line=
        let passes line station=line|>Seq.map Stop.getStation|>Seq.contains station
        let start stop=[stop]
        let add exsisting stop=stop::exsisting
        let random getStop stations length=
            let station=Random.pick stations
            let remain=List.except[station]stations
            let a=start<|getStop station
            let pick remain=
                if List.isEmpty remain then None else
                    let station=Random.pick remain
                    let remain=List.except[station]remain
                    Some(station,remain)
            let followings=List.unfold pick remain
            followings|>List.take length|>List.map getStop|>List.fold add a
    module Station=
        let random=Grids.enum>>Random.pick
        let randomSeq n size=(fun _->random size)|>Seq.init n
        let create a=a
        let countConnections station lines=lines|>Seq.filter(fun line->Line.passes line station)|>Seq.length
        let getStop lines station=station,countConnections station lines
    module Network=
        let random stations=
            let line existingLines=
                let line=Line.random(Station.getStop existingLines)stations 3
                Some(line,line::existingLines)
            Seq.unfold line []
    let sample=
        let grids=Grids.create 99 99
        let stations=grids|>Station.randomSeq 5|>Seq.toList
        let lines=Network.random stations|>Seq.take 3|>Seq.toList
        //let lines=[Line.random stations 4;Line.random stations 3;Line.random stations 2]
        grids,(stations,lines)
module Presentation=
    //type[<Measure>]m and[<Measure>]dm and[<Measure>]cm
    //let cmPerM=1000<cm/m>
    module Line=
        let calculate stops=
            let calculatePointsForStop exsistings=
                let current,previous=List.head exsistings,exsistings|>List.tail|>List.head
                let applyStopNumberPositionFix((x,y),stop)=
                    let positions=[0;1;-1;2;-2]
                    let fix=positions.[stop]
                    x+fix,y+fix
                let current,previous=applyStopNumberPositionFix current,applyStopNumberPositionFix previous
                //let applyCurrentStopNumberPositionFix,applyPreviousStopNumberPositionFixcurrent=applyStopNumberPositionFix current,applyStopNumberPositionFix previous
                let isOn45Degree(x1,y1)(x2,y2)=x1=x2||y1=y2||abs x1-x2=abs y1-y2
                if isOn45Degree current previous then[current;previous]
                else
                    let getTurnPoint(x1,y1)(x2,y2)=
                        let xDistance,yDistance=x1-x2,y1-y2
                        let getDistance a b distanceAfterTurn=
                            //let areSameSign a b=a=0&&b=0||a>0=(b>0)
                            //let distance=if areSameSign a b then a-b else a+b
                            let a=a-b in if a>=0 then a-abs distanceAfterTurn else  a+abs distanceAfterTurn
                        if abs xDistance>abs yDistance then x2+getDistance x1 x2 yDistance,y2
                            else x2,y2+getDistance y1 y2 xDistance
                    assert(getTurnPoint(10,10)(20,30)=(20,20))
                    assert(getTurnPoint(10,10)(30,20)=(20,20))
                    assert(getTurnPoint(20,30)(10,10)=(10,20))
                    assert(getTurnPoint(30,20)(10,10)=(20,10))
                    assert(getTurnPoint(30,40)(20,10)=(20,30))
                    assert(getTurnPoint(20,10)(30,40)=(30,20))
                    assert(getTurnPoint(10,40)(50,20)=(30,20))
                    [current;getTurnPoint current previous;previous]
            let p remain=if List.length remain<=1 then None else Some(calculatePointsForStop remain,List.tail remain)
            stops|>List.unfold p|>List.concat
    ///地铁网
    //module Network=
    //    let draw a=