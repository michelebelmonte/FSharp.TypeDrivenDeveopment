module PollingConsumerProperties

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open TypeDrivenDevelopment.PollingConsumer
open TypeDrivenDevelopment.Timed

[<Property>]
let ``transitionFromNoMessage returns corrent result when it should idle``
    (nm:NoMessageData)
    (idleRes : Timed<unit>) =

    let shouldIdle _ = true
    let idle _ = idleRes

    let actual : State =
        transitionFromNoMessage shouldIdle idle nm

    let expected =
        idleRes |> Untimed.withResult nm.Result |> ReadyState

    expected =! actual

[<Property>]
let ``transitionFromNoMessage returns corrent result when it should not idle``
    (nm:NoMessageData)
    (idleRes : Timed<unit>) =

    let shouldIdle _ = false
    let idle _ = idleRes

    let actual : State =
        transitionFromNoMessage shouldIdle idle nm

    let expected = StoppedState

    expected =! actual

[<Property>]
let ``transitionFromReady returns corrent result when it should not poll``
    (r:ReadyData)
    (mh : Timed<MessageHandler option>) =

    let shouldPoll _ = false
    let poll _ = mh

    let actual : State =
        transitionFromReady shouldPoll poll r

    let expected = StoppedState

    expected =! actual

[<Property>]
let ``transitionFromReady returns corrent result when polling no message``
    (r:ReadyData)
    (mh : Timed<unit>) =

    let shouldPoll _ = true
    let poll _ = mh |> Untimed.withResult None

    let actual : State =
        transitionFromReady shouldPoll poll r

    let expected = mh |> Untimed.withResult r.Result |> NoMessageState

    expected =! actual

[<Property>]
let ``transitionFromReady returns corrent result when polling a message``
    (r:ReadyData)
    (mh : Timed<MessageHandler>) =

    let shouldPoll _ = true
    let poll _ = mh |> Untimed.withResult ( Some mh.Result)

    let actual : State =
        transitionFromReady shouldPoll poll r

    let expected = mh |> Untimed.withResult (r.Result,mh.Result) |> ReceivedMessageState

    expected =! actual

[<Property>]
let ``run runs untill stopped``
   (states : State list)
   (startState : State) =
   (states |> List.exists ((=) StoppedState)) ==> lazy

   let q = System.Collections.Generic.Queue<State> states
   let transition _ = q.Dequeue()

   let actual = run transition startState

   StoppedState =! actual

[<Property>]
let ``unfurl returns correct sequence with constant transition``
    (initialValue: string)
    (constantValue: string)
    (count : byte)=
    let getNext _= constantValue
    let actual : string seq = unfurl getNext initialValue
    test <@
        actual
        |> Seq.skip 1
        |> Seq.truncate (int count)
        |> Seq. forall ((=) constantValue)
    @>

[<Property>]
let ``unfurl returns as many values as requested``
    (initialValue: System.TimeSpan)
    (count : byte)=

    let actual : System.TimeSpan seq = unfurl id initialValue
    int count =! (actual |> Seq.truncate (int count) |> Seq.length)

[<Property>]
let ``unfurl returns correct values``
    (initialValue : byte)
    (count : byte) =
    let actual = unfurl ((+) 1) (int initialValue)
    let expected = [int initialValue .. int initialValue + int count]
    expected =!
        (actual |> Seq.truncate (int count + 1) |> Seq.toList)

type WithoutStoppedState =
    static member State() =
        Arb.Default.Derive ()
        |> Arb.filter (not << isStopped)

[<Property(Arbitrary = [|typeof<WithoutStoppedState>|])>]
let ``run' returns element of sequence without stops``
    (states : State list)=
    not states.IsEmpty ==> lazy

    let actual : State = run' states

    let expected = states |> Seq.last
    expected=!actual