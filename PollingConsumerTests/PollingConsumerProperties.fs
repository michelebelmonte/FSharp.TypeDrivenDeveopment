module PollingConsumerProperties

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
let ``transitionFromReady returns corrent result when it should not idle``
    (nm:ReadyData)
    (pollRes : Timed<MessageHandler option>) =

    let shouldPoll _ = true
    let poll _ = pollRes

    let actual : State =
        transitionFromReady shouldPoll poll nm

    let expected = StoppedState

    expected =! actual