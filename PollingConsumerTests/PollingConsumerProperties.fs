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