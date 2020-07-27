module TypeDrivenDevelopment.PollingConsumer

open System
open TypeDrivenDevelopment.Timed

type MessageHandler = unit -> Timed<unit>

// State data

type ReadyData = Timed<TimeSpan list>
type ReceivedMessageData = Timed<TimeSpan list * MessageHandler>
type NoMessageData = Timed<TimeSpan list>

//States as discriminate union
type State =
    | ReadyState of ReadyData
    | ReceivedMessageState of ReceivedMessageData
    | NoMessageState of NoMessageData
    | StoppedState

//Transitions: Data -> PollingConsumer
let transitionFromStopped = StoppedState

let transitionFromNoMessage shouldIdle idle (nm: NoMessageData): State =
    if shouldIdle (nm) then
        idle()
        |> Untimed.withResult nm.Result
        |> ReadyState
    else
        StoppedState