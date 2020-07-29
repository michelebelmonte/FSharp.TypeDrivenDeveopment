module TypeDrivenDevelopment.PollingConsumer

open System
open TypeDrivenDevelopment.Timed

[<CustomEquality; NoComparison>]
type MessageHandler =
    {    Handle:unit -> Timed<unit>   }

    override this.Equals obj =
        match obj with
        | :? MessageHandler as other -> Object.Equals (this.Handle, other.Handle)
        | _ -> false

    override this.GetHashCode () = (box this).GetHashCode()

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

let transitionFromReady shouldPoll poll (rd: ReadyData): State =
    if shouldPoll rd then
        let msg = poll()
        match msg.Result with
        | Some h ->
            msg
            |> Untimed.withResult (rd.Result, h)
            |> ReceivedMessageState
        | None ->
            msg
            |> Untimed.withResult rd.Result
            |> NoMessageState
    else
        StoppedState