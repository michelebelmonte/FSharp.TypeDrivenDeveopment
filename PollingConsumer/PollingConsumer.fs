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
type StoppedData = TimeSpan list

//States as discriminate union
type State =
    | ReadyState of ReadyData
    | ReceivedMessageState of ReceivedMessageData
    | NoMessageState of NoMessageData
    | StoppedState of StoppedData

//Transitions: Data -> PollingConsumer
let transitionFromStopped = StoppedState

let transitionFromNoMessage shouldIdle idle (nm: NoMessageData): State =
    if shouldIdle (nm) then
        idle()
        |> Untimed.withResult nm.Result
        |> ReadyState
    else
        StoppedState nm.Result

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
        StoppedState rd.Result

let rec run trans state =
    let nextState = trans state
    match nextState with
    | StoppedState -> StoppedState
    | _ -> run trans nextState

let rec unfurl getNext state = seq {
    yield state
    let nextState = getNext state
    yield! unfurl getNext nextState}

let isStopped x =
    match x with
    | StoppedState -> true
    | _ -> false

let run' states =
    // Modified from http://stackoverflow.com/a/12564899/126014
    let takeUntil predicate (s : seq<_>) =
      /// Iterates over the enumerator, yielding elements and
      /// stops after an element for which the predicate holds
      let rec loop (en : System.Collections.Generic.IEnumerator<_>) = seq {
        if en.MoveNext() then
          // Always yield the current, stop when predicate becomes true
          yield en.Current
          if not (predicate en.Current) then
            yield! loop en }

      // Get enumerator of the sequence and yield all results
      // (making sure that the enumerator gets disposed)
      seq { use en = s.GetEnumerator()
            yield! loop en }

    states |> takeUntil isStopped |> Seq.last