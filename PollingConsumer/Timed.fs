module TypeDrivenDevelopment.Timed

open System

type Timed<'a> =
    { Started: DateTimeOffset
      Stopped: DateTimeOffset
      Result: 'a }
    member this.Duration = this.Stopped - this.Started

module Untimed =
    let map f x =
        { Started = x.Started
          Stopped = x.Stopped
          Result = f x.Result }

    let withResult result x = map (fun _ -> result) x

module Timed =
    let capture clock x =
        let now = clock()
        { Started = now
          Stopped = now
          Result = x }

    let map clock f x =
        let result = f x.Result
        let now = clock()
        { Started = x.Started
          Stopped = now
          Result = result }

    let timedOn clock f x =
        x
        |> capture clock
        |> map clock f

module Clocks =
    open System.Collections.Generic

    let toString (x: DateTimeOffset) = x.ToString "T"

    let machine() = DateTimeOffset.Now

    let acceleratedClock (start: DateTimeOffset) rate () =
        let now = machine()
        let span = now - start
        start.AddTicks(span.Ticks * rate)

    let queueClock (q: Queue<DateTimeOffset>) = q.Dequeue

    let seqClock (s: seq<DateTimeOffset>) =
        s
        |> Queue<DateTimeOffset>
        |> queueClock