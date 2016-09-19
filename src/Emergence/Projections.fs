namespace Emergence
open System


type AggregateId = Guid
type AggregateVersion = int


type IMetaData = 
    abstract  Version : AggregateVersion


type WithMeta<'meta, 'event> when 'meta :> IMetaData = 
    {
        Event : 'event
        Meta  : 'meta
    }


type Projection<'meta, 'event, 'snap, 'result> when 'meta :> IMetaData = 
    {
        Fold : 'snap -> WithMeta<'meta, 'event> -> 'snap
        Proj : 'snap -> 'result
        Init : 'snap
    }


module Primitives =

    let foldInto (p : Projection<'meta, 'event, 'snap, 'result>) (events : WithMeta<'meta, 'event> seq) =
        Seq.fold p.Fold p.Init events
        |> p.Proj

    let createProjection (f : 'snap -> 'event -> 'snap) (init : 'snap) (proj : 'snap -> 'result) : Projection<'meta, 'event, 'snap, 'result> = 
        {
            Fold = fun snap withMeta -> f snap withMeta.Event
            Init = init
            Proj = proj
        }

    
    let fmapP (f : 'a -> 'b) (pa : Projection<'meta, 'event, 'snap, 'a>) : Projection<'meta, 'event, 'snap, 'b> =
      {
        Init = pa.Init
        Fold = pa.Fold
        Proj = pa.Proj >> f
      }


    type Pair<'a,'b> = { 
        First     : 'a
        FirstVer  : AggregateVersion
        Second    : 'b 
        SecondVer : AggregateVersion
        }


    let parallelP (pa : Projection<'meta, 'event, 'snapA, 'resultA>, pb : Projection<'meta, 'event, 'snapB, 'resultB>) 
      : Projection<'meta, 'evetn, Pair<'snapA, 'snapB>, 'resultA * 'resultB> =
        { 
            Init = 
                {
                    First  = pa.Init
                    FirstVer = 0
                    Second = pb.Init 
                    SecondVer = 0
                } 

            Proj = function 
                | { First = sA; Second = sB } -> 
                    (pa.Proj sA, pb.Proj sB)

            Fold = fun pair ev ->
                let fst = 
                    if ev.Meta.Version > pair.FirstVer
                    then pa.Fold pair.First ev
                    else pair.First
                let snd = 
                    if ev.Meta.Version > pair.SecondVer
                    then pb.Fold pair.Second ev
                    else pair.Second
                in { pair with First = fst; Second = snd }
          }


    let pureP value =
        {
            Init = ()
            Proj = fun _ -> value
            Fold = (fun _ _ -> ())
        }


    let applictiveMapP (pf : Projection<'meta, 'event, 'snapF, 'a -> 'b>) (pa : Projection<'meta, 'event, 'snapA, 'a>) 
        : Projection<'meta, 'event, Pair<'snapF, 'snapA>, 'b> =
        parallelP (pf, pa)
        |> fmapP (fun (f,a) -> f a)


module Operators =
    open Primitives

    let (|*>) p f =
        fmapP f p


    let (<*>) = 
        applictiveMapP


    let ( *>) f a = 
        (pureP f) <*> a


module Projections =

    open Primitives

    let lastP (select : 'event -> 'result option) =
        let updateSome newVal oldVal = 
            match newVal with
            | Some _ -> newVal
            | None   -> oldVal
        createProjection
            (fun oldValue ev -> updateSome (select ev) oldValue)
            None
            id


    type Labeled<'label,'a> = Labeled of 'a


    let lastLabeledP (_ : 'label) (select : 'event -> 'result option) 
        : Projection<'meta, 'event, Labeled<'label, 'result> option, 'result option> =
        let updateSome newVal oldVal = 
            match newVal with
            | Some value -> Some (Labeled value)
            | None       -> oldVal
        createProjection
            (fun opt ev -> opt |> updateSome (select ev))
            None
            (Option.map (fun (Labeled value) -> value))


    type Sum<'label,'a> = Sum of 'a


    let inline sumByP (_ : 'label) (select : 'event -> 'num option) 
        : Projection<'meta, 'event, Sum<'label, 'num>, 'num> =
        createProjection
            (fun (Sum sum) ev ->
                match select ev with
                | Some nr -> Sum (sum + nr)
                | None    -> Sum sum)
            (Sum LanguagePrimitives.GenericZero)
            (fun (Sum num) -> num)


    let countByP (label : 'label) (select : 'event -> bool)
        : Projection<'meta, 'event, Sum<'label, int>, int> =
        let toNum = 
            function 
            | true -> Some 1 
            | false -> None
        sumByP label (select >> toNum)