[<Measure>]
type Min

[<AutoOpen>]
module Movies =

    type Name = Name of string

    type Rating = 
        | OneStar
        | TwoStars
        | ThreeStars
        override this.ToString() =
            match this with
            | OneStar -> "*"
            | TwoStars -> "**"
            | ThreeStars -> "***"
        member this.Int =
            match this with
            | OneStar -> 1
            | TwoStars -> 2
            | ThreeStars -> 3


    type Ratings =
        {
            Count : int
            AvarageStars : decimal
        }


    type Events =
        | Created of Name
        | Rated of Rating


    type Movie =
        {
            Name : Name
            Rating : Ratings
        }
        override this.ToString() =
            let (Name name)= this.Name
            sprintf "%s \n%d times rated with an average of %.1f stars" 
                name
                this.Rating.Count
                this.Rating.AvarageStars


    let create name rating =
        {
            Name = name
            Rating = rating
        }

    let empty =
        create (Name "") { Count = 0; AvarageStars = 0m }


//////////////////////////////////////////////////////////////////////
// Projections

open Emergence
open Emergence.Operators

let fromOption defaultValue =
    function
    | Some value -> value
    | None       -> defaultValue


let name : Projection<_, _, _, Name> =
    let selectName =
        function
        | Created name -> Some name
        | _ -> None
    Projections.lastP selectName
    |*> fromOption (Name "---")
  

type RatingCount = RatingCount
let ratingCount : Projection<_, _, _, int> =
    let isRating =
        function
        | Rated _ -> true
        | _       -> false
    Projections.countByP RatingCount isRating


type RatingSum = RatingSum
let ratingSum : Projection<_, _, _, int> =
    let rating =
        function
        | Rated stars -> Some stars.Int
        | _           -> None
    Projections.sumByP RatingSum rating


let rating : Projection<_, _, _, Ratings> =
    (fun count sum ->
        let avg =
            if count > 0 
            then decimal sum / decimal count 
            else 0m
        in { Count = count; AvarageStars = avg })
    *> ratingCount <*> ratingSum


let movie : Projection<_, _, _, Movie> =
  create *> name <*> rating


//////////////////////////////////////////////////////////////////////
// Example-Events

type SimpleMeta (version : AggregateVersion) =
    interface IMetaData with
        member __.Version = version

let exampleStream =
    let wrap version event =
        { 
            Meta = (SimpleMeta <| version + 1) :> IMetaData
            Event = event
        }
    seq {
        yield (Events.Created (Name "Project X"))
        yield (Events.Rated OneStar)
        yield (Events.Rated ThreeStars)
        yield (Events.Rated TwoStars)
        yield (Events.Rated ThreeStars)
    }
    |> Seq.mapi wrap


//////////////////////////////////////////////////////////////////////
// Main

[<EntryPoint>]
let main argv = 
    let projectX = exampleStream |> Primitives.foldInto movie
    printfn "%A" projectX

    0
