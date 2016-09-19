# Emergence
build up complex folds/projects from basic building blocks

## Example

given events:

    type Events =
        | Created of Name
        | Rated of Rating

where

    type Name = Name of string

    type Rating = 
        | OneStar
        | TwoStars
        | ThreeStars

you can build up a projection for

    type Movie =
        {
            Name : Name
            Rating : Ratings
        }

and

    type Ratings =
        {
            Count : int
            AvarageStars : decimal
        }

stepwise by first defining building-projections for the name of the movie,
the count of ratings and the sum of rated stars:

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

and then combine those first into a projection for a `Rating`:

    let rating : Projection<_, _, _, Ratings> =
        (fun count sum ->
            let avg =
                if count > 0 
                then decimal sum / decimal count 
                else 0m
            in { Count = count; AvarageStars = avg })
        *> ratingCount <*> ratingSum

and finally for the complete `Movie`-record:

    let movie : Projection<_, _, _, Movie> =
        create *> name <*> rating


