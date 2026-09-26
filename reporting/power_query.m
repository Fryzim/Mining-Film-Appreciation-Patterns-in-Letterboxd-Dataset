// Power Query (M) — Letterboxd dataset cleaning
//
// Usage: Power BI Desktop > Get Data > Blank Query > Advanced Editor > paste this,
// then set FilePath below to the local path of final_movies_dataset.csv.
//
// NOTE: final_movies_dataset.csv is not committed to this repository (see README) —
// add your own copy locally to use this script. Same cleaning as analysis.R:
// same filters on rating/duration/year, same derived columns (decade, primary
// country, genre/studio/country/actor counts from the pipe-separated fields).

let
    FilePath = "C:\Path\To\Mining-Film-Appreciation-Patterns-in-Letterboxd-Dataset\final_movies_dataset.csv",

    Source = Csv.Document(
        File.Contents(FilePath),
        [Delimiter=",", Encoding=65001, QuoteStyle=QuoteStyle.Csv]
    ),
    PromotedHeaders = Table.PromoteHeaders(Source, [PromoteAllScalars=true]),

    RemovePoster = Table.RemoveColumns(PromotedHeaders, {"poster_filename"}, MissingField.Ignore),

    Typed = Table.TransformColumnTypes(RemovePoster, {
        {"rating", type number}, {"minute", Int64.Type}, {"date", Int64.Type},
        {"genre", type text}, {"studio", type text}, {"country", type text}, {"actors", type text}
    }),

    AddYear = Table.AddColumn(Typed, "year", each [date], Int64.Type),

    // Genre/studio/country/actor fields are pipe-separated (e.g. "Drama|Comedy") —
    // country_primary keeps only the first one
    AddCountryPrimary = Table.AddColumn(AddYear, "country_primary", each
        let
            firstPart = Text.BeforeDelimiter([country], "|"),
            trimmed = Text.Trim(firstPart)
        in
            if trimmed = "" then null else trimmed,
        type text
    ),

    // Counts how many pipe-separated values a field holds (0 if blank)
    SplitLen = (x as nullable text) as number =>
        if x = null or x = "" then 0 else List.Count(Text.Split(x, "|")),

    AddCounts = Table.AddColumn(
        Table.AddColumn(
            Table.AddColumn(
                Table.AddColumn(AddCountryPrimary, "n_genres", each SplitLen([genre]), Int64.Type),
                "n_studios", each SplitLen([studio]), Int64.Type),
            "n_countries", each SplitLen([country]), Int64.Type),
        "n_actors", each SplitLen([actors]), Int64.Type
    ),

    // Same filters as the R script: exclude missing rating/duration/year,
    // and drop outliers (very short/long runtimes, implausible years)
    FilterRows = Table.SelectRows(AddCounts, each
        [rating] <> null and [minute] <> null and [year] <> null
        and [minute] > 40 and [minute] < 300
        and [year] >= 1950 and [year] <= 2024
    ),

    AddDecade = Table.AddColumn(FilterRows, "decade", each Number.RoundDown([year] / 10) * 10, Int64.Type)
in
    AddDecade
