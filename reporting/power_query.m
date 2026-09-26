// Power Query (M) — Letterboxd dataset cleaning
//
// Usage: Power BI Desktop > Get Data > Blank Query > Advanced Editor > paste this,
// then set FilePath below to the local path of final_movies_dataset.csv.
//
// NOTE: final_movies_dataset.csv is not committed to this repository (see README).
// Add your own copy locally to use this script — the transformation logic
// below mirrors analysis.R's cleaning steps (sections 1-2), so results match
// the R script: same filters, same derived columns (decade, genre/studio/
// country/actor counts, primary country).

let
    FilePath = "C:\Path\To\Mining-Film-Appreciation-Patterns-in-Letterboxd-Dataset\final_movies_dataset.csv",

    Source = Csv.Document(
        File.Contents(FilePath),
        [Delimiter=",", Encoding=65001, QuoteStyle=QuoteStyle.Csv]
    ),
    PromotedHeaders = Table.PromoteHeaders(Source, [PromoteAllScalars=true]),

    // analysis.R: movies$poster_filename <- NULL
    RemovePoster = Table.RemoveColumns(PromotedHeaders, {"poster_filename"}, MissingField.Ignore),

    Typed = Table.TransformColumnTypes(RemovePoster, {
        {"rating", type number}, {"minute", Int64.Type}, {"date", Int64.Type},
        {"genre", type text}, {"studio", type text}, {"country", type text}, {"actors", type text}
    }),

    // analysis.R: movies$year <- as.integer(movies$date)
    AddYear = Table.AddColumn(Typed, "year", each [date], Int64.Type),

    // analysis.R: country.primary <- trimws(gsub("\\|.*", "", country))  -> text before the first "|"
    AddCountryPrimary = Table.AddColumn(AddYear, "country_primary", each
        let
            firstPart = Text.BeforeDelimiter([country], "|"),
            trimmed = Text.Trim(firstPart)
        in
            if trimmed = "" then null else trimmed,
        type text
    ),

    // analysis.R: split.len() — number of "|"-separated values in a pipe-delimited field
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

    // analysis.R: filter on rating/minute/year present, minute in (40,300), year in [1950,2024]
    FilterRows = Table.SelectRows(AddCounts, each
        [rating] <> null and [minute] <> null and [year] <> null
        and [minute] > 40 and [minute] < 300
        and [year] >= 1950 and [year] <= 2024
    ),

    // analysis.R: decade <- floor(year/10)*10
    AddDecade = Table.AddColumn(FilterRows, "decade", each Number.RoundDown([year] / 10) * 10, Int64.Type)
in
    AddDecade
