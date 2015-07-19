
module Magic

open System
open System.Net
open System.Text.RegularExpressions

open HtmlAgilityPack

let regexParens = @"\([^)]*\)"
let regexParensStatement = sprintf @"%s" regexParens
let regexReplace (pat: string) (replace: string) (s: string) = Regex.Replace(s, pat, replace)

let fixPreRulesText (preRules: string): string =
    preRules
        |> regexReplace @" \(\d+\)\s*$" "" // remove total cmc at end
        |> regexReplace @" (\d)$" @" {$1}" // turn bare number cost into bracket style for clarity

let fixRulesHtml (name: string) (innerHtml: string): string =
    let unsplit = innerHtml.Replace("<b>", "").Replace("</b>", "")
    let split = unsplit.Split(Array.init 1 (fun _ -> "<br>"), StringSplitOptions.RemoveEmptyEntries)
    let fixedLines = split |> Seq.map (fun line -> line.Trim())
    let body = String.Join("  ", fixedLines)
    let replacedName = body.Replace(name, "~")
    replacedName
        |> regexReplace regexParensStatement "" // get rid of parens explanations
        |> regexReplace @"(?<=\S) {2,}(?=\S)" @".  " // turn extra spacing (previous newlines?) into periods
        |> regexReplace @"\.\.(?=\s+)" @"." // above is buggy and leads to double period statements
        |> regexReplace @"\{(\d|W|U|B|R|G)\}+" @"{$1}" // simplify costs

type GetRulesTextResult =
    | Ok of string
    | MultipleMatches
    | NoMatches
    | ParseError
    | Error

let getRulesTextLine (search: string): GetRulesTextResult =
    let escapedSearch = Uri.EscapeDataString search
    let url = 
        if String.Equals(search, "random", StringComparison.InvariantCultureIgnoreCase) then
            "http://magiccards.info/random.html"
        else
            sprintf "http://magiccards.info/query?q=%s" escapedSearch

    let req = WebRequest.Create(url)
    use resp = req.GetResponse() :?> HttpWebResponse
    
    if resp.StatusCode = HttpStatusCode.OK then
        use stream = resp.GetResponseStream()
        let doc = new HtmlDocument()
        doc.Load(stream)

        let rulesTextNodes = doc.DocumentNode.SelectNodes("//p[@class='ctext']")

        if rulesTextNodes = null then
            ParseError
        else
            match rulesTextNodes.Count with
                | 0 -> NoMatches
                | 1 ->
                    let middleColumn = doc.DocumentNode.SelectSingleNode("//td[@width='70%']")

                    if middleColumn <> null then
                        let nameUrlNode = middleColumn.SelectSingleNode("span/a")
                        let preRulesNode = middleColumn.SelectSingleNode("p")

                        if nameUrlNode <> null && preRulesNode <> null then
                            let name = nameUrlNode.InnerText
                            let rulesText = rulesTextNodes.[0].InnerHtml |> fixRulesHtml name
                            let preRulesText = preRulesNode.InnerText.Replace("â€”", "-").Replace("\n", "").Replace(",          ", "").Trim() |> fixPreRulesText // please dont hurt me
                            sprintf "%s: %s: %s" name preRulesText rulesText |> Ok
                        else
                            ParseError
                    else
                        ParseError
                | x -> MultipleMatches
    else
        Error