
module Magic

open System
open System.Net
open HtmlAgilityPack

let fixRulesHtml (innerHtml: string): string =
    let unsplit = innerHtml.Replace("<b>", "").Replace("</b>", "")
    let split = unsplit.Split(Array.init 1 (fun _ -> "<br>"), StringSplitOptions.RemoveEmptyEntries)
    let fixedLines = seq {
        for line in split do
            let clean = line.Trim()
            if not <| clean.EndsWith(".") then
                yield clean + "."
            else
                yield clean
    }
    String.Join("  ", fixedLines)

type GetRulesTextResult =
    | Ok of string
    | MultipleMatches
    | NoMatches
    | ParseError
    | Error

let getRulesTextLine (search: string): GetRulesTextResult =
    let escapedSearch = Uri.EscapeDataString search
    let url = sprintf "http://magiccards.info/query?q=%s" escapedSearch
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
                            let rulesText = rulesTextNodes.[0].InnerHtml |> fixRulesHtml
                            let preRulesText = preRulesNode.InnerText.Replace("â€”", "-").Replace("\n", "").Replace(",          ", "").Trim() // please dont hurt me
                            sprintf "%s: %s %s" nameUrlNode.InnerText preRulesText rulesText |> Ok
                        else
                            ParseError
                    else
                        ParseError
                | x -> MultipleMatches
    else
        Error