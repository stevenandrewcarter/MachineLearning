﻿#load "NaiveBayes.fs"
open System.IO
open System.Text.RegularExpressions
open NaiveBayes.Classifier

type DocType = 
    | Ham
    | Spam

let parseDocType (label:string) =
    match label with
    | "ham"     -> Ham
    | "spam"    -> Spam
    | _         -> failwith "Unknown Label"

let parseLine (line:string) =
    let split = line.Split('\t')
    let label = split.[0] |> parseDocType
    let message = split.[1]
    (label, message)

let fileName = "SMSSpamCollection"
let path = __SOURCE_DIRECTORY__ + @"..\..\Data\" + fileName

let dataset = 
    File.ReadAllLines path
    |> Array.map parseLine

let matchWords = Regex(@"\w+")
let wordTokenizer (text:string) = 
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let validation, training = dataset.[..999], dataset.[1000..]

let txtClassifier = train training wordTokenizer (["txt"] |> set)

validation
|> Seq.averageBy (fun (docType, sms) ->  if docType = txtClassifier sms then  1.0 else 0.0)
|> printfn "Based on 'txt', correctly classified %.3f"

let allTokens =
        training 
        |> Seq.map snd
        |> vocabulary wordTokenizer

let fullClassifier = train training wordTokenizer allTokens

validation
|> Seq.averageBy (fun (docType, sms) -> if docType = fullClassifier sms then 1.0 else 0.0)
|> printfn "Based on all tokens, correctly classified %.3f"

let evaluate (tokenizer:Tokenizer) (tokens:Token Set) =
    let classifier = train training tokenizer tokens
    validation
    |> Seq.averageBy (fun (docType, sms) -> if docType = classifier sms then 1.0 else 0.0)
    |> printfn "Correctly Classified %.3f"

let caseTokenizer (text:string) =
    text
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let casedTokens = training |> Seq.map snd |> vocabulary caseTokenizer

evaluate caseTokenizer casedTokens

let top n (tokenizer:Tokenizer) (docs:string []) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens
    |> Seq.sortBy (fun t -> - countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let ham, spam = 
    let rawHam, rawSpam = 
        training
        |> Array.partition (fun (lbl, _) -> lbl = Ham)
    rawHam |> Array.map snd,
    rawSpam |> Array.map snd

let hamCount = ham |> vocabulary caseTokenizer |> Set.count
let spamCount = spam |> vocabulary caseTokenizer |> Set.count

let topHam = ham |> top (hamCount / 10) caseTokenizer
let topSpam = spam |> top (spamCount / 10) caseTokenizer

let topTokens = Set.union topHam topSpam

evaluate caseTokenizer topTokens

ham |> top 20 caseTokenizer |> Seq.iter (printfn "%s")
spam |> top 20 caseTokenizer |> Seq.iter (printfn "%s")

let commonTokens = Set.intersect topHam topSpam
let specificTokens = Set.difference topTokens commonTokens

evaluate caseTokenizer specificTokens

let rareTokens n (tokenizer:Tokenizer) (docs:string[]) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany 
    tokens |> Seq.sortBy (fun t -> countIn tokenized t) |> Seq.take n |> Set.ofSeq

let rareHam = ham |> rareTokens 50 caseTokenizer |> Seq.iter (printfn "%s")
let rareSpam = spam |> rareTokens 50 caseTokenizer |> Seq.iter (printfn "%s")

let phoneWords = Regex(@"0[7-9]\d{9}")
let phone (text:string) = 
    match (phoneWords.IsMatch text) with 
    | true -> "__PHONE__"
    | false -> text

let txtCode = Regex(@"\b\d{5}\b")
let txt (text:string) = 
    match (txtCode.IsMatch text) with
    | true -> "__TXT__"
    | false -> text

let smartTokenizer = caseTokenizer >> Set.map phone >> Set.map txt

let smartTokens =
    specificTokens |> Set.add "__TXT__" |> Set.add "__PHONE__"

evaluate smartTokenizer smartTokens

let lengthAnalysis len =    
    let long (msg:string) = msg.Length > len
    let ham, spam = dataset |> Array.partition (fun (docType,_) -> docType= Ham)
    let spamAndLongCount = spam |> Array.filter (fun (_, sms) -> long sms) |> Array.length
    let longCount = dataset |> Array.filter (fun (_, sms) -> long sms) |> Array.length
    let pSpam = (float spam.Length) / (float dataset.Length)
    let pLongIfSpam = float spamAndLongCount / float spam.Length
    let pLong = float longCount / float (dataset.Length)
    let pSpamIfLong = pLongIfSpam * pSpam / pLong
    pSpamIfLong

for l in 10 .. 10 .. 130 do
    printfn "P(SPAM IF LENGTH > %i) = %.4f" l (lengthAnalysis l)

let bestClassifier = train training smartTokenizer smartTokens
validation
|> Seq.filter (fun (docType, _) -> docType = Ham) 
|> Seq.averageBy (fun (docType, sms) -> 
    if docType = bestClassifier sms then 1.0 else 0.0)
|> printfn "Correct ham %.5f"
validation
|> Seq.filter (fun (docType, _) -> docType = Spam) 
|> Seq.averageBy (fun (docType, sms) -> 
    if docType = bestClassifier sms then 1.0 else 0.0)
|> printfn "Correct Spam %.5f"
    

//let spamWithFREE =
//    dataset
//    |> Array.filter (fun (docType,_) -> docType = Spam)
//    |> Array.filter (fun (_, sms) -> sms.Contains("FREE"))
//    |> Array.length
//
//let hamWithFree =
//    dataset
//    |> Array.filter (fun (docType,_) -> docType = Ham)
//    |> Array.filter (fun (_, sms) -> sms.Contains("FREE"))
//    |> Array.length
//
//let primitiveClassifier (sms:string) = 
//    if (sms.Contains("FREE")) 
//    then Spam
//    else Ham