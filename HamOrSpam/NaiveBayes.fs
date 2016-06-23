namespace NaiveBayes

module Classifier =
    // Define the Token Type
    type Token = string
    // Define a Tokenized Document as a Set of Tokens
    type TokenizedDoc = Token Set
    // Define a Tokenizer as a function that takes a string and returns a Tokenized Document
    type Tokenizer = string -> TokenizedDoc 
    // Define a Document Group as a Proportion, with the frequenices a particular token is found in the data
    type DocGroups = { Proportion:float; TokenFrequencies:Map<Token, float> }

    let tokenScore (group:DocGroups) (token:Token) =
        if group.TokenFrequencies.ContainsKey(token) 
        then log group.TokenFrequencies.[token]
        else 0.0

    let score (document:TokenizedDoc) (group:DocGroups) = 
        let scoreToken = tokenScore group
        log group.Proportion + (document |> Seq.sumBy scoreToken)

    let classify (groups:(_*DocGroups)[])
                 (tokenizer:Tokenizer)
                 (txt:string) =
        let tokenized = tokenizer txt
        groups
        |> Array.maxBy (fun (label, group) -> score tokenized group)
        |> fst

    let proportion count total = float count / float total

    let laplace count total = float (count + 1) / float (total + 1)

    let countIn (group:TokenizedDoc seq) (token:Token) =
        group
        |> Seq.filter (Set.contains token)
        |> Seq.length

    let analyze (group:TokenizedDoc seq)
                (totalDocs:int)
                (classificationTokens:Token Set) =
        let groupSize = group |> Seq.length
        let score token =
            let count = countIn group token
            laplace count groupSize
        let scoredTokens =
            classificationTokens
            |> Set.map (fun token -> token, score token)
            |> Map.ofSeq
        let groupProportion = proportion groupSize totalDocs
        { Proportion = groupProportion; TokenFrequencies=scoredTokens }

    let learn (docs:(_ * string)[])
              (tokenizer:Tokenizer)
              (classificationTokens:Token Set) =
        let total = docs.Length
        docs
        |> Array.map (fun (label, docs) -> label, tokenizer docs)
        |> Seq.groupBy fst
        |> Seq.map (fun (label, group) -> label, group |> Seq.map snd)
        |> Seq.map (fun (label, group) -> label, analyze group total classificationTokens)
        |> Seq.toArray

    let train (docs:(_ * string)[]) 
              (tokenizer:Tokenizer)
              (classificationTokens:Token Set) =
        let groups = learn docs tokenizer classificationTokens
        let classifier = classify groups tokenizer
        classifier

    let vocabulary (tokenizer:Tokenizer) (corpus:string seq) = 
        corpus
        |> Seq.map tokenizer
        |> Set.unionMany
