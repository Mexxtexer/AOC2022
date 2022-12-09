module Typer
open Common
open Input

type Kørselsparametre =
    { Dag         : int }

// rock paper scissors, dag2
type rps =
    | A
    | B
    | C
    | X // need to loose
    | Y // need to draw
    | Z // need to win
    static member getPoint (rps : rps) = 
        match rps with
        | X
        | A -> 1
        | Y
        | B -> 2
        | Z
        | C -> 3
    static member getWin (oppPlay : int) = 
        match oppPlay with | 1 -> 2 | 2 -> 3 | 3 -> 1 | _ -> failwith ""
    static member getLose (oppPlay : int) = 
        match oppPlay with | 1 -> 3 | 2 -> 1 | 3 -> 2 | _ -> failwith ""
    static member getPlay (oppPlay : int) (outcome : int) = 
        match outcome with
        | 1 -> rps.getLose oppPlay
        | 2 -> oppPlay // draw
        | 3 -> rps.getWin oppPlay
        | _ -> failwith "Fejl ved parsing"
    static member fromString (str : string) = 
        match str.ToLower() with
        | "a" -> A
        | "b" -> B
        | "c" -> C
        | "x" -> X
        | "y" -> Y
        | "z" -> Z
        | _ -> failwith "Fejl ved parsing"
    static member fromStringPair (str : string) : rps*rps = 
        str.ToLower().[0] |> string |> rps.fromString, 
        str.ToLower().[2] |> string |> rps.fromString
    static member getPoints rsps : int*int = 
        rsps |> Common.tupleMap rps.getPoint
    static member getPointFromPoints ((p1, p2) : int*int) : int = 
        p2 + // Husk point fra bare at spille
        match p2 = p1 with
        | true -> 3 // Draw
        | false ->
            match p2, p1 with
            | 3, 2 -> 6 // Win
            | 2, 1 -> 6 // Win
            | 1, 3 -> 6 // Win
            | _ -> 0


module filesystem =
    type file = {Name : string; Size : int}
    type dir = 
        {
            Name : string
            Files : file[]
            Dirs : dir[]
        }
        static member empty (name) = {Name = name; Files = Array.empty; Dirs = Array.empty}
        static member findDir dir strName = 
            dir.Dirs |> Array.find (fun d -> d.Name = strName)
        static member size (d : dir) = 
            let sizeThisDir = d.Files |> Array.map (fun x -> x.Size) |> Array.sum
            match d.Dirs.Length with 
            | 0 -> sizeThisDir
            | _ -> 
                let sizeSubDirs = d.Dirs |> Array.map dir.size |> Array.sum
                sizeThisDir + sizeSubDirs
        static member addDirDeep (orgDir : dir) (cd : string[]) (addDir : dir) : dir = 
            
            match cd.Length with 
            | 1 -> 
                // at the end
                dir.addDir orgDir addDir
            | _ -> 
                let newDirName = (cd |> Array.skip 1 |> Array.head)
                let newOrdDir = dir.findDir orgDir newDirName
                let newOrdDirWithAdd = dir.addDirDeep newOrdDir (cd |> Array.tail) addDir
                let dirIdx = orgDir.Dirs |> Array.findIndex (fun d -> d.Name = newDirName)

                orgDir.Dirs.[dirIdx] <- newOrdDirWithAdd
                orgDir

         static member addFileDeep (orgDir : dir) (cd : string[]) (addFile : file) : dir = 
            
            match cd.Length with 
            | 1 -> 
                // at the end
                dir.addFile orgDir addFile
            | _ -> 
                let newDirName = (cd |> Array.skip 1 |> Array.head)
                let newOrdDir = dir.findDir orgDir newDirName
                let newOrdDirWithAdd = dir.addFileDeep newOrdDir (cd |> Array.tail) addFile
                let dirIdx = orgDir.Dirs |> Array.findIndex (fun d -> d.Name = newDirName)

                orgDir.Dirs.[dirIdx] <- newOrdDirWithAdd
                orgDir
 
        static member addDir (orgDir : dir) (addDir : dir) = 
            match Array.contains addDir orgDir.Dirs with
            | true -> orgDir // do nothing
            | false ->  {orgDir with Dirs = Array.append orgDir.Dirs [|addDir|] }
        static member addFile orgDir (addFile : file) = 
            match Array.contains addFile orgDir.Files with
            | true -> orgDir // do nothing
            | false ->  {orgDir with Files = Array.append orgDir.Files [|addFile|] }
        static member getDirFromInput (dag : int) = 
            let data = dag |> Input.path |> Input.readLines
            let mutable path = [|"root"|]
            let mutable root = dir.empty("root")
            data 
            |> Seq.toArray
            |> Array.iter (fun str -> 
                    match str with
                    | re.RegexGroups "\$ cd \/" host -> ()
                    | re.RegexGroups "dir (.*)" host ->
                        let newDirName = host |> re.getFromGroup 1 |> Seq.exactlyOne 
                        root <- dir.addDirDeep root path (dir.empty(newDirName))
                    | re.RegexGroups "\$ cd (.*)" host -> 
                        let newDirName = host |> re.getFromGroup 1 |> Seq.exactlyOne
                        match newDirName with 
                        | ".." -> 
                            path <- path |> Array.rev |> Array.tail |> Array.rev
                        | strName -> 
                            path <- Array.append path [|newDirName|]
                    | re.RegexGroups "[0-9]*[0-9] [a-zA-Z]*[a-zA-Z]" host -> 
                        let file : file = 
                            let a =  host |> re.getFromGroup 0
                            let b = a |> Seq.exactlyOne
                            let spl = (host |> re.getFromGroup 0 |> Seq.exactlyOne).Split " "
                            {Name = spl.[1]; Size = spl.[0] |> int}

                        root <- dir.addFileDeep root path file
                    | re.RegexGroups "\$ ls" host -> ()
                    | _ -> ()
            )
            root
        static member getAllSizes (d : dir) = 
            let mutable sizes = [||]

            let rec getSizes (d : dir) = 
                let sizeThisDir = d |> dir.size
                sizes <- Array.append sizes [|(d, sizeThisDir)|] 
                match d.Dirs.Length with 
                | 0 -> ()
                | _ -> 
                    d.Dirs |> Array.iter getSizes

            getSizes d
            sizes

        