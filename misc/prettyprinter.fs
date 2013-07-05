type 'a tree =    
    | Node of 'a tree * 'a * 'a tree
    | Nil

(*
For any given tree
     ddd
     / \
   lll rrr  
we think about it as these three sections, left|middle|right (L|M|R):
     d | d | d
     / |   | \
   lll |   | rrr  
M is always exactly one character.
L will be as wide as either (d's width / 2) or L's width, whichever is more (and always at least one)
R will be as wide as either ((d's width - 1) / 2) or R's width, whichever is more (and always at least one)
     (above two lines mean 'dddd' of even length is slightly off-center left)
We want the '/' to appear directly above the rightmost character of the direct left child.
We want the '\' to appear directly above the leftmost character of the direct right child.
If the width of 'ddd' is not long enough to reach within 1 character of the slashes, we widen 'ddd' with
    underscore characters on that side until it is wide enough.
*)

// PrettyAndWidthInfo : 'a tree -> string[] * int * int * int
// strings are all the same width (space padded if needed)
// first int is that total width
// second int is the column the root node starts in
// third int is the column the root node ends in
// (assumes d.ToString() never returns empty string)
let rec PrettyAndWidthInfo t =
    match t with
    | Nil -> 
        [], 0, 0, 0
    | Node(Nil,d,Nil) -> 
        let s = d.ToString()
        [s], s.Length, 0, s.Length-1
    | Node(l,d,r) ->
        // compute info for string of this node's data
        let s = d.ToString()
        let sw = s.Length
        let swl = sw/2
        let swr = (sw-1)/2
        assert(swl+1+swr = sw)  
        // recurse
        let lp,lw,_,lc = PrettyAndWidthInfo l
        let rp,rw,rc,_ = PrettyAndWidthInfo r
        // account for absent subtrees
        let lw,lb = if lw=0 then 1," " else lw,"/"
        let rw,rb = if rw=0 then 1," " else rw,"\\"
        // compute full width of this tree
        let totalLeftWidth = (max (max lw swl) 1)
        let totalRightWidth = (max (max rw swr) 1)
        let w = totalLeftWidth + 1 + totalRightWidth
(*
A suggestive example:
     dddd | d | dddd__
        / |   |       \
      lll |   |       rr
          |   |      ...
          |   | rrrrrrrrrrr
     ----       ----           swl, swr (left/right string width (of this node) before any padding)
      ---       -----------    lw, rw   (left/right width (of subtree) before any padding)
     ----                      totalLeftWidth
                -----------    totalRightWidth
     ----   -   -----------    w (total width)
*)
        // get right column info that accounts for left side
        let rc2 = totalLeftWidth + 1 + rc
        // make left and right tree same height        
        let lp = if lp.Length < rp.Length then lp @ List.init (rp.Length-lp.Length) (fun _ -> "") else lp
        let rp = if rp.Length < lp.Length then rp @ List.init (lp.Length-rp.Length) (fun _ -> "") else rp
        // widen left and right trees if necessary (in case parent node is wider, and also to fix the 'added height')
        let lp = lp |> List.map (fun s -> if s.Length < totalLeftWidth then (nSpaces (totalLeftWidth - s.Length)) + s else s)
        let rp = rp |> List.map (fun s -> if s.Length < totalRightWidth then s + (nSpaces (totalRightWidth - s.Length)) else s)
        // first part of line1
        let line1 =
            if swl < lw - lc - 1 then
                (nSpaces (lc + 1)) + (nBars (lw - lc - swl)) + s
            else
                (nSpaces (totalLeftWidth - swl)) + s
        // line1 right bars
        let line1 =
            if rc2 > line1.Length then
                line1 + (nBars (rc2 - line1.Length))
            else
                line1
        // line1 right padding
        let line1 = line1 + (nSpaces (w - line1.Length))
        // first part of line2
        let line2 = (nSpaces (totalLeftWidth - lw + lc)) + lb 
        // pad rest of left half
        let line2 = line2 + (nSpaces (totalLeftWidth - line2.Length))
        // add right content
        let line2 = line2 + " " + (nSpaces rc) + rb
        // add right padding
        let line2 = line2 + (nSpaces (w - line2.Length))
        let resultLines = line1 :: line2 :: ((lp,rp) ||> List.map2 (fun l r -> l + " " + r))
        for x in resultLines do
            assert(x.Length = w)
        resultLines, w, lw-swl, totalLeftWidth+1+swr
and nSpaces n = 
    String.replicate n " "
and nBars n = 
    String.replicate n "_"

let PrettyPrint t =
    let sl,_,_,_ = PrettyAndWidthInfo t
    for s in sl do
        printfn "%s" s

let y = Node(Node (Node (Nil,35,Node (Node(Nil,1,Nil),88888888,Nil)),48,Node (Nil,777777777,Node (Nil,53,Nil))),     
             80,Node (Node (Nil,82,Node (Nil,83,Nil)),1111111111,Node (Nil,98,Nil)))
let z = Node(y,55555,y)
let x = Node(z,4444,y)

PrettyPrint x
(*
                                   ___________________________4444_________________
                                  /                                                \
                      ________55555________________                         ________80
                     /                             \                       /         \
            ________80                      ________80             _______48         1111111111
           /         \                     /         \            /        \            /  \
   _______48         1111111111    _______48         1111111111 35         777777777  82   98
  /        \            /  \      /        \            /  \      \             \       \
35         777777777  82   98   35         777777777  82   98     88888888      53      83
  \             \       \         \             \       \            /
  88888888      53      83        88888888      53      83           1
     /                               /
     1                               1
*)
