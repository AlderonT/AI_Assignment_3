namespace AI
module Utility =

    module VTerm =
        open System.Runtime.InteropServices
        open System

        [<Flags>]
        type ConsoleMode =
            | ENABLE_ECHO_INPUT = 0x0004u
            | ENABLE_EXTENDED_FLAGS = 0x0080u
            | ENABLE_INSERT_MODE = 0x0020u
            | ENABLE_LINE_INPUT = 0x0002u
            | ENABLE_MOUSE_INPUT = 0x0010u
            | ENABLE_PROCESSED_INPUT = 0x0001u
            | ENABLE_QUICK_EDIT_MODE = 0x0040u
            | ENABLE_WINDOW_INPUT = 0x0008u
            | ENABLE_VIRTUAL_TERMINAL_INPUT = 0x0200u  // allows all the VT100 goodness, this is also how all the NEW goodys in Windows 10 Console are surfaced
            | ENABLE_PROCESSED_OUTPUT = 0x0001u
            | ENABLE_WRAP_AT_EOL_OUTPUT = 0x0002u
            | ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004u
            | DISABLE_NEWLINE_AUTO_RETURN = 0x0008u    // If you're extensively using VT100 then you might need to use this mode as well.
            | ENABLE_LVB_GRID_WORLDWIDE = 0x0010u

        type StandardHandles =
            | STD_INPUT_HANDLE = -10
            | STD_OUTPUT_HANDLE = -11
            | STD_ERROR_HANDLE = -12

        [<DllImport("kernel32",CallingConvention=CallingConvention.Winapi,SetLastError=true)>]
        extern bool GetConsoleMode(System.IntPtr hConsoleHandle,ConsoleMode* mode);

        [<DllImport("kernel32",CallingConvention=CallingConvention.Winapi,SetLastError=true)>]
        extern bool SetConsoleMode(System.IntPtr hConsoleHandle,ConsoleMode mode);

        [<DllImport("kernel32",CallingConvention=CallingConvention.Winapi,SetLastError=true)>]
        extern IntPtr GetStdHandle(StandardHandles stdHandle);

        let turnOnVtermSupport() =
            let stdout = GetStdHandle(StandardHandles.STD_OUTPUT_HANDLE)
            let mutable originalConsoleMode = Unchecked.defaultof<_>
            if GetConsoleMode(stdout,&&originalConsoleMode) |> not then
                Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error())
            if SetConsoleMode(stdout,originalConsoleMode|||ConsoleMode.ENABLE_VIRTUAL_TERMINAL_PROCESSING) |> not then
                Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error())
            System.Console.OutputEncoding <- System.Text.Encoding.UTF8
            originalConsoleMode
        
    do
        try
            VTerm.turnOnVtermSupport() |> ignore
        with
            | e -> printfn "Can't turn on VTerm mode, perhaps we're running in a redirected console?\n%s" e.Message

    type Direction = 
        | North 
        | South
        | East
        | West 
    
    type RoomModel = 
        {
            pos : int*int
            hasWampus : bool option
            hasPit : bool option 
            hasBreeze : bool option
            hasStench : bool option
            isWall : bool option
            pathBack : bool
        }

    
    type Room = 
        {
            x : int
            y : int
            fow : bool
            hasChar : bool
            hasWampus : bool
            hasPit : bool
            hasGold : bool
            hasBreeze : bool
            hasStench : bool
            hasGlitter : bool
        }
        with
            member this.Write(b:System.IO.BinaryWriter) =
                b.Write(this.x |> byte)
                b.Write(this.y |> byte)
                let flags =
                    (if this.fow then 1<<<7 else 0)
                    + (if this.hasChar then 1<<<6 else 0)
                    + (if this.hasWampus then 1<<<5 else 0)
                    + (if this.hasPit then 1<<<4 else 0)
                    + (if this.hasGold then 1<<<3 else 0)
                    + (if this.hasBreeze then 1<<<2 else 0)
                    + (if this.hasStench then 1<<<1 else 0)
                    + (if this.hasGlitter then 1<<<0 else 0)
                    |> byte
                b.Write(flags)
            static member Read(b:System.IO.BinaryReader) =
                let x = b.ReadByte() |> int
                let y = b.ReadByte() |> int
                let flags = b.ReadByte() |> int
                let flagSet n = let f = 1 <<< n in  (flags &&& f) = f
                {
                    x = x
                    y = y
                    fow = flagSet 7
                    hasChar = flagSet 6
                    hasWampus = flagSet 5
                    hasPit = flagSet 4
                    hasGold = flagSet 3
                    hasBreeze = flagSet 2
                    hasStench = flagSet 1
                    hasGlitter = flagSet 0
                }


    type Player = 
        {
            currentPos : (int*int)
            direction : Direction 
            hasGold : bool
            escape : bool
            dead : bool
            map : Room[,]
            arrows : int
            finished : bool
            feelsBreeze : bool 
            smellsStench : bool 
            seesGlitter : bool 
            heardScream : bool 
            heardPing : bool
            wumpusLocation : (int*int) option
            memoryMap : Map<int*int,RoomModel>
        }
    type Action =
        | Escape
        | MoveForward
        | TurnRight 
        | TurnLeft 
        | ShootArrow
        | PickupGold
        | LeaveMap
        | FollowPathTo of Action list

    let greedyFirst (memoryMap : Map<int*int, RoomModel>) ((targetx,targety) as targetP:int*int) (currentP:int*int) =       
        let rec loop ((currentx,currenty) as currentP:int*int) (seen:Set<int*int>) backtrack cont =
            if currentP = targetP then cont []
            else
                let possiblePositions = 
                    [
                        currentx,(currenty+1) //North
                        (currentx+1),currenty //East
                        currentx,(currenty-1) //South
                        (currentx-1),currenty //West
                    ]
                    |> List.filter (fun p -> seen.Contains p |> not)
                    |> List.choose(function 
                        | (x,y) as p when x = targetx && y = targety -> 
                            match memoryMap.TryFind p with
                            | None -> Some { pos = p; hasWampus = None; hasPit = None; hasBreeze = None; hasStench = None; isWall = None; pathBack = false }
                            | x -> x
                        | p -> memoryMap.TryFind p
                    )
                    |> List.filter(fun rm -> 
                        match rm.hasWampus, rm.hasPit, rm.isWall with 
                        | Some false, Some false, Some false -> true 
                        | _ when rm.pos = (targetx,targety) -> true
                        | _ -> false       
                    )
                    |> List.sortBy (fun rm -> 
                        let (x,y) = rm.pos
                        abs(x-targetx) + abs(y-targety)
                    )
                match possiblePositions with 
                | [] -> backtrack()
                | x :: rest ->
                    let seen' = seen.Add(x.pos)
                    loop x.pos seen' (fun _ -> loop2 rest seen backtrack cont) (fun rs -> x::rs |> cont)
        and loop2 (rest:RoomModel list) seen oldBacktrack cont =
            match rest with
            | [] -> oldBacktrack() // when no more solutions to work on then backtrack up
            | x :: rest ->
                let seen' = seen.Add(x.pos)
                loop x.pos seen' (fun _ -> loop2 rest seen oldBacktrack cont) (fun rs -> x::rs |> cont)
        loop currentP (Set.singleton currentP) (fun _ -> []) id


    let safeMove (player : Player) (target : int*int) =
        match greedyFirst player.memoryMap target player.currentPos with
        | [] -> None
        | x :: _ ->
            let (sx,sy) = x.pos
            let (px,py) = player.currentPos
            let targetDirection = match sx-px, sy-py with | -1, 0 -> West | 0, 1 -> North | 1, 0 -> East | 0, -1 -> South | a,b -> failwithf "shouldn't happen: (%d,%d)" a b
            match targetDirection, player.direction with
            | North, South -> Some TurnRight
            | North, West -> Some TurnRight
            | North, East -> Some TurnLeft
            | North, North -> Some MoveForward
            | South, North -> Some TurnRight
            | South, East -> Some TurnRight
            | South, West -> Some TurnLeft
            | South, South -> Some MoveForward
            | West, East -> Some TurnRight
            | West, North -> Some TurnLeft
            | West, South -> Some TurnRight
            | West, West -> Some MoveForward
            | East, West -> Some TurnRight
            | East, South -> Some TurnLeft
            | East, North -> Some TurnRight
            | East, East -> Some MoveForward
            
    let convertPathToMoves (player:Player) (path:(int*int) list) =
        let rec loop (player:Player) path cont =
            match path with
            | [] -> cont []
            | target :: rest ->
                let action = safeMove player target |> Option.get
                let player',rest' =
                    match action with
                    | TurnLeft -> 
                        let newDirection = match player.direction with | North -> West | West -> South | South -> East | East -> North 
                        {player with direction = newDirection},path
                    | TurnRight ->
                        let newDirection = match player.direction with | North -> East | East -> South | South -> West | West -> North 
                        {player with direction = newDirection},path
                    | MoveForward -> 
                        let px,py = player.currentPos
                        match player.direction with 
                        | North -> {player with currentPos = (px,py+1)},rest
                        | East -> {player with currentPos = (px+1,py)},rest
                        | South -> {player with currentPos = (px,py-1)},rest
                        | West -> {player with currentPos = (px-1,py)},rest
                    | other -> failwithf "safeMove shouldn't use any other action: %A" other
                loop player' rest' (fun actions -> action :: actions |> cont)
        loop player path (fun actions -> FollowPathTo actions)            

    let knownSpots (player:Player) (x,y) =
        [x,y+1;x+1,y;x,y-1;x-1,y]
        |> List.choose (player.memoryMap.TryFind)
    let canWeFindWumpus (player:Player) =
        if player.wumpusLocation.IsNone then
            let frontier =
                player.memoryMap
                |> Seq.map (fun (KeyValue(k,v)) -> k) // get all the existing positions we've seen
                |> Seq.filter (fun p -> match player.memoryMap.[p].isWall with Some true -> false | _ -> true) // remove all the walls
                |> Seq.collect (fun (x,y) -> [x,y+1;x+1,y;x,y-1;x-1,y] ) // generate a list of adjacent rooms from this one
                |> Seq.distinct
                |> Seq.filter (fun p -> // filter out any known locations from this list that have been visited (ie isWall is Some value)
                    match player.memoryMap.TryFind p with
                    | None -> true
                    | Some rm -> rm.isWall = None
                )
                |> Seq.toList
            let knownSpots = knownSpots player
            let wumpusSpots =
                frontier
                |> List.filter (fun p ->
                    let stenchCnt, totalCnt =
                        knownSpots p
                        |> Seq.fold (fun (sc,tc) (rm:RoomModel) ->
                            match rm.hasStench, rm.isWall with
                            | None, None -> (sc,tc)
                            | _,Some true -> (sc,tc)
                            | Some s, _ -> (if s then sc+1 else sc),(tc+1)
                            | _ -> failwithf "This should NEVER be able to happen!!"
                        ) (0,0)
                    stenchCnt = totalCnt && totalCnt > 1
                )
            match wumpusSpots with
            | [wumpusSpots] ->
                printfn "Found wumpus at %A" wumpusSpots
                { player with wumpusLocation = Some wumpusSpots }
            | [] -> player
            | moreThanOneWumpus -> failwithf "We shouldn't be able to find more than one wumpus! %A" moreThanOneWumpus
        else player
    
    
    let bestLocation (player:Player) =
        let playerx,playery = player.currentPos
        let frontier =
            player.memoryMap
            |> Seq.map (fun (KeyValue(k,v)) -> k) // get all the existing positions we've seen
            |> Seq.filter (fun p -> match player.memoryMap.[p].isWall with Some true -> false | _ -> true) // remove all the walls
            |> Seq.collect (fun (x,y) -> [x,y+1;x+1,y;x,y-1;x-1,y] ) // generate a list of adjacent rooms from this one
            |> Seq.distinct
            |> Seq.filter (fun p -> if player.wumpusLocation.IsNone then true else p <> player.wumpusLocation.Value)
            |> Seq.filter (fun p -> // filter out any known locations from this list that have been visited (ie isWall is Some value)
                match player.memoryMap.TryFind p with
                | None -> true
                | Some rm -> rm.isWall = None
            )
            |> Seq.toList
        let knownSpots = knownSpots player
        let safeSpots =
            frontier
            |> List.filter (fun p ->
                if player.wumpusLocation.IsNone then
                    let breezeCnt, stenchCnt, totalCnt =
                        knownSpots p
                        |> Seq.fold (fun (bc,sc,tc) (rm:RoomModel) ->
                            match rm.hasBreeze, rm.hasStench, rm.isWall with
                            | None, None, None -> (bc,sc,tc)
                            | _,_,Some true -> (bc,sc,tc)
                            | Some b, Some s,_ -> (if b then bc+1 else bc),(if s then sc+1 else sc),(tc+1)
                            | _ -> failwithf "This should NEVER be able to happen!!"
                        ) (0,0,0)
                    breezeCnt <> totalCnt && stenchCnt <> totalCnt
                else
                    let breezeCnt, totalCnt =
                        knownSpots p
                        |> Seq.fold (fun (bc,tc) (rm:RoomModel) ->
                            match rm.hasBreeze, rm.isWall with
                            | None, None -> (bc,tc)
                            | _,Some true -> (bc,tc)
                            | Some b,_ -> (if b then bc+1 else bc),(tc+1)
                            | _ -> failwithf "This should NEVER be able to happen!!"
                        ) (0,0)
                    breezeCnt <> totalCnt
            )
            |> List.map (fun ((x,y) as p) -> p,abs(x-playerx)+abs(y-playery))
            |> List.sortBy snd

        match safeSpots with
        | (x,distance) :: rest -> Some x
        | [] -> None              

    let rec decision (player : Player) = 
        if player.seesGlitter then PickupGold
        elif player.escape && player.currentPos = (0,0) then LeaveMap
        elif player.escape then
            greedyFirst player.memoryMap (0,0) player.currentPos
            |> List.map (fun rm -> rm.pos)
            |> convertPathToMoves player            
        elif player.hasGold then 
            Escape
        else 
            // find the next safe location to move to
            match bestLocation player with
            | None ->
                // escape since we can't progress
                Escape
            | Some target ->
                let path =
                    greedyFirst player.memoryMap target player.currentPos
                    |> List.map (fun rm -> rm.pos)
                convertPathToMoves player path
            
    let hitWall p hitWall (player:Player) =
        let rm =
            match player.memoryMap.TryFind p, hitWall with
            | None, true -> { pos = p; hasWampus = Some false; hasPit = Some false; hasBreeze = Some false; hasStench = Some false; isWall = Some true; pathBack = false }
            | None, false -> { pos = p; hasWampus = None; hasPit = None; hasBreeze = None; hasStench = None; isWall = Some false; pathBack = false }
            | Some rm, true -> { rm with isWall = Some true }
            | Some rm, false -> { rm with isWall = Some false }
        { player with memoryMap = player.memoryMap.Add(p,rm) }
    let foundWumpus p hasWumpus (player:Player) =        
        let rm =
            match player.memoryMap.TryFind p, hasWumpus with
            | None, true -> { pos = p; hasWampus = Some true; hasPit = None; hasBreeze = None; hasStench = Some true; isWall = Some false; pathBack = false }
            | None, false -> { pos = p; hasWampus = Some false; hasPit = None; hasBreeze = None; hasStench = None; isWall = Some false; pathBack = false }
            | Some rm, true -> { rm with hasWampus = Some true; hasStench = Some true }
            | Some rm, false -> { rm with hasWampus = Some false }
        { player with memoryMap = player.memoryMap.Add(p,rm) }
    let foundPit p hasPit (player:Player) =        
        let rm =
            match player.memoryMap.TryFind p, hasPit with
            | None, true -> { pos = p; hasWampus = None; hasPit = Some true; hasBreeze = Some true; hasStench = None; isWall = Some false; pathBack = false }
            | None, false -> { pos = p; hasWampus = None; hasPit = Some false; hasBreeze = None; hasStench = None; isWall = Some false; pathBack = false }
            | Some rm, true -> { rm with hasPit = Some true; hasBreeze = Some true }
            | Some rm, false -> { rm with hasPit = Some false }
        { player with memoryMap = player.memoryMap.Add(p,rm) }
    let sense  p hasBreeze hasStench (player:Player) =
        let rm =
            match player.memoryMap.TryFind p with
            | None -> { pos = p; hasWampus = Some false; hasPit = Some false; hasBreeze = Some hasBreeze; hasStench = Some hasStench; isWall = Some false; pathBack = false }
            | Some rm -> { rm with hasBreeze = Some hasBreeze; hasStench = Some hasStench}
        { player with memoryMap = player.memoryMap.Add(p,rm) }
        

    let checkPlayer (player:Player) = 
        let x,y = player.currentPos
        if x<0 then 
            printfn "Hit Western Wall "
            {player with currentPos = (0,y)}
            |> hitWall player.currentPos true
        elif y<0 then 
            printfn "Hit Southern Wall "
            {player with currentPos = (x,0)}
            |> hitWall player.currentPos true
        elif x>=player.map.GetLength 0 then 
            printfn "Hit Eastern Wall "
            {player with currentPos = ((player.map.GetLength 0)-1,y)}
            |> hitWall player.currentPos true
        elif y>=player.map.GetLength 1 then 
            printfn "Hit Northern Wall "
            {player with currentPos = (x, (player.map.GetLength 1)-1)}
            |> hitWall player.currentPos true
        elif player.map.[x,y].hasWampus then 
            printfn "The Wumpus enjoyed his snack"
            {player with dead = true}
            |> foundWumpus player.currentPos true
            |> hitWall player.currentPos false
        elif player.map.[x,y].hasPit then 
            printfn "There was a long fall"
            {player with dead = true}
            |> foundPit player.currentPos true
            |> hitWall player.currentPos false
        else
            let room = player.map.[x,y] 
            let p =
                {player with feelsBreeze = room.hasBreeze; smellsStench = room.hasStench; seesGlitter = room.hasGlitter}
                |> sense player.currentPos room.hasBreeze room.hasStench
                |> foundWumpus player.currentPos false
                |> foundPit player.currentPos false
                |> hitWall player.currentPos false
            match p.smellsStench, p.feelsBreeze, p.seesGlitter with 
            | true, true, true -> printfn "A foul stench wafts on the breeze, but something shines on the floor..."
            | true, true, false -> printfn "A foul stench wafts on the breeze..."
            | true, false, true -> printfn "A foul stench fills the room, but something shines on the floor..."
            | true, false, false -> printfn "A foul stench fills the room..."
            | false, true, true -> printfn "There is a light breeze, but something shines on the floor..."
            | false, true, false -> printfn "There is a light breeze..."
            | false, false, true -> printfn "Something shines on the floor..."
            | false, false, false -> if not p.finished then printfn "Just a dank, dark room..." else ()

            p
        |> fun p ->
            printfn "--"
            {p with map = p.map |> Array2D.map(fun r -> {r with hasChar = ((r.x,r.y) = p.currentPos); fow = r.fow && not ((r.x,r.y) = p.currentPos)})}
            |> canWeFindWumpus
        
    
    let rec updatePlayer (action:Action) (player:Player)  =
        if player.dead || player.finished then 
            player,[action]
        else
            match action with
            | Escape ->
                printfn "Escaping the cave ..."
                {player with escape = true},[action]
            | FollowPathTo (actions) ->
                let rec loop player actions cont =
                    match actions with
                    | [] -> (player,[]) |> cont
                    | action :: rest -> loop (updatePlayer action player |> fst) rest (fun (player,actions) -> (player,action::actions) |> cont)
                loop player actions id
            | LeaveMap when player.currentPos = (0,0) ->
                printfn "Climbed out the dark cave!"
                {player with finished = true},[action]  //if @ (1,1) leave
            | PickupGold when let (x,y) = player.currentPos in (player.map.[x,y].hasGold) ->  // if we are on a square that has gold
                let newMap = player.map |> Array2D.map (fun r -> {r with hasGold = false; hasGlitter = false}) //clear out gold and glitter
                printfn "Picked up some gold"
                {player with hasGold = true; map = newMap},[action]  // pickup the gold and assign the new map
            | TurnLeft -> 
                let newDirection = match player.direction with | North -> West | West -> South | South -> East | East -> North 
                printfn "Turning Left, now facing %A ..." newDirection
                {player with direction = newDirection},[action]
            | TurnRight ->
                let newDirection = match player.direction with | North -> East | East -> South | South -> West | West -> North 
                printfn "Turning Right, now facing %A ..." newDirection
                {player with direction = newDirection},[action]
            | MoveForward -> 
                let px,py = player.currentPos
                match player.direction with 
                | North -> {player with currentPos = (px,py+1)}
                | East -> {player with currentPos = (px+1,py)}
                | South -> {player with currentPos = (px,py-1)} 
                | West -> {player with currentPos = (px-1,py)}
                |> fun p ->
                    let x,y = p.currentPos
                    printfn "Walking Forward into (%d,%d) ..." x y
                    p,[action]
            | ShootArrow when player.arrows>0 ->
                printfn "Shooting an Arrow toward: %A" player.direction
                let px,py = player.currentPos
                let dx,dy=
                    match player.direction with 
                    | North -> 0,1
                    | South -> 0,-1
                    | East -> 1,0
                    | West -> -1,0
                let mutable killedWumpas = false 
                let newMap = player.map |> Array2D.map (fun room -> 
                    if room.hasWampus then 
                        let wdx,wdy = room.x-px,room.y-py
                        if (dx=wdx && wdy>=dy) || (dx=wdx && wdy<=dy) || (dx>=wdx && wdy=dy) || (dx<=wdx && wdy=dy) then //wumpas is in the path of the arrow
                            killedWumpas <- true 
                            {room with hasWampus = false}
                        else 
                            room
                    else 
                        room
                )
                if killedWumpas then 
                    printfn "A scream was heard!"
                    {player with heardScream = true; map = newMap; arrows = player.arrows-1},[action]
                else 
                    printfn "A ping was heard..."
                    {player with heardPing = true; arrows = player.arrows-1},[action]
            | _ -> failwithf "Unknown action : %A for player state : %A" action  player // if bad combination fail
            |> fun (p,a) -> checkPlayer p,a
            |> fun (p,a) ->
                printfn "%A -> %A" p.currentPos p.direction
                p,a
    let getRandNumber =
        let rand = System.Random ()
        fun max -> rand.Next (max)
  

    
    let generateMap n=                              //(row,column)
        let charLoc = (0,0)
        let wampusLoc = 
            let rec loop () =
                let w = (getRandNumber n,getRandNumber n)
                if w <> charLoc then w else loop()
            loop ()
        let goldLoc = 
            let rec loop () =
                let g = (getRandNumber n,getRandNumber n)
                if g <> wampusLoc || g <> charLoc then g else loop()
            loop ()

        let mutable pitList = []

        let isPit p =
            if p <> charLoc && p <> goldLoc then 
                if getRandNumber 5 = 0 then 
                    pitList <- p :: pitList
                    true
                else 
                    false
            else 
                false
        
        let isAdjacent (ax,ay) (bx, by) =
            (ax = bx && ay = by) 
            || (ax+1 = bx && ay = by) 
            || (ax-1 = bx && ay = by) 
            || (ax = bx && ay+1 = by) 
            || (ax = bx && ay-1 = by) 

        let isAdjacentToPit p =
            pitList|>List.exists (isAdjacent p)

                
        Array2D.init n n (fun x y -> 
            let p = (x,y)
            {
                x=x
                y=y
                fow=true
                hasChar= (p = charLoc)
                hasWampus= (p = wampusLoc)
                hasPit= isPit p
                hasGold= (p = goldLoc)
                hasBreeze=false
                hasStench= false
                hasGlitter= (p = goldLoc)
            }
        )|> Array2D.mapi(fun x y room ->
            { room with 
                hasBreeze = isAdjacentToPit (x,y)
                hasStench = isAdjacent (x,y) wampusLoc
            }
        )
    let makeTestPlayer () = 
        let charLoc = 0,0
        let wumpusLoc = 2,1
        let goldLoc = 2,2
        let pitLoc = 1,2

        let isAdjacent (ax,ay) (bx, by) =
            (ax = bx && ay = by) 
            || (ax+1 = bx && ay = by) 
            || (ax-1 = bx && ay = by) 
            || (ax = bx && ay+1 = by) 
            || (ax = bx && ay-1 = by) 

        let isAdjacentToPit p =
            isAdjacent p pitLoc

        let map = 
            Array2D.init 4 4 (fun x y-> 
                let p = x,y
                {
                    x=x
                    y=y
                    fow=true
                    hasChar= (p = charLoc)
                    hasWampus= (p = wumpusLoc)
                    hasPit= (p = pitLoc)
                    hasGold= (p = goldLoc)
                    hasBreeze=false
                    hasStench= false
                    hasGlitter= (p = goldLoc)
                }
            ) 
            |> Array2D.mapi(fun x y room ->
                { room with 
                    hasBreeze = isAdjacentToPit (x,y)
                    hasStench = isAdjacent (x,y) wumpusLoc
                }
            )
        {
            currentPos = 0,0
            direction = North
            escape = false
            hasGold = false
            dead = false
            map = map
            arrows = 1
            finished = false
            feelsBreeze = false
            smellsStench = false
            seesGlitter = false
            heardScream = false
            heardPing = false
            wumpusLocation = None
            memoryMap = Map.empty
        }
        |> checkPlayer
    type GameResult =
        | Escaped of int*Action list
        | Died of int*Action list
    let runPlayer (player:Player) =
        let rec loop (player:Player) cont =
            if player.finished || player.dead then cont (player.hasGold,player.dead,[])
            else
                let action = decision player
                let newPlayer,actions = updatePlayer action player
                match actions with
                | [action] ->
                    loop newPlayer (fun (hasGold,dead,actions) -> (hasGold,dead,action :: actions) |> cont)
                | action ->
                    loop newPlayer (fun (hasGold,dead,actions) -> (hasGold,dead,action @ actions) |> cont)
        let hasGold,isDead,actions = loop player id
        let actionScore = actions |> Seq.map (function | TurnRight | TurnLeft -> 0 | MoveForward -> -1 | PickupGold -> 0 | ShootArrow -> -10 | LeaveMap -> 0 | FollowPathTo(_) -> 0 | Escape -> 0) |> Seq.sum
        if isDead then
            Died(actionScore-1000,actions)
        else
            Escaped(actionScore+(if hasGold then 1000 else 0),actions)

    let newPlayerOfMapSize n =
        let map = generateMap n
        {
            currentPos = 0,0
            direction = North
            escape = false
            hasGold = false
            dead = false
            map = map
            arrows = 1
            finished = false
            feelsBreeze = false
            smellsStench = false
            seesGlitter = false
            heardScream = false
            heardPing = false
            wumpusLocation = None
            memoryMap = Map.empty
        }
        |> checkPlayer

    let newPlayerFromMap (map:Room[,]) =
        {
            currentPos = 0,0
            direction = North
            escape = false
            hasGold = false
            dead = false
            map = map
            arrows = 1
            finished = false
            feelsBreeze = false
            smellsStench = false
            seesGlitter = false
            heardScream = false
            heardPing = false
            wumpusLocation = None
            memoryMap = Map.empty
        }
        |> checkPlayer        

    let printMap (map:Room[,]) =
        for y = (map.GetLength(1)-1) downto 0 do
            printf "%02d" y
            for x = 0 to (map.GetLength(0)-1) do
                let rm = map.[x,y]
                let content =
                    let G = if rm.hasGold then 'G' else ' '
                    match rm.hasPit,rm.hasWampus,rm.hasChar with
                    | true,true,true -> sprintf "PWC%c" G
                    | true,true,false -> sprintf " PW%c" G
                    | true,false,true -> sprintf " PC%c" G
                    | true,false,false -> sprintf " P%c " G
                    | false,true,true -> sprintf " WC%c" G
                    | false,true,false -> sprintf " W%c " G
                    | _ ->       
                        if rm.hasChar then
                            match rm.hasBreeze,rm.hasStench with
                            | true, true -> sprintf "bCs%c" G
                            | true, false -> sprintf " bC%c" G
                            | false, true -> sprintf " Cs%c" G
                            | false, false -> sprintf " C %c" G
                        elif rm.hasGold then
                            match rm.hasBreeze,rm.hasStench with
                            | true, true -> " bGs"
                            | true, false -> " bG "
                            | false, true -> " Gs "
                            | false, false -> "  G "
                        else
                            match rm.hasBreeze,rm.hasStench with
                            | true, true -> " bs "
                            | true, false -> " b  "
                            | false, true -> " s  "
                            | false, false -> "    "
                printf "|%s" content
            printfn "|"
        printf "  "
        for x = 0 to (map.GetLength(0)-1) do
            printf " %02d  " x
        printfn ""
    let sprintMap (map:Room[,]) =
        seq {
            yield ""
            for y = (map.GetLength(1)-1) downto 0 do
                yield
                    seq {
                        yield sprintf "%02d" y
                        for x = 0 to (map.GetLength(0)-1) do
                            let rm = map.[x,y]
                            let content =
                                let G = if rm.hasGold then 'G' else ' '
                                match rm.hasPit,rm.hasWampus,rm.hasChar with
                                | true,true,true -> sprintf "PWC%c" G
                                | true,true,false -> sprintf " PW%c" G
                                | true,false,true -> sprintf " PC%c" G
                                | true,false,false -> sprintf " P%c " G
                                | false,true,true -> sprintf " WC%c" G
                                | false,true,false -> sprintf " W%c " G
                                | _ ->       
                                    if rm.hasChar then
                                        match rm.hasBreeze,rm.hasStench with
                                        | true, true -> sprintf "bCs%c" G
                                        | true, false -> sprintf " bC%c" G
                                        | false, true -> sprintf " Cs%c" G
                                        | false, false -> sprintf " C %c" G
                                    elif rm.hasGold then
                                        match rm.hasBreeze,rm.hasStench with
                                        | true, true -> " bGs"
                                        | true, false -> " bG "
                                        | false, true -> " Gs "
                                        | false, false -> "  G "
                                    else
                                        match rm.hasBreeze,rm.hasStench with
                                        | true, true -> " bs "
                                        | true, false -> " b  "
                                        | false, true -> " s  "
                                        | false, false -> "    "
                            yield sprintf "|%s" content
                        yield "|"
                    } |> String.concat ""
            yield
                seq {
                    yield "  "
                    for x = 0 to (map.GetLength(0)-1) do
                        yield sprintf " %02d  " x
                } |> String.concat ""
            yield ""
        } |> String.concat "\n"
    let printPlayer (player:Player) =
        printMap player.map

    let serializeMap (map:Room[,]) =
        use ms = new System.IO.MemoryStream();
        use bw = new System.IO.BinaryWriter(ms);
        Seq.init (map.GetLength(1)) (fun y ->
            Seq.init (map.GetLength(0)) (fun x ->
                map.[x,y]
            )
        )
        |> Seq.collect id
        |> Seq.iter (fun r ->
            bw.Write(1uy)
            r.Write bw
        )
        bw.Write(0uy)
        bw.Flush()
        bw.Close()
        let ba = ms.ToArray()
        System.Convert.ToBase64String ba

    let deserializeMap (mapTxt:string) =
        let ba = System.Convert.FromBase64String mapTxt
        use ms = new System.IO.MemoryStream(ba);
        use br = new System.IO.BinaryReader(ms);
        let rec loop maxN (rooms:Map<_,_>) =
            if br.ReadByte() = 0uy then
                br.Close()
                maxN,rooms
            else
                let room = Room.Read br
                let maxN' = max (max maxN room.x) room.y
                loop maxN' (Map.add (room.x,room.y) room rooms)
        let n,rooms = loop 0 Map.empty

        Array2D.init (n+1) (n+1) (fun x y ->
            let p = (x,y)
            rooms.[p]
        )

    let saveAsInt (r:Room) =
        use ms = new System.IO.MemoryStream()
        use bw = new System.IO.BinaryWriter(ms)
        bw.Write(1uy)
        r.Write bw
        bw.Flush()
        bw.Close()
        let ba = ms.ToArray()
        let asInt = System.BitConverter.ToInt32(ba,0)
        asInt,ba

    let readFromInt (i:int) =
        use ms = new System.IO.MemoryStream(System.BitConverter.GetBytes(i))
        use br = new System.IO.BinaryReader(ms)
        try
            br.ReadByte() |> ignore
            Room.Read br
        finally
            br.Close()

    let makeSeqOfArray2D (arr:'a [,]) =
        seq {
            for y = 0 to arr.GetLength(1)-1 do
                yield!                
                    seq {
                        for x = 0 to arr.GetLength(0)-1 do
                            yield arr.[x,y]
                    }
        }

    let computeAverageScoreForMaps (maps:Room[,] seq) =
        let tests =
            maps
            |> Seq.map newPlayerFromMap
            |> Seq.map (fun player ->
                try
                    runPlayer player |> Result.Ok
                with
                    | e -> Result.Error(e,player.map)
            )
            |> Seq.toList
        let errors = tests |> List.choose (function | Result.Error(e,map) -> Some (e.Message, serializeMap map) | _ -> None)
        let avgScore =
            tests
            |> Seq.choose (function | Result.Ok (Died(score,_)) -> score |> float |> Some | Result.Ok (Escaped(score,_)) -> score |> float |> Some | _ -> None)
            |> Seq.average
        avgScore,errors

    let computeAverageScoreForSize count size =
        Seq.init count (fun _ -> generateMap size)
        |> computeAverageScoreForMaps
            
    let test () = 

        (*
        Example 4x4
        3|   | b |   |   |
        2| b | P |bGg|   |
        1|   | bs| W | s |
        0|   |   | s |   |
           0   1   2   3
        P = Pit, W = Wumpus, G = Gold, b = breeze, s = stench, g = glitter
        *)

        //let player = makeTestPlayer () 

        printfn "Test Map:"
        makeTestPlayer()
        |> runPlayer
        |> printfn "%A"

        printfn "Random 5x5 Map:"
        newPlayerOfMapSize 5
        |> runPlayer
        |> printfn "%A"

        printfn "Random 10x10 Map:"
        newPlayerOfMapSize 10
        |> runPlayer
        |> printfn "%A"
        //let showPathsTo p (player:Player) =
        //    greedyFirst player.memoryMap p player.currentPos
        

    // fsi.AddPrinter (fun (rooms: Utility.Room[,])-> Utility.sprintMap rooms )

module Test =
    open Utility

    let map = Utility.generateMap 5
    map.[0,0] <- { map.[0,0] with hasBreeze = false }
    map.[0,1] <- { map.[0,1] with hasPit = false; hasBreeze = true }
    
    map
    |> newPlayerFromMap
    |> runPlayer

    let resultsTest4x4 = Utility.computeAverageScoreForSize 100 4
    let resultsTest5x5 = Utility.computeAverageScoreForSize 100 5
    let resultsTest6x6 = Utility.computeAverageScoreForSize 100 6
    let resultsTest7x7 = Utility.computeAverageScoreForSize 100 7
    let resultsTest8x8 = Utility.computeAverageScoreForSize 100 8
    let resultsTest9x9 = Utility.computeAverageScoreForSize 100 9
    let resultsTest10x10 = Utility.computeAverageScoreForSize 100 10


    
