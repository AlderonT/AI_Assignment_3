module Utility

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
        
    type Room = 
        {
            x : int
            y : int
            fow : bool
            hasChar : bool
            hasWampus : bool
            hasPit : bool
            hasGold : bool
            hasBreaze : bool
            hasStench : bool
            hasGlitter : bool
        }

    type Player = 
        {
            currentPos : (int*int)
            direction : Direction 
            hasGold : bool 
            dead : bool
            map : Room[,]
            arrows : int
            finished : bool
            feelsBreaze : bool 
            smellsStench : bool 
            seesGlitter : bool 
            heardScream : bool 
            heardPing : bool
        }
    type Action =
        | MoveForward
        | TurnRight 
        | TurnLeft 
        | ShootArrow
        | PickupGold
        | LeaveMap
    
    let checkPlayer (player:Player) = 
        let x,y = player.currentPos
        if x<0 then 
            printfn "Hit Western Wall "
            {player with currentPos = (0,y)}
        elif y<0 then 
            printfn "Hit Southern Wall "
            {player with currentPos = (x,0)}
        elif x>=player.map.GetLength 0 then 
            printfn "Hit Eastern Wall "
            {player with currentPos = ((player.map.GetLength 0)-1,y)}
        elif y>=player.map.GetLength 1 then 
            printfn "Hit Northern Wall "
            {player with currentPos = (x, (player.map.GetLength 1)-1)}
        elif player.map.[x,y].hasWampus then 
            printfn "The Wumpus enjoyed his snack"
            {player with dead = true}
        elif player.map.[x,y].hasPit then 
            printfn "There was a long fall"
            {player with dead = true}
        else
            let room = player.map.[x,y] 
            let p = {player with feelsBreaze = room.hasBreaze; smellsStench = room.hasStench; seesGlitter = room.hasGlitter}
            match p.smellsStench, p.feelsBreaze, p.seesGlitter with 
            | true, true, true -> printfn "A foul stench wafts on the breaze, but something shines on the floor..."
            | true, true, false -> printfn "A foul stench wafts on the breaze..."
            | true, false, true -> printfn "A foul stench fills the room, but something shines on the floor..."
            | true, false, false -> printfn "A foul stench fills the room..."
            | false, true, true -> printfn "There is a light breaze, but something shines on the floor..."
            | false, true, false -> printfn "There is a light breaze..."
            | false, false, true -> printfn "Something shines on the floor..."
            | false, false, false -> if not p.finished then printfn "Just a dank, dark room..." else ()

            p
        |> fun p ->
            printfn "--"
            {p with map = p.map |> Array2D.map(fun r -> {r with hasChar = ((r.x,r.y) = p.currentPos); fow = r.fow && not ((r.x,r.y) = p.currentPos)})}
        
    
    let updatePlayer (action:Action) (player:Player)  =
        if player.dead || player.finished then 
            player
        else
            match action with
            | LeaveMap when player.currentPos = (0,0) -> printfn "Climbed out the dark cave!"; {player with finished = true}  //if @ (1,1) leave
            | PickupGold when let (x,y) = player.currentPos in (player.map.[x,y].hasGold) ->  // if we are on a square that has gold
                let newMap = player.map |> Array2D.map (fun r -> {r with hasGold = false; hasGlitter = false}) //clear out gold and glitter
                printfn "Picked up some gold"
                {player with hasGold = true; map = newMap}  // pickup the gold and assign the new map
            | TurnLeft -> 
                let newDirection = match player.direction with | North -> West | West -> South | South -> East | East -> North 
                {player with direction = newDirection}
            | TurnRight ->
                let newDirection = match player.direction with | North -> East | East -> South | South -> West | West -> North 
                {player with direction = newDirection}
            | MoveForward -> 
                let px,py = player.currentPos
                match player.direction with 
                | North -> {player with currentPos = (px,py+1)}
                | East -> {player with currentPos = (px+1,py)}
                | South -> {player with currentPos = (px,py-1)} 
                | West -> {player with currentPos = (px-1,py)}
            | ShootArrow when player.arrows>0 ->
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
                    {player with heardScream = true; map = newMap; arrows = player.arrows-1}
                else 
                    printfn "A ping was heard..."
                    {player with heardPing = true; arrows = player.arrows-1}
            | _ -> failwithf "Unknown action : %A for player state : %A" action  player // if bad combination fail
            |> checkPlayer 
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
                hasBreaze=false
                hasStench= false
                hasGlitter= (p = goldLoc)
            }
        )|> Array2D.mapi(fun x y room ->
            { room with 
                hasBreaze = isAdjacentToPit (x,y)
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
                    hasBreaze=false
                    hasStench= false
                    hasGlitter= (p = goldLoc)
                }
            ) 
            |> Array2D.mapi(fun x y room ->
                { room with 
                    hasBreaze = isAdjacentToPit (x,y)
                    hasStench = isAdjacent (x,y) wumpusLoc
                }
            )
        {
            currentPos = 0,0
            direction = North
            hasGold = false
            dead = false
            map = map
            arrows = 1
            finished = false
            feelsBreaze = false
            smellsStench = false
            seesGlitter = false
            heardScream = false
            heardPing = false
        }
        |> checkPlayer
    // fsi.AddPrinter (fun (rooms: Room[,])-> "Some rooms" )
    let test () = 
    
        makeTestPlayer () 
        |> updatePlayer TurnRight
        |> updatePlayer MoveForward 
        |> updatePlayer TurnLeft
        |> updatePlayer MoveForward 
        |> updatePlayer TurnRight
        |> updatePlayer ShootArrow
        |> updatePlayer MoveForward 
        |> updatePlayer TurnLeft
        |> updatePlayer MoveForward 
        |> updatePlayer PickupGold
        |> updatePlayer TurnLeft
        |> updatePlayer TurnLeft
        |> updatePlayer MoveForward 
        |> updatePlayer MoveForward 
        |> updatePlayer TurnRight
        |> updatePlayer MoveForward 
        |> updatePlayer MoveForward 
        |> updatePlayer LeaveMap
