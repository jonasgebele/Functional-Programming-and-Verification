type quadtree_node = 
  NoPoint
  | Point of int * int
  | QNode of  
              quadtree_node  (* bottom left *)
            * quadtree_node  (* top left *)
            * quadtree_node  (* bottom right *)
            * quadtree_node  (* top right *)

type quadtree = { width:int; height:int; root:quadtree_node }

let rect = {width = 512; height = 512; root = NoPoint};;

let rec insert_node : int * int -> int -> int -> quadtree_node -> quadtree_node =
  fun (x, y) h w rect ->
    match rect
      with NoPoint -> Point(x, y)
      | Point(a, b) -> if (a = x && b = y) then Point(a, b) else
        if (a <= (w/2) && b <= (h/2))       then insert_node (x, y) h w (QNode (Point(a,b), NoPoint, NoPoint, NoPoint)) 
          else if (x <= (w/2) && y > (h/2)) then insert_node (x, y) h w (QNode (NoPoint, Point(a,b), NoPoint, NoPoint)) 
          else if (x > (w/2) && y <= (h/2)) then insert_node (x, y) h w (QNode (NoPoint, NoPoint, Point(a,b), NoPoint)) 
          else                                   insert_node (x, y) h w (QNode (NoPoint, NoPoint, NoPoint, Point(a,b)))
      | QNode(NoPoint, NoPoint, NoPoint,_) ->  NoPoint 
      | QNode(NoPoint, NoPoint, _, NoPoint) ->  NoPoint 
      | QNode(NoPoint, _, NoPoint, NoPoint) ->  NoPoint 
      | QNode(_, NoPoint, NoPoint, NoPoint) ->  NoPoint 
;;

let insert : int * int -> quadtree -> quadtree =
  fun (x, y) rect ->
    match rect.root
      with NoPoint ->  {width = rect.width; height = rect.height; root = (Point(x, y)) }
      | Point(_,_) ->  {width = rect.width; height = rect.height; root = (Point(x, y)) }
      | _          ->  {width = rect.width; height = rect.height; root = (Point(x, y)) }
;;