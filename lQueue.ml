type 'a t = { l : 'a list; r : 'a list }

let empty = { l = []; r = [] }

let queue = function
  | { l = []; r } -> { l = List.rev r; r = [] }
  | q -> q

let push { l; r } x = queue { l; r = x :: r }
