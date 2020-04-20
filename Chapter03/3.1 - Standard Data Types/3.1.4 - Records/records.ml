type ptype =
  | TFire
  | TNormal
  | TWater
  | TEarh

type  mon =
  { name : string;
    hp : int;
    ptype : ptype }

let charmander = { name = "Charmander"; hp = 39; ptype = TFire }

let hp = function
  | {name=n; hp=h; ptype=t} -> h

let hp' = function
  | {name; hp; ptype} -> hp
