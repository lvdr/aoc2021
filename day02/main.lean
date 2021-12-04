inductive Direction where
  | forward
  | down
  | up
  deriving Repr

instance : ToString Direction where toString p :=
    match p with
    | Direction.forward => "forward"
    | Direction.down    => "down"
    | Direction.up      => "up"

def stringAsNat (β : String) : Nat :=
    let βₙ := String.toNat? β 
    match βₙ with
        | some γ => γ 
        | none   => 0

def lineAsCommands (α : String) : Direction × Nat :=
    let αₛ  := α.splitOn " "
    match αₛ with
    | [θ, Δ] =>
      let Δ₁ := stringAsNat Δ
      let θ₁ := match θ with
      | "forward" => Direction.forward
      | "down"    => Direction.down
      | "up"      => Direction.up
      | _         => Direction.forward
      (θ₁, Δ₁)
    | _ => (Direction.forward, 0)

def parse (α : String) : List (Direction × Nat) :=
    let αₛ := α.splitOn "\n"
    αₛ.map lineAsCommands

def add (α β : Int × Int) : Int × Int :=
    let (α₁, α₂) := α
    let (β₁, β₂) := β
    (α₁ + β₁, α₂ + β₂)

def reduceAdd (α : List (Direction × Nat)) : Int × Int :=
    match α with
    | α₁::ω =>
        match α₁ with
        | (Direction.forward, Δx) => add (Δx, 0)  (reduceAdd ω)
        | (Direction.down, Δy)    => add (0, Δy)  (reduceAdd ω)
        | (Direction.up, Δy)      => add (0, -Δy) (reduceAdd ω)
    | []                          => (0, 0)

def reduceMove (α : Int) (β : List (Direction × Nat)) : Int × Int :=
    match β with
    | β₁::ω =>
        match β₁ with
        | (Direction.forward, Δx) => add (Δx, α * Δx) (reduceMove α ω)
        | (Direction.down, Δα)    => reduceMove (α + Δα) ω
        | (Direction.up, Δα)      => reduceMove (α - Δα) ω
    | []                          => (0, 0)

def main (α : List String) : IO Unit := do
    let p := match α with
             | []      => "day02/input.in"
             | p₁::pₙ  => p₁
    let ι ← IO.FS.readFile p
    let (β₁, γ₁) := reduceAdd (parse ι)
    let (β₂, γ₂) := reduceMove 0 (parse ι)
    IO.println s!"{β₁} {γ₁} {β₁ * γ₁}"
    IO.println s!"{β₂} {γ₂} {β₁ * γ₂}"
