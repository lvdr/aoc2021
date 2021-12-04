def stringAsInt (β : String) : Int :=
    let βₙ := String.toInt? β 
    match βₙ with
        | some γ => γ 
        | none   => 0

def lineAsCoords (α : String) : Int × Int :=
    let αₛ  := α.splitOn " "
    match αₛ with
    | [θ, Δ] =>
      let Δ₁ := stringAsInt Δ
      match θ with
      | "forward" => (Δ₁, 0)
      | "down"    => (0, Δ₁)
      | "up"      => (0, -Δ₁)
      | _         => (-1, -1)
    | _ => (0, 0)

def parse (α : String) : List (Int × Int) :=
    let αₛ := α.splitOn "\n"
    αₛ.map lineAsCoords

def add (α β : Int × Int) : Int × Int :=
let (α₁, α₂) := α
let (β₁, β₂) := β
(α₁ + β₁, α₂ + β₂)

def reduceAdd (α : List (Int × Int)) : Int × Int :=
match α with
| (Δx, Δy)::ω => add (Δx, Δy) (reduceAdd ω)
| []          => (0, 0)

def reduceMove (α : Int) (β : List (Int × Int)) : Int × Int :=
match β with
| (Δx, 0)::ω => add (Δx, α * Δx) (reduceMove α ω)
| (0, Δα)::ω => reduceMove (α + Δα) ω
| (_, _)::ω   => (-1, -1)
| _           => (0, 0)

def main (α : List String) : IO Unit := do
    let p := match α with
             | []      => "day02/input.in"
             | p₁::pₙ  => p₁
    let ι ← IO.FS.readFile p
    let (β₁, γ₁) := reduceAdd (parse ι)
    let (β₂, γ₂) := reduceMove 0 (parse ι)
    IO.println s!"{β₁} {γ₁} {β₁ * γ₁}"
    IO.println s!"{β₂} {γ₂} {β₁ * γ₂}"
