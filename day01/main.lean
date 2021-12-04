def stringAsNat (α : Nat) (β : String) : Nat :=
    let βₙ := String.toNat? β 
    match βₙ with
        | some γ => γ 
        | none   => α 

def parse (α : String) : List Nat :=
    let αₛ := α.splitOn "\n"
    αₛ.map (stringAsNat 0)

def increases : List Nat → Nat
| α::β::ω => (if β > α then 1 else 0) + increases (β::ω)
| _       => 0

def window : List Nat → List Nat
| α::β::γ::ω => (α + β + γ)::(window (β::γ::ω))
| _          => []

def main (α : List String) : IO Unit := do
    let p := match α with
    | []      => "day01/input.in"
    | p₁::pₙ  => p₁
    let ι ← IO.FS.readFile p
    IO.println (increases (parse ι))
    IO.println (increases (window (parse ι)))
