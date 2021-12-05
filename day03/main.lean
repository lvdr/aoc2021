def innerAppend (α₁ : List α) (αₙ : List (List α)) : List (List α) :=
    match α₁, αₙ with
    | [ω₁], [ωₙ]       => [ω₁::ωₙ]
    | α₁₁::ω₁, αₙ₁::ωₙ => (α₁₁::αₙ₁)::(innerAppend ω₁ ωₙ)
    | _,  []           => [α₁]
    | [], _            => []

def simpleTranspose : List α → List (List α)
    | []     => []
    | α₁::ω  => [α₁]::(simpleTranspose ω)

def transpose : List (List α) → List (List α)
    | []    => []
    | [ω]   => simpleTranspose ω
    | α₁::ω => innerAppend α₁ (transpose ω)

def stringToBits (α : List Char) : List Nat :=
    match α with
    | α₁::ω =>
        (match α₁ with
         | '1' => 1
         |  _  => 0)::(stringToBits ω)
    | []  => []
    
def strip : String → Option String
    | "" => none
    | α  => some α

def parse (α : String) : List (List Nat) :=
    let αₛ := α.splitOn "\n"
    transpose (((αₛ.filterMap strip).map String.toList).map stringToBits)

def countBits : List Nat -> Nat
    | []    => 0
    | β₁::ω => β₁ + (countBits ω)

def len : (List α) → Nat
    | []    => 0
    | α₁::ω => 1 + len ω

def selectBit (α : Nat) (β : Nat) : Nat :=
    if 2*β >= α then 1 else 0

def dominantBit (α : List Nat) : Nat :=
    selectBit (len α) (countBits α)

def negate : Nat → Nat
    | 1 => 0
    | _ => 1

def recessiveBit (α : List Nat) : Nat :=
    let l := len α
    let c := countBits α
    let b := selectBit l c
    if c == l ∨ c == 0 then b else negate b

def bitsToNat : List Nat → Nat
    | 1::ω => 1 + 2*(bitsToNat ω)
    | _::ω => 2*(bitsToNat ω)
    | []   => 0

def maskBits (s : Nat) (α : List Nat) (β : List Nat) : List Nat :=
    match α, β with
    | [], _ => []
    | _, [] => []
    | α₁::ω₁, β₁::ω₂ => if α₁ == s then
                            β₁::(maskBits s ω₁ ω₂)
                        else
                            maskBits s ω₁ ω₂

partial def selectDominantBit : List (List Nat) -> List Nat
    | []     => []
    | α₁::ω  => let s := dominantBit α₁
                s::(selectDominantBit (ω.map (maskBits s α₁)))

partial def selectRecessiveBit : List (List Nat) -> List Nat
    | []     => []
    | α₁::ω  => let s := recessiveBit α₁
                s::(selectRecessiveBit (ω.map (maskBits s α₁)))

def main (α : List String) : IO Unit := do
    let p := match α with
             | []      => "day03/input.in"
             | p₁::pₙ  => p₁
    let ι ← IO.FS.readFile p
    let β   := parse ι
    let γ   := β.map dominantBit
    let γ₁₀ := bitsToNat γ.reverse
    let ε₁₀ := bitsToNat (γ.map negate).reverse
    let Ο₁₀ := bitsToNat (selectDominantBit β).reverse
    let Α₁₀ := bitsToNat (selectRecessiveBit β).reverse
    IO.println s!"{γ₁₀} {ε₁₀} {γ₁₀ * ε₁₀}"
    IO.println s!"{Ο₁₀} {Α₁₀} {Ο₁₀ * Α₁₀}"
