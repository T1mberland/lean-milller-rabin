import Rabin

def _miller_rabin_find_sd (m : Nat) : (Nat × Nat) :=
  if m = 0 then
    (0, 0)
  else if m % 2 = 0 then
    let k := m / 2;
    let (s, d) := _miller_rabin_find_sd k
    (s+1, d)
  else
    (0, m)

-- Returns True if composite
def _miller_rabin_sub (n : Nat) (x : Nat) (s : Nat) : Bool :=
  if s > 0 then
    let y := (x^2) % n;
    if y = 1 && x != 1 && x != n-1 then
      True
    else
      _miller_rabin_sub n y (s-1)
  else
    x != 1

def power_mod (base : Nat) (expo : Nat) (modulo : Nat) : Nat :=
  if expo > 0 then
    let next := power_mod base (expo/2) modulo;
    let a := expo % 2;
    let result := (next^(2+a)) % modulo;
    result
  else
    base % modulo

-- Returns True if composite
def miller_rabin_single (n : Nat) (a : Nat) : Bool :=
  let (s,d) := _miller_rabin_find_sd (n-1)
  let x := power_mod a d n
  _miller_rabin_sub n x s

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"
