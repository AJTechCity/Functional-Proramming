class Atom:
    def __init__(self): pass

class Beep(Atom):
    def __init__(self): pass
    def __str__(self): return "Beep"
      
class Silence(Atom):
    def __init__(self): pass
    def __str__(self): return "Silence"

class Code():
    def __init__(self, *atoms):
        self.data = [atoms]
    def __str__(self):
        return self.data
    
dit = Code(Beep(), Silence())
dah = Code(Beep(), Beep(), Beep(), Silence())
shortGap = Code(Silence(), Silence())
mediumGap = Code(Silence(), Silence(), Silence(), Silence(), Silence(), Silence())

morseCode = {
    'A': Code(dit, dah),
    'B': Code(dah, dit, dit, dit),
    'C': Code(dah, dit, dah, dit),
    'D': Code(dah, dit, dit),
    'E': Code(dit),
    'F': Code(dit, dit, dah, dit),
    'G': Code(dah, dah, dit),
    'H': Code(dit, dit, dit, dit),
    'I': Code(dit, dit),
    'J': Code(dit, dah, dah, dah),
    'K': Code(dah, dit, dah),
    'L': Code(dit, dah, dit, dit),
    'M': Code(dah, dah),
    'N': Code(dah, dit),
    'O': Code(dah, dah, dah),
    'P': Code(dit, dah, dah, dit),
    'Q': Code(dah, dah, dit, dah),
    'R': Code(dit, dah, dit),
    'S': Code(dit, dit, dit),
    'T': Code(dah),
    'U': Code(dit, dit, dah),
    'V': Code(dit, dit, dit, dah),
    'W': Code( dit, dit, dit, dah),
    'X': Code(dah, dit, dit, dah),
    'Y': Code(dah, dit, dah, dah),
    'Z': Code(dah, dah, dit, dit),
}

class Tree:
    def __init__(self, value, left, right):
        self.value = value
        self.left = left
        self.right = right
    def __str__(self):
        return self.value
    

print(morseCode['A'])