# LL(k) Parser Combinators for finding VCs of a simple LPA


def empty(j):
    return {j}




class attrib:
    def __init__(self):
        self.lhs = ""
        self.rhs = ""
    def wpc(self):
        self.lhs = ""
        self.rhs = ""
    def spc(self):
        self.lhs = ""
        self.lhs = ""

class skip:
    pass
class havoc:
    pass
class assume:
    pass
class force:
    pass
class choice:
    pass
class composition:
    pass

#[skip; assume phi; force psi]
#formul = parse(composition(skip, assume(phi), force(psi))
#formul = parse("skip ; assume phi ; force psi")
"""
=> formul = "True and (" + parse(composition(assume(phi), force(psi)) + ")"
=> formul = "True and (" + "psi implies (" + parse(composition(force(psi))) + ")" + ")"
=> formul = "True and (psi implies (" + parse(force(psi)) + "))"
=> formul = "True and (psi implies (psi))"
"""
