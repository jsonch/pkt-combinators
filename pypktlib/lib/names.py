from typing import TypeVar, Generic, get_type_hints
# names of variables, types, and locations
# Variable names
class VarMaker(type):
    def __getattr__(cls, attr):
        return attr
class Var(metaclass = VarMaker):
    """Arbitrary variables. Lets you write Var.x 
       and have it eval to the string "x" """
    def __init__(self):
        pass

# Location names
class LocMaker(type):
    def __getitem__(cls, key):
        return key
class Core(metaclass=LocMaker):
    @staticmethod
    def to_ir(core):
        return f"{core}"


