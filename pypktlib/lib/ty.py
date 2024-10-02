# User defined types for surface syntax

from typing import TypeVar, Generic, get_type_hints
import syntax
# class TyMaker(type):
#     def __getattr__(cls, attr):
#         # Define a custom __repr__ method
#         def __repr__(self):
#             return f"{cls.__name__}.{attr}"
#         def to_ir(self):
#             return syntax.TyName(f"{attr}")
        
#         # Dynamically create and return a new type with the given name and custom __repr__
#         new_type = type(f"{cls.__name__}.{attr}", (object,), {"__repr__": __repr__, "to_ir":to_ir})
#         return new_type()

# class Ty(metaclass=TyMaker):
#     """Arbitrary named types. Lets you write Ty.x
#        and have it resolve to the string "x" """
#     pass

class UserTy():
    def __init__(self, name, cstr=None):
        self.name = name
        self.cstr = cstr
    def to_ir(self):
        return syntax.TyName(name=self.name, cstr = self.cstr)

class AtomState():
    """Type of a stateful object with constructor function"""
    def __init__(self, ty, initname=None, init=None, initargs = None):
        self.ty = ty 
        self.initname = initname # name of initializer function
        self.init = init # implementation of initializer function in c
        self.initargs = initargs
    def __call__(self, *args):
        """bind the initialization arguments for the constructor"""
        return AtomState(self.ty, self.initname, self.init, args)
        

    def to_ir(self):
        """Return a named type in the ir"""
        return self.ty.to_ir()

# pointer type
T = TypeVar('T')
class Ptr(Generic[T]):
    def __init__(self, type_arg=None):
        self._type_arg = type_arg
    @classmethod
    def __class_getitem__(cls, item):
        return cls(type_arg=item)
    def to_ir(self):
        type_var = self._type_arg
        ir_type_var = type_var.to_ir()
        return syntax.Ptr(ir_type_var)

ref = Ptr
