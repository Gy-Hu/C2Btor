import kratos.expr

# verification results
#: error verification result
RESULT_ERROR = 0 
#: unknown verification result
RESULT_UNKNOWN = 1 
#: safe verification result
RESULT_SAFE = 2 
#: unsafe verification result
RESULT_UNSAFE = 3 

# verification methods
#: model checking verification method
VERIFICATION_MODEL_CHECKING = 0 
#: symbolic execution verification method
VERIFICATION_SYMBOLIC_EXECUTION = 1 
#: simulation verification method
VERIFICATION_SIMULATION = 2 


class VerificationResult(object):
    """
    A Kratos verification result.
    Attributes:
    - status: the verification status (`RESULT_UNKNOWN`, `RESULT_SAFE`,
              or `RESULT_UNSAFE`)
    - trace: the counterexample trace (can be None)
    """
    def __init__(self, status, trace=None):
        self.status = status
        self.trace = trace

    def __eq__(self, other):
        if isinstance(other, int):
            return self.status == other
        raise TypeError(f'unsupported comparison between VerificationResult '
                        f'and {type(other)}')

    def __str__(self):
        return {
            RESULT_UNKNOWN : 'unknown',
            RESULT_SAFE : 'safe',
            RESULT_UNSAFE : 'unsafe',
        }.get(self.status, 'ERROR')

# end of class VerificationResult


def get_version():
    """
    Get the Kratos version information as a string.
    """
    raise NotImplementedError

def encode(config, program):
    """
    Encode a `Program` into a symbolic transition system.
    Parameters:
    - config: a dictionary with the configuration settings to use
              for the encoding
    - program: the program to encode
    Return:
    - a string representation of the symbolic transition system
    """
    raise NotImplementedError

def verify(method, config, program):
    """
    Perform verification of the given Kratos program.
    Parameters:
    - method: the verification method to use (`VERIFICATION_MODEL_CHECKING`,
             `VERIFICATION_SYMBOLIC_EXECUTION`, or `VERIFICATION_SIMULATION`)
    - config: a dictionary with the configuration settings to use for
              the verification
    - program: the program to verify
    Return:
    - a `VerificationResult`
    """
    raise NotImplementedError

try:
    import kratos._pykratos as _pykratos

    get_version = _pykratos.get_version
    encode = _pykratos.encode

    def verify(method, config, program):
        status, trace = _pykratos.verify(kratos.expr, method, config, program)
        return VerificationResult(status, trace)
    
except ImportError:
    pass
