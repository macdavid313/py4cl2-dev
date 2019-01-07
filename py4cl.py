import sys

try:
    from io import StringIO # Python 3
except:
    from StringIO import StringIO


# Direct stdout to a StringIO buffer,
# to prevent commands from printing to the output stream

write_stream = sys.stdout
redirect_stream = StringIO()

sys.stdout = redirect_stream

class Symbol:
    """
    A wrapper around a string, representing a Lisp symbol. 
    """
    def __init__(self, name):
        self._name = name
    def __str__(self):
        return self._name
    def __repr__(self):
        return "Symbol("+self._name+")"

##################################################################
# This code adapted from cl4py
#
# https://github.com/marcoheisig/cl4py
#
# Copyright (c) 2018  Marco Heisig <marco.heisig@fau.de>

def lispify(obj):
    return lispify_aux(obj)

def lispify_aux(obj):
    return lispifiers.get(type(obj), lambda x: "NIL")(obj)

lispifiers = {
    bool       : lambda x: "T" if x else "NIL",
    type(None) : lambda x: "NIL",
    int        : lambda x: str(x),
    float      : lambda x: str(x),
    complex    : lambda x: "#C(" + lispify_aux(x.real) + " " + lispify_aux(x.imag) + ")",
    list       : lambda x: "#(" + " ".join(lispify_aux(elt) for elt in x) + ")",
    tuple      : lambda x: "(" + " ".join(lispify_aux(elt) for elt in x) + ")",
    dict       : lambda x: "#.(let ((table (make-hash-table))) " + " ".join("(setf (gethash {} table) {})".format(key, value) for key, value in x.items()) + " table)",
    str        : lambda x: "\"" + x.replace("\\", "\\\\").replace('"', '\\"')  + "\"",
    Symbol     : lambda x: str(x)
}

##################################################################

eval_globals = {}
eval_locals = {}

def recv_string():
    header = sys.stdin.readline()
    if len(header) == 0:
        return None, ""
    cmd_type = header[0]  # First character specifies type of command
    cmd_length = int(header[1:]) # Remainder is the length
    cmd_string = sys.stdin.read(cmd_length)
    return cmd_type, cmd_string

def recv_value():
    cmd_type, cmd_string = recv_string()
    return cmd_type, eval(cmd_string, eval_globals, eval_locals)

def send_value(value):
    """
    Send a value to stdout as a string, with length of string first
    """
    value_str = lispify(value)
    print(len(value_str))
    write_stream.write(value_str)
    write_stream.flush()

def return_value(value):
    """
    Send a value to stdout
    """
    # Mark response as a returned value
    try:
        sys.stdout = write_stream
        write_stream.write("r")
        send_value(value)
    finally:
        sys.stdout = redirect_stream

def return_error(err):
    """
    Send an error message
    """
    try:
        sys.stdout = write_stream
        write_stream.write("e")
        send_value(str(err))
    finally:
        sys.stdout = redirect_stream

def message_dispatch_loop():
    """
    Wait for a message, dispatch on the type of message
    """
    while True:
        try:
            # Read command
            cmd_type, cmd_string = recv_string()
        
            if cmd_type == "e":  # Evaluate an expression
                result = eval(cmd_string, eval_globals, eval_locals)
                return_value(result)
        
            elif cmd_type == "x": # Execute a statement
                exec(cmd_string, eval_globals, eval_locals)
                return_value(None)
            
            elif cmd_type == "q": # Quit
                sys.exit(0)
                
            elif cmd_type == "r": # Return value from Lisp function
                return eval(cmd_string, eval_globals, eval_locals)

            else:
                return_error("Unknown message type '{0}', content: {1}".format(cmd_type, cmd_string))
            
        except Exception as e:
            return_error(e)

        
def callback_func(ident, *args, **kwargs):
    """
    Call back to Lisp

    ident  Uniquely identifies the function to call
    args   Arguments to be passed to the function
    """

    # Convert kwargs into a sequence of ":keyword value" pairs
    # appended to the positional arguments
    allargs = args
    for key, value in kwargs.items():
        allargs += (Symbol(":"+str(key)), value)
    
    try:
        sys.stdout = write_stream
        write_stream.write("c")
        send_value((ident, allargs))
    finally:
        sys.stdout = redirect_stream

    # Wait for a value to be returned.
    # Note that the lisp function may call python before returning
    return message_dispatch_loop()

# Make callback function accessible to evaluation
eval_globals["_py4cl_callback"] = callback_func

# Main loop
message_dispatch_loop()



