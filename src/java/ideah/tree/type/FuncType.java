package ideah.tree.type;

import ideah.util.LineColRange;

public final class FuncType extends Type {

    public final Type arg;
    public final Type result;

    public FuncType(LineColRange location, Type arg, Type result) {
        super(location);
        this.arg = arg;
        this.result = result;
    }
}
