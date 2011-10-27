package ideah.tree.type;

import ideah.util.LineColRange;

import java.util.Arrays;

public final class FuncType extends Type {

    public final Type arg;
    public final Type result;

    public FuncType(LineColRange location, Type arg, Type result) {
        super(location);
        this.arg = arg;
        this.result = result;
    }

    protected Iterable<Type> getChildren() {
        return Arrays.asList(arg, result);
    }
}
