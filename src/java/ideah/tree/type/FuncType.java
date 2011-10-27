package ideah.tree.type;

import ideah.util.IRange;

import java.util.Arrays;

public final class FuncType extends Type {

    public final Type arg;
    public final Type result;

    public FuncType(IRange location, Type arg, Type result) {
        super(location);
        this.arg = arg;
        this.result = result;
    }

    protected Iterable<Type> getChildren() {
        return Arrays.asList(arg, result);
    }
}
