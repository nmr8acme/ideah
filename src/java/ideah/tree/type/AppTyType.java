package ideah.tree.type;

import ideah.util.IRange;

import java.util.Arrays;

public final class AppTyType extends Type {

    public final Type type;
    public final Type arg;

    public AppTyType(IRange location, Type type, Type arg) {
        super(location);
        this.type = type;
        this.arg = arg;
    }

    protected Iterable<Type> getChildren() {
        return Arrays.asList(type, arg);
    }
}
