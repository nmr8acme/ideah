package ideah.tree.type;

import ideah.util.LineColRange;

import java.util.Arrays;

public final class BangType extends Type {

    public final Type type;

    public BangType(LineColRange location, Type type) {
        super(location);
        this.type = type;
    }

    protected Iterable<Type> getChildren() {
        return Arrays.asList(type);
    }
}
