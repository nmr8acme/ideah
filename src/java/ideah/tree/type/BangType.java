package ideah.tree.type;

import ideah.util.IRange;

import java.util.Arrays;

public final class BangType extends Type {

    public final Type type;

    public BangType(IRange location, Type type) {
        super(location);
        this.type = type;
    }

    protected Iterable<Type> getChildren() {
        return Arrays.asList(type);
    }
}
