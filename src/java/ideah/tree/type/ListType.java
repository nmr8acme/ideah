package ideah.tree.type;

import ideah.util.LineColRange;

import java.util.Arrays;

public final class ListType extends Type {

    public final Type type;

    public ListType(LineColRange location, Type type) {
        super(location);
        this.type = type;
    }

    protected Iterable<Type> getChildren() {
        return Arrays.asList(type);
    }
}
