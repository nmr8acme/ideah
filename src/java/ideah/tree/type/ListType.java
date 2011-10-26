package ideah.tree.type;

import ideah.util.LineColRange;

public final class ListType extends Type {

    public final Type type;

    public ListType(LineColRange location, Type type) {
        super(location);
        this.type = type;
    }
}
