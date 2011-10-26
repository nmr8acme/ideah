package ideah.tree.type;

import ideah.util.LineColRange;

public final class BangType extends Type {

    public final Type type;

    public BangType(LineColRange location, Type type) {
        super(location);
        this.type = type;
    }
}
