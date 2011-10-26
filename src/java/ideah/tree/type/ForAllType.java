package ideah.tree.type;

import ideah.util.LineColRange;

public final class ForAllType extends Type {

    public final Type type;

    public ForAllType(LineColRange location, Type type) {
        super(location);
        this.type = type;
    }
}
