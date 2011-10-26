package ideah.tree.type;

import ideah.util.LineColRange;

public final class AppTyType extends Type {

    public final Type type;
    public final Type arg;

    public AppTyType(LineColRange location, Type type, Type arg) {
        super(location);
        this.type = type;
        this.arg = arg;
    }
}
