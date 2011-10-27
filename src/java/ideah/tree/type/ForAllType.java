package ideah.tree.type;

import ideah.util.IRange;

import java.util.Arrays;

public final class ForAllType extends Type {

    public final Type type;

    public ForAllType(IRange location, Type type) {
        super(location);
        this.type = type;
    }

    protected Iterable<Type> getChildren() {
        return Arrays.asList(type);
    }
}
