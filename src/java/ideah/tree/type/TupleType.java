package ideah.tree.type;

import ideah.util.IRange;

import java.util.List;

public final class TupleType extends Type {

    public final List<Type> types;

    public TupleType(IRange location, List<Type> types) {
        super(location);
        this.types = types;
    }

    protected Iterable<Type> getChildren() {
        return types;
    }
}
