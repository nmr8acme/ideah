package ideah.tree.type;

import ideah.tree.Located;
import ideah.util.IRange;

import java.util.Collections;

public final class TyVarType extends Type {

    public TyVarType(IRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
