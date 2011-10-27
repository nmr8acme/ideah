package ideah.tree.type;

import ideah.tree.Located;
import ideah.util.LineColRange;

import java.util.Collections;

public final class TyVarType extends Type {

    public TyVarType(LineColRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
