package ideah.tree.expr;

import ideah.tree.IRange;
import ideah.tree.Located;

import java.util.Collections;

public final class VarExpr extends Expression {

    public VarExpr(IRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
