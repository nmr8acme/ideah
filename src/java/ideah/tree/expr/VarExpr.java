package ideah.tree.expr;

import ideah.tree.Located;
import ideah.util.LineColRange;

import java.util.Collections;

public final class VarExpr extends Expression {

    public VarExpr(LineColRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
