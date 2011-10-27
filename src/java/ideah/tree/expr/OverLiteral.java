package ideah.tree.expr;

import ideah.tree.Located;
import ideah.util.LineColRange;

import java.util.Collections;

public final class OverLiteral extends Expression {

    public OverLiteral(LineColRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
