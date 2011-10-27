package ideah.tree.expr;

import ideah.tree.Located;
import ideah.util.IRange;

import java.util.Collections;

public final class Literal extends Expression {

    public Literal(IRange location) {
        super(location);
    }

    protected Iterable<? extends Located> getChildren() {
        return Collections.emptyList();
    }
}
