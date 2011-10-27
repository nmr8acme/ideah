package ideah.tree;

import ideah.tree.expr.Expression;
import ideah.util.LineColRange;

import java.util.Arrays;

public final class GRHS extends Located {

    // todo: statements? what statements?
    public final Expression expression;

    public GRHS(LineColRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }

    protected Iterable<? extends Located> getChildren() {
        return Arrays.asList(expression); // todo
    }
}
