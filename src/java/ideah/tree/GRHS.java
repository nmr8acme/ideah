package ideah.tree;

import ideah.tree.expr.Expression;
import ideah.util.LineColRange;

public final class GRHS extends Located {

    // todo: statements? what statements?
    public final Expression expression;

    public GRHS(LineColRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }
}
