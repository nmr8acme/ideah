package ideah.tree;

import ideah.tree.expr.Expression;

public final class GRHS extends Located {

    // todo: statements? what statements?
    public final Expression expression;

    public GRHS(Expression expression) {
        this.expression = expression;
    }
}
