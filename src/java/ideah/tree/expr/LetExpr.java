package ideah.tree.expr;

import ideah.tree.LocalBinds;

public final class LetExpr extends Expression {

    public final LocalBinds localBinds;
    public final Expression expression;

    public LetExpr(LocalBinds localBinds, Expression expression) {
        this.localBinds = localBinds;
        this.expression = expression;
    }
}
