package ideah.tree.expr;

import ideah.tree.LocalBinds;
import ideah.util.LineColRange;

public final class LetExpr extends Expression {

    public final LocalBinds localBinds;
    public final Expression expression;

    public LetExpr(LineColRange location, LocalBinds localBinds, Expression expression) {
        super(location);
        this.localBinds = localBinds;
        this.expression = expression;
    }
}
