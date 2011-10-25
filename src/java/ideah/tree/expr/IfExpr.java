package ideah.tree.expr;

import ideah.util.LineColRange;

public final class IfExpr extends Expression {

    public final Expression condition;
    public final Expression thenExpr;
    public final Expression elseExpr;

    public IfExpr(LineColRange location, Expression condition, Expression thenExpr, Expression elseExpr) {
        super(location);
        this.condition = condition;
        this.thenExpr = thenExpr;
        this.elseExpr = elseExpr;
    }
}
