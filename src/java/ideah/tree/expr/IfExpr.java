package ideah.tree.expr;

public final class IfExpr extends Expression {

    public final Expression condition;
    public final Expression thenExpr;
    public final Expression elseExpr;

    public IfExpr(Expression condition, Expression thenExpr, Expression elseExpr) {
        this.condition = condition;
        this.thenExpr = thenExpr;
        this.elseExpr = elseExpr;
    }
}
