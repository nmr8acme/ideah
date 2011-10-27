package ideah.tree.expr;

import ideah.util.IRange;

import java.util.Arrays;

public final class IfExpr extends Expression {

    public final Expression condition;
    public final Expression thenExpr;
    public final Expression elseExpr;

    public IfExpr(IRange location, Expression condition, Expression thenExpr, Expression elseExpr) {
        super(location);
        this.condition = condition;
        this.thenExpr = thenExpr;
        this.elseExpr = elseExpr;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(condition, thenExpr, elseExpr);
    }
}
