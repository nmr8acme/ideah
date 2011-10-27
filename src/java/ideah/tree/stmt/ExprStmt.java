package ideah.tree.stmt;

import ideah.tree.expr.Expression;
import ideah.util.IRange;

import java.util.Arrays;

public final class ExprStmt extends Statement {

    public final Expression expression;

    public ExprStmt(IRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }

    protected Iterable<Expression> getChildren() {
        return Arrays.asList(expression);
    }
}
