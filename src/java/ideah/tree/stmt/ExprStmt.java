package ideah.tree.stmt;

import ideah.tree.expr.Expression;
import ideah.util.LineColRange;

public final class ExprStmt extends Statement {

    public final Expression expression;

    public ExprStmt(LineColRange location, Expression expression) {
        super(location);
        this.expression = expression;
    }
}
