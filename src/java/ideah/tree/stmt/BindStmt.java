package ideah.tree.stmt;

import ideah.tree.expr.Expression;
import ideah.tree.pat.Pat;
import ideah.util.LineColRange;

public final class BindStmt extends Statement {

    public final Pat pattern;
    public final Expression expression;

    public BindStmt(LineColRange location, Pat pattern, Expression expression) {
        super(location);
        this.pattern = pattern;
        this.expression = expression;
    }
}
